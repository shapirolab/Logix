-mode(trust).
-language([evaluate,compound,colon]).
-include(spi_constants).
-export([run/1, run/2,
	 run/3]).	/* debugging */

AMBIENT_IDLE_PRIORITY => 110.
SYSTEM_IDLE_PRIORITY => 105.

AMBIENT_ARITY => 2.
AMBIENT_CONTROL => 1.
AMBIENT_ID => 2.

/**/
DEBUG(TAG,ARG) => true.
TERMS(Terms) => true.
DEBUGT(TAG,ARG,Terms) => true.
DEBUGC(THING1,THING2) => THING1.
/**/

/*
DEBUG(TAG,ARG) => debug_out(Ambient, TAG, ARG,  Debug).
TERMS(Terms) => Terms.
DEBUGC(THING1,THING2) => THING2.
DEBUGT(TAG,ARG,Terms) => (debug_out(Ambient, TAG, ARG,  Debug), Terms).

debug_out(Ambient, Tag, Item, Debug) :-
    vector(Ambient),
    read_vector(AMBIENT_ID, Ambient, AmbientId),
    channel(Debug) :
      write_channel((AmbientId: Tag => Item), Debug);
    otherwise :
      Ambient = _,
      Debug = _,
      Item = _,
      Tag = _.

debug_remove_channels(Ambient, ChannelTuple, WaitChannelTuple, Debug) :-
    tuple(ChannelTuple),
    arity(ChannelTuple, Index),
    make_tuple(Index, OutTuple) :
      OutChannel = ready |
	debug_channel_tuple,
	debug_out(Ambient, close, FormattedChannelTuple?, Debug);

    otherwise :
      Ambient = _,
      Debug = _,
      WaitChannelTuple = ChannelTuple.

  debug_channel_tuple(OutChannel, Index, ChannelTuple, WaitChannelTuple,
			OutTuple, FormattedChannelTuple) :-
    known(OutChannel),
    arg(Index, ChannelTuple, Channel),
    arg(Index, OutTuple, OutChannel'),
    Index-- |
	spi_debug # format_channel(CHAR_d, Channel, OutChannel'),
	self;

    known(OutChannel),
    otherwise :
      Index = _,
      FormattedChannelTuple = OutTuple,
      WaitChannelTuple = ChannelTuple.
*/


run(Commands) :-
	run(Commands, _System, _Out).
run(Commands, System) :-
	run(Commands, System, _Out).
run(Commands, System, Out) :-
   true :
      SharedChannels = [],
      make_vector(AMBIENT_ARITY, System, InTuple),
      InTuple = {In, _AmbientId},
      store_vector(AMBIENT_ID, system, System),
      make_channel(Debug, Out),
      Children = [Ambient?] |
	spi_monitor # scheduler(Scheduler),
	computation # events(Events),
	computation # self # service_id(SId),
	serve_system + (Ambient = System, UniqueId = 0, Status = running),
	ambient + (AmbientName = public, Parent = System).

/*
** The system ambient is not a nested computation.
** It is a normal member of its computation (usually
** a shell computation).
**/

/*
** serve_system monitors the command stream (In) and the events stream (Events)
**
** It recognises commands:
**
**    ambient_id(AmbientId^)
**    done(Child, Reply^)
**    lookup(Locus, PrivateChannel, SharedChannel?^, Addrefs)
**    remove_channels(ChannelTuple, SubAmbient)
**    new_child(Child)
**    new_id(Id^)
**    resume(Reply^)
**    state(State^)
**    suspend(Reply^)
**    tree(Format, Tree^)
**
** and debugging commands:
**
**    debug(NewChannel)
**    debug(NewStream^)
**
** It recognises events:
**
**    aborted
**
** State:
**
**   Children - list of FCP vectors to child ambients
**   SharedChannels - list of inter-ambient channels in use by children
**   Status - one of suspended,resumed
**   UniqueId - integer updated and assigned to new ambients
**
** FCP Vectors
**
**   Ambient - to {Id,In}
**   Debug - to {DebugOut} (shared by all ambients)
**   Scheduler - to spi_monitor {Schedule} (shared by all ambients)
*/

serve_system(In, Events, Children, UniqueId, Status, SharedChannels,
			Scheduler, Ambient, Debug) :-

/************************** Debugging Aids *********************************/
    In ? debug(Out),
    we(Out) :
      make_channel(Debug', Out) |
	new_debug,
	close_debug(Debug),
	self;

    In ? debug(Debug'),
    channel(Debug') |
	new_debug,
	close_debug(Debug),
	self;

/******************** Inter-ambient communication **************************/

    In ? ambient_id(AmbientId^),
    read_vector(AMBIENT_ID, Ambient, AmbientId) |
	self;

    In ? done(Child, Reply) |
	DEBUG(done/1, children),
	remove_child(Child, Children, Children'),
	remove_shared_communications(Child, SharedChannels, Scheduler, Reply),
	self;

    In ? lookup(Locus, PrivateChannel, SharedChannel?^, AddRefs),
    Locus =?= self(Kind),
    read_vector(AMBIENT_ID, Ambient, AmbientId),
    read_vector(SPI_CHANNEL_NAME, PrivateChannel, Name) |
	DEBUGT(lookup/4 + self(Kind, AddRefs) + PC + SC1s + SC2s, search,
	       (format_channel(PrivateChannel, PC),
	        format_channel_list(SharedChannels, SC1s),
	        format_channel_list(SharedChannels, SC2s)
	       )
	),
	lookup(Kind(Name), PrivateChannel, SharedChannel, AddRefs,
	       SharedChannels, SharedChannels', Scheduler, AmbientId, Debug),
	system_lookup;


    In ? Remove, Remove =?= remove_channels(ChannelTuple, SubAmbient) :
      SubAmbient = _ |			% Only used for debugging.
	DEBUG(remove_channels - ChannelTuple - SubAmbient -
			SharedChannels, scheduler),
	remove_shared_channels(ChannelTuple, SharedChannels, SharedChannels',
				Unremoved),
	diagnose_unremoved,
	self;

    In ? new_child(Child) :
      Children' = [Child | Children] |
	DEBUG(new_child, added-NewChildId),
	TERMS(read_vector(AMBIENT_ID,Child,NewChildId)),
	self;

    In ? new_id(Id),
    UniqueId++ :
      Id = UniqueId' |
	self;

    In ? state(State) :
      State = [id(system), children(Children), unique_id(UniqueId)] |
	self;

/**************************** External Signals ******************************/

    In ? suspend(Reply),
    Status =?= running :
      Reply = Status'? |
	DEBUG(suspend, suspending),
	processor # machine(idle_queue(Done, SYSTEM_IDLE_PRIORITY), _Ok),
	control_children(suspend, Children, Children', Ready, Done?),
	system_suspending(Ready?, SharedChannels, Status'),
	self;

    In ? suspend(Reply),
    Status =\= running :
      Reply = suspend - false(Status) |
	self;

    In ? resume(Reply),
    Status =?= suspended |
	DEBUG(resume, Status'),
	system_resume(Scheduler, Children, Children', Status', Reply),
	self;

    In ? resume(Reply),
    Status =\= suspended |
	Reply = resume - false(Status),
	self;

/****************************** Ambient Tree ********************************/

    In ? tree(channels, Tree) :
      Tree = channels(system, SharedChannels?, SubTrees?) |
	ambient_tree(channels, Children, Children', SubTrees),
	self;

    In ? tree(resolvent, Tree),
    Status =?= running :
      In'' = [suspend(_Reply), tree(resolvent, Tree) | In'] |
	self;

    In ? tree(resolvent, Tree),
    Status =\= running, Status =\= suspended :
      Tree = resolvent(system, ["Can't resolve"(Status)], []) |
	self;

    In ? tree(resolvent, Tree),
    Status =?= suspended :
      Tree = resolvent(system, [], SubTrees?) |
	ambient_tree(resolvent, Children, Children', SubTrees),
	self;

/**************************************************************************/

    In ? Other,
    otherwise |
	fail(ambient(system) ? Other, unknown),
	self;

    Children =?= [],
    unknown(In) :
      Events  = _,
      Scheduler = _,
      SharedChannels = _,
      Status = _,
      UniqueId = _,
      close_vector(AMBIENT_CONTROL, Ambient) |
	close_debug;

    Events ? aborted :
      Status = _,
      Status' = aborted |
	DEBUG((system: event-aborted), ambient),
	abort_children(Children, Children'),
	self;

    Events ? Other, Other =\= aborted |
	DEBUG((system: event-Other), trash),
	self.

  system_lookup(In, Events, Children, UniqueId, Status,
		SharedChannels, Scheduler, Ambient, Debug, SharedChannel) :-
    known(SharedChannel) |
	serve_system.

  new_debug(Debug, Children) :-
    Children ? Child :
      write_vector(AMBIENT_CONTROL, debug(Debug), Child) |
	self;
    Children = [] :
      Debug = _.

  close_debug(Debug) :-
    channel(Debug) :
      close_channel(Debug);
    otherwise :
      Debug = _.

  system_resume(Scheduler, Children, NewChildren, Status, Reply) :-

    true :
      write_channel(status(_), Scheduler),
      Status = Reply? |
	control_children(resume, Children, NewChildren, Ready),
	Ready?(Reply) = true(running);

    /* This is the case where the Scheduler has closed. */
    otherwise :
      Scheduler = _,
      NewChildren = Children,
      Status = suspended,
      Reply = false(done).

  system_suspending(Ready, SharedChannels, Status) :-

    known(Ready) |
	remove_all_communications(SharedChannels, Reply),
	Reply?(Status) = true(suspended).

/*
** Create a nested computation.
**/
ambient(AmbientName, SId, Commands, Parent, Ambient, Debug) :-
    vector(Parent) :
      write_vector(AMBIENT_CONTROL, new_id(UniqueId), Parent, Parent'),
      write_vector(AMBIENT_CONTROL, ambient_id(ParentId), Parent', Parent''),
      make_channel(SuperChannel, FromSub),
      make_vector(AMBIENT_ARITY, Ambient, InTuple),
      InTuple = {In, _},
      make_channel(ToDomain, DIn),
      Children = [],
      store_vector(AMBIENT_ID, AmbientId?, Ambient) |
	spi_monitor # scheduler(Scheduler),
	make_ambient_id,
	computation # "_domain"(domain_channel(Domain)),
	computation_server#computation([identifier(AmbientId?, _) | Requests],
					{Controls, Done, done, SuperChannel?},
					ToDomain?,
					Events),
	request_commands(Commands, Requests, Requests'),
	computation # events(RelayEvents),
	watcher + (Done = AmbientDone),
	serve_ambient0.

  make_ambient_id(AmbientName, UniqueId, ParentId, AmbientId, Scheduler) :-
    channel(Scheduler),
    string(AmbientName),
    integer(UniqueId) :
      AmbientId = AmbientName(UniqueId),
      write_channel(debug_note((ParentId->AmbientId)), Scheduler).


request_commands(Commands, Requests, EndRequests) :-

    Commands ? Command |
	request_commands(Command, Requests, Requests'?),
	self;

    Commands =?= [] :
      Requests = EndRequests;

    Commands =\= [_|_], Commands =\= [] :
      Requests = [Commands | EndRequests].
   

watcher(RelayEvents, DIn, Done, SId, Domain, Ambient, Debug) :-

    RelayEvents ? aborted :
      Debug = _,
      DIn = _,
      Domain = _,
      Done = _,
      RelayEvents' = _,
      SId = _,
      write_vector(AMBIENT_CONTROL, abort, Ambient) ; /* |
	DEBUG(relay-aborted, abort);
*/

    RelayEvents ? Other, Other =\= aborted |
	DEBUG(relay-Other, trash),
	self;

    Done =?= done :
      Ambient = _,
      Debug = _,
      DIn = _,
      Domain = _,
      RelayEvents = _,
      SId = _ ; /*|
	DEBUG(done, quit - watcher);
*/

    DIn ? Command |
	domain_command.

  domain_command(RelayEvents,DIn, Done, SId, Domain, Ambient,
			Debug, Command) :-

    Command =?= export(CallInfo, _Scope, Goals, UCC),
    channel(Domain),
    ground(SId) :
      write_channel(export(CallInfo, SId, Goals, UCC), Domain) |
	DEBUG(Command, domain(SId)),
	watcher;

    Command =?= reduce(CallInfo, _Scope, Goals, UCC),
    channel(Domain),
    ground(SId) :
      write_channel(reduce(CallInfo, SId, Goals, UCC), Domain) |
	DEBUG(Command, domain(SId)),
	watcher.
    

serve_ambient0(In, Events, FromSub, Done,
	       Ambient, Parent,
	       Children,
	       Requests, Controls, AmbientDone, Scheduler, Debug) :-
	serve_ambient + (PublicChannels = [{0, _, -1, ""}, {[], _, -1, ""}],
			 PrivateChannels = [], SharedChannels = []).

/*
** serve_ambient monitors the command stream (In), the events stream (Events)
** and the delegated request stream (FromSub).
**
** It recognises commands:
**
**    ambient_id(AmbientId^)
**    done(Child, Reply^)
**    lookup(Locus, PrivateChannel, SharedChannel?^, Addrefs)
**    remove_channels(ChannelTuple, SubAmbient)
**    new_child(Child)
**    new_id(Id^)
**    resume(Reply^)
**    state(State^)
**    suspend(Reply^)
**    tree(Format, Tree^)
**
** and debugging commands:
**
**    debug(NewChannel)
**    debug(NewStream^)
**
** It recognises events:
**
**    aborted
**
** State:
**
**   Children - list of FCP vectors to child ambients
**   SharedChannels - list of inter-ambient channels in use by children
**   Status - one of suspended,resumed
**   UniqueId - integer updated and assigned to new ambients
**
** FCP Vectors
**
**   Ambient - to {Id,In}
**   Debug - to {DebugOut} (shared by all ambients)
**   Scheduler - to spi_monitor {Schedule} (shared by all ambients)
*/

serve_ambient(In, Events, FromSub, Done,
	      Ambient, Parent, Children,
	      PrivateChannels, PublicChannels, SharedChannels,
	      Requests, Controls, AmbientDone,
	      Scheduler, Debug) :-

/* Internal Communication */

    In ? abort :
      Controls' = _ |
	DEBUG(abort, children),
	unify_without_failure(Controls, [abort]),
	unify_without_failure(Done, done),
	abort_children(Children, Children'),
	self;

    In ? ambient_id(AmbientId^),
    read_vector(AMBIENT_ID, Ambient, AmbientId) |
	self;

    In ? done(Child, Reply) |
	DEBUG(done/1, children),
	remove_child(Child, Children, Children'),
	remove_shared_communications(Child, SharedChannels, Scheduler, Reply),
	self;

    In ? NewId, NewId =?= new_id(_Id) :
      write_vector(AMBIENT_CONTROL, NewId, Parent, Parent') |
	DEBUG(NewId, pass),
	self;

/* Procedure Services */

    In ? Close, Close =?= close(ChannelTuple) :
      write_channel(close(DEBUGC(ChannelTuple, WaitChannelTuple?), Reply),
		    Scheduler) |
	DEBUGC(true,debug_remove_channels),
	DEBUG(close - Reply - ChannelTuple - PrivateChannels - SharedChannels,
		scheduler),
	remove_local_channels(Reply, ChannelTuple,
			      PrivateChannels, PrivateChannels',
			      SharedChannels, SharedChannels', Unremoved),
	pass_unremoved;

/* Obsolescent global channel declarations */
    In ? global_channels(List) |
	DEBUG(global/1, scheduler),
	merge_public_channels(List, PublicChannels, PublicChannels',
			      Scheduler),
	self;

    In ? global_channels(List, Ambient^) |
	DEBUG(global/2, scheduler),
	merge_public_channels(List, PublicChannels, PublicChannels',
			      Scheduler),
	self;

    In ? public_channels(List) |
	DEBUG(public/1, scheduler),
	merge_public_channels(List, PublicChannels, PublicChannels',
			      Scheduler),
	self;

    In ? public_channels(List, Ambient^) |
	DEBUG(public/2, scheduler),
	merge_public_channels(List, PublicChannels, PublicChannels',
			      Scheduler),
	self;

    In ? lookup(Locus, PrivateChannel, SharedChannel?^),
    Locus =?= private,
    read_vector(SPI_CHANNEL_NAME, PrivateChannel, Name) |
	DEBUGT(lookup/3 + private + PC + SC1s + SC2s, search,
	       (format_channel(PrivateChannel, PC),
	        format_channel_list(PrivateChannels, SC1s),
	        format_channel_list(PrivateChannels', SC2s)
	       )
        ),
	lookup(Name, PrivateChannel, SharedChannel, 1,
	       PrivateChannels, PrivateChannels', Scheduler, Ambient, Debug),
	ambient_lookup;

    In ? lookup(Locus, PrivateChannel, SharedChannel?^, AddRefs),
    Locus =?= private,
    read_vector(SPI_CHANNEL_NAME, PrivateChannel, Name) |
	DEBUGT(lookup/4 + private(AddRefs) + PC + SC1s + SC2s, search,
	       (format_channel(PrivateChannel, PC),
	        format_channel_list(PrivateChannels, SC1s),
	        format_channel_list(PrivateChannels', SC2s)
	      )
	),
	lookup(Name, PrivateChannel, SharedChannel, AddRefs,
	       PrivateChannels, PrivateChannels', Scheduler, Ambient, Debug),
	ambient_lookup;

    In ? lookup(Locus, PrivateChannel, SharedChannel?^),
    Locus =?= public,
    read_vector(SPI_CHANNEL_NAME, PrivateChannel, Id),
    string(Id),
    string_length(Id) > 7,
    string_to_dlist(Id, IdL, []),
    string_to_dlist("public.", Prefix, NL) :
      IdL = Prefix |
	DEBUGT(lookup/3 + public + PC + SC1s + SC2s, search ,
	       (format_channel(PrivateChannel, PC),
	        copy_public_channels(PublicChannels, GCsBefore),
	        format_channel_list(GCsBefore, SC1s),
	        copy_public_channels(PublicChannels', GCsAfter),
	        format_channel_list(GCsAfter, SC2s)
	       )
	),
	list_to_string(NL, Name),
	lookup(Name, PrivateChannel, SharedChannel, 1,
	       PublicChannels, PublicChannels', Scheduler, Ambient, Debug),
	ambient_lookup;

    In ? lookup(Locus, PrivateChannel, SharedChannel?^),
    Locus =?= self(Kind),
    read_vector(SPI_CHANNEL_NAME, PrivateChannel, Name) |
	DEBUGT(lookup/3 + Locus + PC + SC1s + SC2s, search,
	       (format_channel(PrivateChannel, PC),
	        format_channel_list(SharedChannels, SC1s),
	        format_channel_list(SharedChannels', SC2s)
	       )
	),
	lookup(Kind(Name), PrivateChannel, SharedChannel, 1,
	       SharedChannels, SharedChannels', Scheduler, Ambient, Debug),
	ambient_lookup;

    In ? lookup(Locus, PrivateChannel, SharedChannel?^),
    Locus =?= parent(Kind) :
      write_vector(AMBIENT_CONTROL,
		   lookup(self(Kind), PrivateChannel, SharedChannel, 1),
		   Parent, Parent') |
	DEBUG(lookup/3 - Locus - PrivateChannel, pass),
	ambient_lookup;

    /***** Obsolescent
    In ? new_ambient(Name, ModuleId, Goal, NewAmbient?^),
    ModuleId = [ModuleName | SId] :
      Children' = [NewAmbient'? | Children] |
	DEBUG(new_ambient, ambient - AId?),
	TERMS(read_vector(AMBIENT_ID, NewAmbient, AId)),
	processor # machine(idle_queue(Idle, AMBIENT_IDLE_PRIORITY), _Ok),
	merge_local_channels(Idle, Goal, NewGoal, copy,	Ambient, NewAmbient,
				Lookups, []),
	copy_initial_commands(Lookups, NewAmbient, NewAmbient'),
	ambient(Name, SId, ModuleName # NewGoal?,
		Ambient, NewAmbient, Debug),
	self;
     *****/

    In ? new_ambient(Name, ModuleId, Goal),
    ModuleId = [ModuleName | SId] :
      Children' = [NewAmbient'? | Children] |
	DEBUG(new_ambient, ambient),
	processor # machine(idle_queue(Idle, AMBIENT_IDLE_PRIORITY), _Ok),
	merge_local_channels(Idle, Goal, NewGoal, copy,	Ambient, NewAmbient,
				Lookups, []),
	copy_initial_commands(Lookups, NewAmbient, NewAmbient'),
	ambient(Name, SId, ModuleName # NewGoal?,
		Ambient, NewAmbient, Debug),
	self;

    In ? New, New = new_channel(_Creator, Channel, _BaseRate) :
      write_channel(New, Scheduler) |
	DEBUG(new_channel/3, scheduler),
	add_local_channel(Channel, PrivateChannels, PrivateChannels'),
	self;

    In ? New, New = new_channel(_Creator, Channel, _ComputeWeight,
					_BaseRate) :
      write_channel(New, Scheduler) |
	DEBUG(new_channel/4, scheduler),
	add_local_channel(Channel, PrivateChannels, PrivateChannels'),
	self;

    In ? new_locals(Locals, NewLocals) |
	DEBUGT(new_locals(Ls), NLs,
	       (format_channel_tuple(Locals, Ls),
	        format_channel_tuple(NewLocals, NLs)
              )
	),
	processor # machine(idle_queue(Idle, AMBIENT_IDLE_PRIORITY), _Ok),
	copy_local_channels(Idle, Locals, NewLocals, Ambient, In'', In'),
	self;

    In ? Start, Start =?= start(Signature, Operations, Message, Chosen),
    read_vector(AMBIENT_ID, Ambient, AmbientId),
    AmbientId =?= AmbientName(UniqueId),
    convert_to_string(UniqueId, UIS),
    string_to_dlist(UIS, UIL, [CHAR_RIGHT_PAREN]),
    string_to_dlist(AmbientName, ANL, [CHAR_LEFT_PAREN | UIL]),
    list_to_string(ANL, AID) :
      Start' = start(Signature, Operations, Message, Chosen, AID),
      write_channel(Start', Scheduler) |
	DEBUGT(Start, Ch-scheduler,
	       (Operations? = [Op | _],arg(SPI_MS_CHANNEL,Op,SCh),
	        format_channel(SCh,Ch)
	       )
	),
	self;

/******************** Inter-ambient communication **************************/

    In ? Remove, Remove =?= remove_channels(ChannelTuple, SubAmbient) :
      SubAmbient = _ |			% Only used for debugging.
	DEBUGT(remove_channels + CT + SubAmbient + SC1s + SC2s, scheduler,
	       (format_channel_tuple(ChannelTuple, CT),
	        format_channel_list(SharedChannels, SC1s),
	        format_channel_list(SharedChannels', SC2s)
	       )
	),
	remove_shared_channels(ChannelTuple,
			SharedChannels, SharedChannels', Unremoved),
	diagnose_unremoved,
	self;

    In ? lookup(Locus, PrivateChannel, SharedChannel?^, AddRefs),
    Locus =?= public,
    AddRefs--,
    read_vector(SPI_CHANNEL_NAME, PrivateChannel, Id),
    string(Id),
    string_length(Id) > 7,
    string_to_dlist(Id, IdL, []),
    string_to_dlist("public.", Prefix, NL) :
      IdL = Prefix |
	DEBUG(lookup/4 - public(AddRefs') - PrivateChannel -
		PublicChannels - PublicChannels', search),
	list_to_string(NL, Name),
	lookup(Name, PrivateChannel, SharedChannel, AddRefs',
	       PublicChannels, PublicChannels', Scheduler, Ambient, Debug),
	ambient_lookup;

    In ? lookup(Locus, PrivateChannel, SharedChannel?^, AddRefs),
    Locus =?= self(Kind),
    read_vector(SPI_CHANNEL_NAME, PrivateChannel, Name) |
	DEBUG(lookup/4 - self(Kind, AddRefs) - PrivateChannel -
		SharedChannels - SharedChannels', search),
	lookup(Kind(Name), PrivateChannel, SharedChannel, AddRefs,
	       SharedChannels, SharedChannels', Scheduler, Ambient, Debug),
	ambient_lookup;

    In ? lookup(Locus, PrivateChannel, SharedChannel?^, AddRefs),
    Locus =?= parent(Kind) :
      write_vector(AMBIENT_CONTROL,
		   lookup(self(Kind), PrivateChannel, SharedChannel, AddRefs),
		   Parent, Parent') |
	DEBUG(lookup/4 - parent(Kind, AddRefs) - PrivateChannel, pass),
	ambient_lookup;

    In ? new_child(Child) :
      Children' = [Child | Children] |
	DEBUG(new_child, added-NewChildId),
	TERMS(read_vector(AMBIENT_ID,Child,NewChildId)),
	self;

/* Capability Initialization */

    In ? delegate(Ambient^, Ready) :
      Ready = _ |
	DEBUG(delegate, Ready),
	self;

    In ? withdraw(Child, Removed) |
	DEBUG(withdraw(Removed), ChildId),
        TERMS(read_vector(AMBIENT_ID, Child, ChildId)),
	remove_shared_communications(Child, SharedChannels, Scheduler,
					Removed),
	self;

    In ? enter(Enterer, Ready) :
      write_vector(AMBIENT_CONTROL,
		   withdraw(Enterer, Removed),
		   Parent),
      write_vector(AMBIENT_CONTROL,
		   change_parent(Ambient, Removed?, Ready),
		   Enterer) |
	DEBUG("enter"(Removed, Ready), move - EntererId - into - MyId),
        TERMS((read_vector(AMBIENT_ID, Enterer, EntererId),
	       read_vector(AMBIENT_ID, Ambient, MyId))),
	self;

    In ? exit(Exiter, Ready) :
      write_vector(AMBIENT_CONTROL,
		   change_parent(Parent, Removed, Ready),
		   Exiter) |
	DEBUG("exit"(Removed, Ready), move - ExiterId - into - ParentId),
        TERMS((read_vector(AMBIENT_ID, Exiter, ExiterId),
	       read_vector(AMBIENT_ID, Parent, ParentId))),
	remove_shared_communications(Exiter, SharedChannels, Scheduler,
					Removed),
	self;

    In ? merge(MergingAmbient, Ready),
    vector(MergingAmbient),
    read_vector(AMBIENT_ID, MergingAmbient, FromId) :
      write_channel(record_item(reset(FromId)), Scheduler),
      write_vector(AMBIENT_CONTROL,
		   extract(Goals, Ambient, Ready),
		   MergingAmbient, MergingAmbient') |
	DEBUG(merge/2, merge(Goals)),
	processor # machine(idle_queue(Idle, AMBIENT_IDLE_PRIORITY), _Ok),
	merge_local_channels(Idle, Goals, MergedGoals, pass,
			     MergingAmbient', Ambient, In'', In'),
	add_merged_goals(MergedGoals?, Requests, Requests', Ready),
	self;

/* Capability Services */

    In ? change_parent(Parent', false, Ready) :
      write_vector(AMBIENT_CONTROL, done(Ambient, Ready), Parent, _Parent),
      write_vector(AMBIENT_CONTROL, new_child(Ambient), Parent', Parent''),
      Ready = true |
        TERMS(read_vector(AMBIENT_ID,Parent'',NewParentId)),
	DEBUG(change_parent, NewParentId-"no_remove"),
	self;

    In ? change_parent(Parent', true, Ready),
    read_vector(AMBIENT_ID, Ambient, AmbientId) :
      write_channel(record_item(reset(AmbientId)), Scheduler),
      write_vector(AMBIENT_CONTROL, done(Ambient, Ready), Parent, _Parent),
      write_vector(AMBIENT_CONTROL, new_child(Ambient), Parent', Parent''),
      Controls ! suspend |
	DEBUG(change_parent, NewParentId-"remove p2c & all local communications"),
        TERMS(read_vector(AMBIENT_ID,Parent'',NewParentId)),
	remove_shared_communications(Ambient, SharedChannels, Scheduler,
					Reply1),
	copy_public_channels,
	remove_all_communications(Channels?, Reply2),
	remove_all_communications(PrivateChannels, Reply3),
	resume_controls_when_ready(Reply1, Reply2, Reply3, Ready,
					Controls', Controls''),
	self;

    In ? extract(Goals, MergedAmbient, Ready) :
      Controls ! suspend |
	DEBUG(suspend/2, suspend-extract-send-resume),
	children_to_merged_ambient;

/* Control Services */

    In ? suspend(Ready) :
      Controls ! suspend |
	control_children(suspend, Children, Children', ReadyChildren),
	ambient_suspending,
	self;

    In ? resume(Ready) :
      Controls ! resume |
	control_children(resume, Children, Children', Ready),
	self;

    In ? tree(channels, AllChannels, SubTrees) |
	copy_public_channels,
	concatenate_lists([SharedChannels, Channels, PrivateChannels],
			  AllChannels),
	ambient_tree(channels, Children, Children', SubTrees),
	self;

    In ? tree(resolvent, Resolvent, SubTrees) :
      Controls ! request(state(Resolvent)) |
	ambient_tree(resolvent, Children, Children', SubTrees),
	self;

/************************** Debugging Aids *********************************/

   In ? ambient_channel(Ambient?^) |
	self;

    In ? debug(Out),
    we(Out) :
      make_channel(Debug', Out) |
	new_debug,
	close_debug(Debug),
	self;

    In ? debug(Debug'),
    channel(Debug') |
	new_debug,
	close_debug(Debug),
	self;

    In ? state(State),
    read_vector(AMBIENT_ID, Ambient, AmbientId) :
      State = [id(AmbientId), self(Ambient), parent(Parent),
		children(Children), private(PrivateChannels),
		public(Channels?), shared(SharedChannels),
		debug(Debug)] |
	copy_public_channels,
	self;
/***************************************************************************/

    In ? Other,
    otherwise,
    read_vector(AMBIENT_ID, Ambient, AmbientId) |
	DEBUG((other = Other), fail),
	/* other(Other) = In'?, */
	fail(ambient(AmbientId) ? Other, unknown),
	self;

    /* Obsolescent global_channels */
    Events ? Public,
    Public =?= event(global_channels(List)) |
	DEBUG(delegated-Public, scheduler),
	merge_public_channels(List, PublicChannels, PublicChannels',
			      Scheduler),
	self;

    Events ? Public,
    Public =?= event(global_channels(List, AmbientChannel)) |
	DEBUG(delegated-Public, scheduler),
	unify_without_failure(AmbientChannel, Ambient),
	merge_public_channels(List, PublicChannels, PublicChannels',
			      Scheduler),
	self;

    Events ? Public,
    Public =?= event(public_channels(List)) |
	DEBUG(delegated-Public, scheduler),
	merge_public_channels(List, PublicChannels, PublicChannels',
			      Scheduler),
	self;

    Events ? Public,
    Public =?= event(public_channels(List, AmbientChannel)) |
	DEBUG(delegated-Public, scheduler),
	unify_without_failure(AmbientChannel, Ambient),
	merge_public_channels(List, PublicChannels, PublicChannels',
			      Scheduler),
	self;

    Events ? Event,
    Event =\= event(public_channels(_)),
    Event =\= event(public_channels(_, _)),
    /* Obsolescent global_channels */
    Event =\= event(global_channels(_)),
    Event =\= event(global_channels(_, _)) |
	serve_event;

    FromSub ? delegated([], CCC),
    CCC = {_, Left, Right, _} :
      Left = Right |
	DEBUG(delegated-[], trash),
	self;

    FromSub ? delegated(Message, CCC),
    Message =\= [],
    CCC = {_, Left, Right, _} :
      Left = Right |
	DEBUG((delegated-message = Message), computation),
	computation # Message,
	self;

    FromSub ? request(From, Event, Latch, Latch^) :
      Event = _,
      From = _ |
	DEBUG((request-from(From) = Event), trash),
	self;

    Done =?= done, Children =?= [] :
      Debug = _,
      Events = _,
      FromSub = _,
      In = _,
      Scheduler = _,
      Controls = [],
      Requests = [],
      AmbientDone = done |
	DEBUG(closed, detach(PrivateChannels, Channels, SharedChannels) - quit),
	detach_channel_list(SharedChannels, Scheduler),
	detach_channel_list(PrivateChannels, Scheduler),
	copy_public_channels,
	detach_channel_list(Channels, Scheduler),
	ambient_done.

  ambient_done(Ambient, Parent) :-

    true :
      write_vector(AMBIENT_CONTROL, done(Ambient, _Reply), Parent);

    otherwise :
      Parent = _,
      close_vector(AMBIENT_CONTROL, Ambient).


  add_merged_goals(Goals, Requests, NewRequests, Ready) :-

    Goals ? Goal :
      Requests ! Goal |
	self;

    Goals =?= [] :
      NewRequests = Requests,
      Ready = true.


  ambient_lookup(In, Events, FromSub, Done,
		 Ambient, Parent, Children,
		 PrivateChannels, PublicChannels, SharedChannels,
		 Requests, Controls, AmbientDone,
		 Scheduler, Debug,
		 SharedChannel) :-
    known(SharedChannel) |
	serve_ambient.

  copy_resolvent(R, Resolvent, NextResolvent) :-

    R ? Term :
      Resolvent ! Term |
	self;

    R =?= [] :
     Resolvent = NextResolvent.

  copy_initial_commands(Commands, Ambient, NewAmbient) :-

    Commands ? Command,
    vector(Ambient) :
      write_vector(AMBIENT_CONTROL, Command, Ambient, Ambient') |
 	self;

    Commands =?= [] :
      NewAmbient = Ambient.

  diagnose_unremoved(Unremoved, Ambient, SubAmbient, Debug) :-

    Unremoved ? Channel,
    read_vector(SPI_CHANNEL_NAME, Channel, Name),
    read_vector(AMBIENT_ID, Ambient, Id),
    read_vector(AMBIENT_ID, SubAmbient, SubId) |
	DEBUG(unremoved, Unremoved - Ambient - SubAmbient),
	screen#display(Id - "can't remove channel" - Name - "for" - SubId),
	self;

    Unremoved =?= [] :
      Ambient = _,
      Debug = _,
      SubAmbient = _.

  pass_unremoved(In, Events, FromSub, Done,
	      Ambient, Parent,
	      Children, PrivateChannels, PublicChannels, SharedChannels,
	      Requests, Controls, AmbientDone,
	      Scheduler, Debug,
	      Unremoved) :-

    Unremoved =?= [] |
	serve_ambient;

    Unremoved =?= [Channel] :
      write_vector(AMBIENT_CONTROL,
		   remove_channels({Channel}, Ambient),
		   Parent, Parent') |
	serve_ambient;

    Unremoved =?= [Channel1, Channel2] :
      write_vector(AMBIENT_CONTROL,
		   remove_channels({Channel1, Channel2}, Ambient),
		   Parent,Parent') |
	serve_ambient;

    Unremoved =\= [], Unremoved =\= [_], Unremoved =\= [_, _] :
      write_vector(AMBIENT_CONTROL,
		   remove_channels(ChannelTuple?, Ambient),
		   Parent, Parent') |
	utils#list_to_tuple(Unremoved, ChannelTuple),
	serve_ambient.

  remove_child(Ambient, Children, NewChildren) :-

    Children ? Ambient :
      NewChildren = Children';

    Children ? Child,
    Child =\= Ambient :
      NewChildren ! Child |
	self;

    Children =?= [] :
      Ambient = _,
      NewChildren = [].

  abort_children(Children, Reply) :-
    Children ? Child :
      write_vector(AMBIENT_CONTROL, abort, Child) |
	self;
    Children ? _Child,
    otherwise |
	self;
    Children =?= [] |
      Reply = [].

  serve_event(In, Events, FromSub, Done,
	      Ambient, Parent, Children,
	      PublicChannels, PrivateChannels, SharedChannels,
	      Requests, Controls, AmbientDone,
	      Scheduler, Debug,
	      Event) :-

    Event =?= terminated |
	DEBUG(event-terminated, done),
	unify_without_failure(Done, done),
	serve_ambient;

    Event =?= aborted :
      Controls = [],
      Requests = [],
      close_vector(AMBIENT_CONTROL, Ambient) | 
	DEBUG(event-aborted, done),
	unify_without_failure(Done, done),
	abort_children(Children, Children'),
	serve_ambient;
	
    Event = failed(Goal, Reason),
    read_vector(AMBIENT_ID, Ambient, AmbientId) |
	computation # failed(AmbientId@Goal, Reason),
	serve_ambient;
		
    Event =?= comment(Comment),
    read_vector(AMBIENT_ID, Ambient, AmbientId) |
	computation # comment(AmbientId@Comment),
	serve_ambient;

    Event =?= diagnostic(Diagnostic),
    read_vector(AMBIENT_ID, Ambient, AmbientId) |
	computation # diagnostic(AmbientId@Diagnostic),
	serve_ambient;

    otherwise :
      Event = _ |
	DEBUG(event-Event, trash),
	serve_ambient.

  children_to_merged_ambient(In, Events, FromSub, Done,
			     Ambient, Parent, Children,
			     PublicChannels, PrivateChannels, SharedChannels,
			     Requests, Controls, AmbientDone,
			     Scheduler, Debug,
			     MergedAmbient, Goals, Ready) :-

    Children ? Child :
      write_vector(AMBIENT_CONTROL,
		   change_parent(MergedAmbient, Removed, _Ready),
		   Child) |
	DEBUG("merge"(Removed, Ready), move - ChildId - into - NewParentId),
        TERMS((read_vector(AMBIENT_ID, Child, ChildId),
	       read_vector(AMBIENT_ID, MergedAmbient, NewParentId))),
	remove_shared_communications(Child, SharedChannels, Scheduler,
					Removed),
	self;

    Children =?= [] :
      MergedAmbient = _,
      SharedChannels = _,
      Controls ! request(extract(all, Goals)) |
	resume_ambient_when_ready.


lookup(Id, PrivateChannel, SharedChannel, AddRefs, ChannelList, NewChannelList,
	 Scheduler, Ambient, Debug) + (Last = "") :-

    ChannelList =?= [Channel | _],
    vector(Channel),
    read_vector(SPI_CHANNEL_REFS, Channel, Refs),
    Refs > 0,
    Refs += AddRefs,
    read_vector(SPI_CHANNEL_NAME, Channel, ChannelId),
    ChannelId =?= Id :
      Ambient = _,
      Debug = _,
      Last = _,
      Scheduler = _,
      store_vector(SPI_CHANNEL_REFS, Refs', Channel) |
	DEBUG(lookup, found(Refs-Refs') - Id - Channel),
	verify_shared_channel;

    ChannelList ? Channel,
    vector(Channel),
    read_vector(SPI_CHANNEL_REFS, Channel, Refs),
    Refs =< 0 :
      NewChannelList ! Channel |
	DEBUG(lookup, skip_deleted(Refs) - Id - Channel),
	self;

    ChannelList ? Channel,
    vector(Channel),
    read_vector(SPI_CHANNEL_NAME, Channel, OtherId),
    OtherId =\= Id :
      NewChannelList ! Channel |
	DEBUG(lookup, mismatch - Id =\= OtherId),
	self;

    ChannelList = [Public | _],
    Public =?= Id(Channel, _ComputeWeight, _BaseRate),
    vector(Channel),
    read_vector(SPI_CHANNEL_REFS, Channel, Refs),
    Refs += AddRefs :
      Ambient = _,
      Debug = _,
      Last = _,
      Scheduler = _,
      store_vector(SPI_CHANNEL_REFS, Refs', Channel) |
	DEBUG(lookup, found(Refs - Refs') - Id - Channel),
	verify_shared_channel;

    ChannelList ? Entry,
    Entry = Last'(_, _, _),
    Last' @< Id :
      Last = _,
      NewChannelList ! Entry |
	self;

    ChannelList =?= [Name1(_, _, _) | _],
    Last @< Id, Id @< Name1,
    string_to_dlist("public.", GL, GT),
    string_to_dlist(Id, NL, []),
    AddRefs++,
    read_vector(SPI_CHANNEL_TYPE, PrivateChannel, Type),
    read_vector(SPI_CHANNEL_RATE, PrivateChannel, Rate),
    read_vector(SPI_WEIGHT_TUPLE, PrivateChannel, WeightTuple) :
      Ambient = _,
      Debug = _,
      NewChannelList =
	[Id(SharedChannel?, SPI_DEFAULT_WEIGHT_NAME, BaseRate?) | ChannelList],
      write_channel(new_channel(PublicId, NewChannel, BaseRate?), Scheduler),
      GT = NL |
	list_to_string(GL, PublicId),
	DEBUG(lookup, new - PublicId - PrivateChannel(Type,Rate)),
	rate_to_baserate,
	update_new_channel;

    ChannelList =?= [],
    read_vector(SPI_CHANNEL_TYPE, PrivateChannel, Type),
    read_vector(SPI_CHANNEL_RATE, PrivateChannel, Rate),
    read_vector(SPI_WEIGHT_TUPLE, PrivateChannel, WeightTuple) :
      Ambient = _,
      Debug = _,
      Last = _,
      NewChannelList = [SharedChannel?],
      write_channel(new_channel(Id, NewChannel, BaseRate?), Scheduler) |
	DEBUGT(lookup, new - Id - NC,
	       format_channel(NewChannel, NC)
	),
	rate_to_baserate,
	update_new_channel.

  rate_to_baserate(Type, Rate, BaseRate) :-

    Type =?= SPI_INSTANTANEOUS :
      Rate = _,
      BaseRate = infinite;

    Type =\= SPI_INSTANTANEOUS :
      BaseRate = Rate.

  update_new_channel(NewChannel, AddRefs, Type, Rate, WeightTuple,
			SharedChannel) :-

    vector(NewChannel) :
      store_vector(SPI_CHANNEL_TYPE, Type, NewChannel),
      store_vector(SPI_CHANNEL_RATE, Rate, NewChannel),
      store_vector(SPI_WEIGHT_TUPLE, WeightTuple, NewChannel),
      store_vector(SPI_CHANNEL_REFS, AddRefs, NewChannel),
      SharedChannel = NewChannel.

  verify_shared_channel(PrivateChannel, Channel, SharedChannel,
			ChannelList, NewChannelList, Scheduler) :-

    read_vector(SPI_CHANNEL_RATE, PrivateChannel, Rate),
    read_vector(SPI_WEIGHT_TUPLE, PrivateChannel, WeightTuple),
    read_vector(SPI_CHANNEL_RATE, Channel, SharedRate),
    read_vector(SPI_WEIGHT_TUPLE, Channel, SharedWeightTuple),
    Rate =?= SharedRate,
    WeightTuple =?= SharedWeightTuple |
	verify_shared_channel_type;

    otherwise,
    read_vector(SPI_CHANNEL_NAME, PrivateChannel, Id) :
      NewChannelList = [Channel | ChannelList],
      SharedChannel = Channel |
	ambient_diagnostic("shared weight/rate conflict!" - Id, Scheduler).

  verify_shared_channel_type(PrivateChannel, Channel, SharedChannel,
			     ChannelList, NewChannelList, Scheduler) :-

    read_vector(SPI_CHANNEL_TYPE, PrivateChannel, Type),
    read_vector(SPI_CHANNEL_TYPE, Channel, SharedType),
    Type =?= SharedType :
      Scheduler = _,
      SharedChannel = Channel,
      NewChannelList = ChannelList;

    read_vector(SPI_CHANNEL_TYPE, PrivateChannel, Type),
    read_vector(SPI_CHANNEL_TYPE, Channel, SharedType),
    Type =?= SPI_UNKNOWN,
    SharedType =\= SPI_UNKNOWN :
      Scheduler = _,
      SharedChannel = Channel,
      NewChannelList = ChannelList;

    read_vector(SPI_CHANNEL_TYPE, PrivateChannel, Type),
    read_vector(SPI_CHANNEL_TYPE, Channel, SharedType),
    Type =\= SPI_UNKNOWN,
    SharedType =?= SPI_UNKNOWN :
      Scheduler = _,
      store_vector(SPI_CHANNEL_TYPE, Type, Channel),
      SharedChannel = Channel,
      NewChannelList = ChannelList;

    read_vector(SPI_CHANNEL_TYPE, PrivateChannel, Type),
    read_vector(SPI_CHANNEL_TYPE, Channel, SharedType),
    otherwise,
    read_vector(SPI_CHANNEL_NAME, PrivateChannel, Id) :
      SharedChannel = Channel,
      NewChannelList = ChannelList |
	ambient_diagnostic("shared type conflict!" - Id(Type =\= SharedType),
				Scheduler).
    

add_local_channel(Channel, PrivateChannels, NewPrivateChannels) :-

    vector(Channel),
    read_vector(SPI_CHANNEL_NAME, Channel, Name),
    Name =?= _String(_PrivateId) :
      NewPrivateChannels = [Channel | PrivateChannels];

    otherwise :
      Channel = _,
      NewPrivateChannels = PrivateChannels.

remove_local_channels(Indices, ChannelTuple,
	PrivateChannels, NewPrivateChannels,
	SharedChannels, NewSharedChannels, Unremoved) :-

    Indices =?= true(Indices') |
	self;

    Indices ? Index,
    arg(Index, ChannelTuple, Channel),
    vector(Channel),
    read_vector(SPI_CHANNEL_NAME, Channel, Name),
    Name =?= _String(LocalId),
    number(LocalId) |
	remove_channel(Channel, PrivateChannels, PrivateChannels',
			UnremovedLocal, []),
	unremoved_local_channel,
	self;

    Indices ? Index,
    arg(Index, ChannelTuple, Channel),
    vector(Channel),
    read_vector(SPI_CHANNEL_NAME, Channel, Name),
    string(Name) |
	/* Public Channel - ignore - they're all going at once ? */
	screen#display("remove public channel" - Name),
	self;

    Indices ? Index,
    arg(Index, ChannelTuple, Channel),
    vector(Channel),
    read_vector(SPI_CHANNEL_NAME, Channel, Name),
    Name = _Prefix(SharedName),
    "" @< SharedName |
	remove_channel(Channel, SharedChannels, SharedChannels',
			Unremoved, Unremoved'),
	self;

    Indices ? Index,
    arg(Index, ChannelTuple, Channel),
    vector(Channel),
    read_vector(SPI_CHANNEL_NAME, Channel, Name),
    otherwise |
	screen#display("Unrecognized Channel Kind " - Name),
	self;

    Indices =?= [] :
      ChannelTuple = _,
      NewPrivateChannels = PrivateChannels,
      NewSharedChannels = SharedChannels,
      Unremoved = [];

    Indices =\= true, Indices =\= [_|_], Indices =\= [] :
      ChannelTuple = _,
      NewPrivateChannels = PrivateChannels,
      NewSharedChannels = SharedChannels,
      Unremoved = [] |
	spi_utils#show_value(Indices, [], Failure),
	wait_fail.

  wait_fail(Failure) :-
    known(Failure) |
	fail(Failure).


remove_shared_channels(ChannelTuple, SharedChannels, NewSharedChannels,
				Unremoved) + (Index = 1) :-

    arg(Index, ChannelTuple, Channel),
    vector(Channel),
    arg(Index, ChannelTuple, Channel),
    vector(Channel),
    Index++ |
	remove_channel(Channel, SharedChannels, SharedChannels',
			Unremoved, Unremoved'),
	self;

    otherwise :
      ChannelTuple = _,
      Index = _,
      NewSharedChannels = SharedChannels,
      Unremoved = [].

  remove_channel(Channel, List, NewList, Unremoved, NewUnremoved) :-

    List ? Channel :
      NewList = List',
      NewUnremoved = Unremoved;

    List ? Other,
    Channel =\= Other :
      NewList ! Other |
	self;

    List =?= [] :
      NewList = [],
      Unremoved = [Channel | NewUnremoved].

  unremoved_local_channel(UnremovedLocal) :-

    UnremovedLocal =\= [_] | true;

    UnremovedLocal =?= [Channel],
    read_vector(SPI_CHANNEL_NAME, Channel, Name) |
	screen#display("Couldn't find local channel" - Name).


detach_channel_list(ChannelList, Scheduler) :-

    ChannelList = [] :
      Scheduler = _;

    list(ChannelList) :
      write_channel(close(ChannelTuple?, _Reply), Scheduler) |
	make_tuple(N?, Tuple),
	detach_channel_tuple(ChannelList, Tuple, 0, N, ChannelTuple);

    otherwise : /* Scheduler closed! */
      ChannelList = _,
      Scheduler = _.

  detach_channel_tuple(List, Tuple1, I, N, Tuple2) :-

    List ? Vector,
    vector(Vector),
    I' := I + 1 :
      store_vector(SPI_CHANNEL_REFS, 1, Vector) |
	arg(I', Tuple1, Vector),
	self;

    otherwise : List = _,
      N = I,
      Tuple2 = Tuple1.


copy_local_channels(Idle, Locals, NewLocals, Ambient, In, NextIn) :-

    Locals =?= {A},
    vector(A) |
	merge_local_channels(Idle, Locals, NewLocals, copy, Ambient, Ambient,
					In, NextIn);

    Locals =?= {A, B},
    vector(A),
    vector(B) |
	merge_local_channels(Idle, Locals, NewLocals, copy, Ambient, Ambient,
					In, NextIn);

    Locals =?= {A, B, C},
    vector(A),
    vector(B),
    vector(C) |
	merge_local_channels(Idle, Locals, NewLocals, copy, Ambient, Ambient,
					In, NextIn);

    arity(Locals, Arity),
    Arity > 3 |
	wait_for_vectors,
	merge_local_channels(Idle, Locals, NewLocals, copy, Ambient, Ambient,
					In, NextIn).

  wait_for_vectors(Locals, Arity) :-

    Arity > 0,
    arg(Arity, Locals, V),
    vector(V),
    Arity -- |
	self;

    Arity =< 0 :
      Locals = _.

merge_local_channels(Idle, Argument, NewArgument, Action,
			FromAmbient, ToAmbient, In, NextIn) :-
    known(Idle),
    freeze(Argument, FrozenArgument, FrozenAtoms) :
      melt(FrozenArgument, MeltedArgument, MeltedAtoms) |
	merge_channels.

  merge_channels(FrozenAtoms, MeltedAtoms, NewArgument, Action,
		FromAmbient, ToAmbient, In, NextIn, MeltedArgument) :-
    /* Variables are inherited by the new ambient,
    ** not by the receiver of the channel tuple.
     */
    FrozenAtoms ? Variable,
    unknown(Variable) :
      MeltedAtoms ! Variable |
	self;

    /* Communication channels may not have a unique structure -
       The user should not employ vectors of size CHANNEL_SIZE. */
    FrozenAtoms ? Channel,
    vector(Channel),
    arity(Channel, CHANNEL_SIZE),
    read_vector(SPI_CHANNEL_NAME, Channel, ChannelName),
    string(ChannelName),
    Action =\= pass :
      In ! lookup(public, Channel, NewChannel),
      MeltedAtoms ! NewChannel? |
	remove_one_reference,
	self;

    FrozenAtoms ? Channel,
    vector(Channel),
    arity(Channel, CHANNEL_SIZE),
    read_vector(SPI_CHANNEL_NAME, Channel, ChannelName),
    string(ChannelName),
    Action =?= pass,
    read_vector(SPI_CHANNEL_REFS, Channel, Refs) :
      In ! lookup(public, Channel, NewChannel, Refs),
      MeltedAtoms ! NewChannel? |
	self;

    FrozenAtoms ? Channel,
    vector(Channel),
    arity(Channel, CHANNEL_SIZE),
    read_vector(SPI_CHANNEL_NAME, Channel, ChannelName),
    tuple(ChannelName),
    Action =\= pass :
      In ! lookup(private, Channel, NewChannel),
      MeltedAtoms ! NewChannel? |
	remove_one_reference,
	self;

    FrozenAtoms ? Channel,
    vector(Channel),
    arity(Channel, CHANNEL_SIZE),
    read_vector(SPI_CHANNEL_NAME, Channel, ChannelName),
    tuple(ChannelName),
    Action =?= pass,
    read_vector(SPI_CHANNEL_REFS, Channel, Refs) :
      In ! lookup(private, Channel, NewChannel, Refs),
      MeltedAtoms ! NewChannel? |
	self;

    FrozenAtoms ? FromAmbient :
      MeltedAtoms ! ToAmbient |
	self;

    FrozenAtoms ? Other,
    otherwise :
      MeltedAtoms ! Other |
	self;

    FrozenAtoms = [] :
      Action = _,
      ToAmbient = _,
      FromAmbient = _,
      In = NextIn,
      MeltedAtoms = [],
      MeltedArgument = NewArgument.

  /* A little kluge - fix someday to actually close channel in spi_monitor. */
  remove_one_reference(FromAmbient, ToAmbient, Channel) :-

    ToAmbient =\= FromAmbient,
    read_vector(SPI_CHANNEL_REFS, Channel, Refs),
    Refs--,
    Refs' > 0 :
      store_vector(SPI_CHANNEL_REFS, Refs', Channel);

    ToAmbient =\= FromAmbient,
    read_vector(SPI_CHANNEL_REFS, Channel, Refs),
    Refs--,
    Refs' =< 0 |
	remove_one_reference_from_ambient;

    ToAmbient =?= FromAmbient :
      Channel = _;

    otherwise |
read_vector(AMBIENT_ID, FromAmbient, FId),
read_vector(AMBIENT_ID, ToAmbient, TId),
read_vector(SPI_CHANNEL_NAME, Channel, CId),
%	fail(remove_one_reference(FromAmbient, ToAmbient, Channel).
	fail(remove_one_reference(FId, TId, CId)).

  remove_one_reference_from_ambient(Channel, FromAmbient) :-

    vector(Channel),
    vector(FromAmbient) :
      write_vector(AMBIENT_CONTROL, close({Channel}), FromAmbient);

    otherwise :
      Channel = _,
      FromAmbient = _.


merge_public_channels(List, PublicChannels, NewPublicChannels, Scheduler) 
			+ (Last = "") :-

    List =?= [] :
      Last = _,
      Scheduler = _,
      NewPublicChannels = PublicChannels;

    List ? Name(NewChannel, BaseRate),
    Last @< Name,
    we(NewChannel),
    PublicChannels ? Public,
    Public = Name(SpiChannel, _ComputeWeight, BaseRate),
    vector(SpiChannel),
    read_vector(SPI_CHANNEL_REFS, SpiChannel, References),
    References++ :
      Last = _,
      NewChannel = SpiChannel,
      store_vector(SPI_CHANNEL_REFS, References', SpiChannel),
      NewPublicChannels ! Public,
      Last' = Name |
	self;

    List ? Name(_NewChannel, BaseRate),
    PublicChannels ? Entry,
    Entry = Name(_SpiChannel, _ComputeWeight, OtherBaseRate),
    BaseRate =\= OtherBaseRate :
      NewPublicChannels ! Entry |
	fail(public_channel(rate_conflict(Name - BaseRate =\= OtherBaseRate))),
	self;

    List = [Name(_NewChannel, _BaseRate) | _], string(Name),
    PublicChannels ? Entry,
    Entry = Last'(_, _, _),
    Last' @< Name :
      Last = _,
      NewPublicChannels ! Entry |
	self;

    List ? Name(NewChannel, BaseRate),
    we(NewChannel),
    PublicChannels =?= [Name1(_, _, _) | _],
    string(Name),
    Last @< Name, Name @< Name1,
    string_to_dlist("public.", GL, GT),
    string_to_dlist(Name, NL, []) :
      NewChannel = NewChannel'?,
      List'' = [Name(NewChannel', BaseRate) | List'],
      PublicChannels' =
	[Name(SpiChannel?, SPI_DEFAULT_WEIGHT_NAME, BaseRate)
	| PublicChannels],
      write_channel(new_channel(Id?, SpiChannel, SPI_DEFAULT_WEIGHT_NAME,
				BaseRate), Scheduler),
      GT = NL |
	list_to_string(GL, Id),
	self;

    List ? Name(NewChannel, BaseRate),
    Last @< Name,
    we(NewChannel),
    PublicChannels ? Public,
    Public = Name(SpiChannel, _ComputeWeight, BaseRate),
    vector(SpiChannel),
    read_vector(SPI_CHANNEL_REFS, SpiChannel, References),
    References++ :
      Last = _,
      NewChannel = SpiChannel,
      store_vector(SPI_CHANNEL_REFS, References', SpiChannel),
      NewPublicChannels ! Public,
      Last' = Name |
	self;

    List ? Name(_NewChannel, BaseRate),
    PublicChannels ? Entry,
    Entry = Name(_SpiChannel, _ComputeWeight, OtherBaseRate),
    BaseRate =\= OtherBaseRate :
      NewPublicChannels ! Entry |
	fail(public_channel(rate_conflict(Name - BaseRate =\= OtherBaseRate))),
	self;

    List = [Name(_NewChannel, _BaseRate) | _], string(Name),
    PublicChannels ? Entry,
    Entry = Last'(_, _, _),
    Last' @< Name :
      Last = _,
      NewPublicChannels ! Entry |
	self;

    List ? Name(NewChannel, BaseRate),
    we(NewChannel),
    PublicChannels =?= [Name1(_, _, _) | _],
    string(Name),
    Last @< Name, Name @< Name1,
    string_to_dlist("public.", GL, GT),
    string_to_dlist(Name, NL, []) :
      NewChannel = NewChannel'?,
      List'' = [Name(NewChannel', BaseRate) | List'],
      PublicChannels' =
	[Name(SpiChannel?, SPI_DEFAULT_WEIGHT_NAME, BaseRate)
	| PublicChannels],
      write_channel(new_channel(Id?, SpiChannel, SPI_DEFAULT_WEIGHT_NAME,
				BaseRate), Scheduler),
      GT = NL |
	list_to_string(GL, Id),
	self;

    List ? Name(NewChannel, CW, BaseRate),
    Last @< Name,
    we(NewChannel),
    PublicChannels ? Public,
    Public = Name(SpiChannel, ComputeWeight, BaseRate),
    vector(SpiChannel),
    read_vector(SPI_CHANNEL_REFS, SpiChannel, References),
    References++ :
      Last = _,
      CW = ComputeWeight?,
      NewChannel = SpiChannel,
      store_vector(SPI_CHANNEL_REFS, References', SpiChannel),
      NewPublicChannels ! Public,
      Last' = Name |
	self;

    List ? Name(_NewChannel, BaseRate, _),
    PublicChannels ? Entry, Entry = Name(_SpiChannel, OtherBaseRate, _),
    BaseRate =\= OtherBaseRate :
      NewPublicChannels ! Entry |
	fail(public_channel(rate_conflict(Name - BaseRate =\= OtherBaseRate))),
	self;

    List ? Name(_NewChannel, ComputeWeight, _),
    PublicChannels ? Entry, Entry = Name(_SpiChannel, OtherComputeWeight, _),
    ComputeWeight =\= OtherComputeWeight :
      NewPublicChannels ! Entry |
	fail(public_channel(compute_weight_conflict(Name -
				ComputeWeight =\= OtherComputeWeight))),
	self;

    List = [Name(_NewChannel, _ComputeWeight, _BaseRate) | _], string(Name),
    PublicChannels ? Entry,
    Entry = Last'(_, _, _),
    Last' @< Name :
      Last = _,
      NewPublicChannels ! Entry |
	self;

    List ? Name(NewChannel, ComputeWeight, BaseRate),
    we(NewChannel),
    PublicChannels =?= [Name1(_, _, _) | _],
    string(Name),
    Last @< Name, Name @< Name1,
    string_to_dlist("public.", GL, GT),
    string_to_dlist(Name, NL, []) :
      NewChannel = NewChannel'?,
      List'' = [Name(NewChannel', ComputeWeight, BaseRate) | List'],
      PublicChannels' = [Name(SpiChannel?, ComputeWeight, BaseRate)
			| PublicChannels],
      write_channel(new_channel(Id?, SpiChannel, ComputeWeight, BaseRate),
			Scheduler),
      GT = NL |
	list_to_string(GL, Id),
	self;

    otherwise :
      Last = _,
      Scheduler = _,
      NewPublicChannels = PublicChannels |
	fail(merge_public_channels(List)).


/***************************** Utilities ************************************/


ambient_diagnostic(Diagnostic, Scheduler) :-

    true :
      write_channel(diagnostic(Diagnostic), Scheduler);

    /* This is the case where the Scheduler has closed. */
    otherwise :
      Diagnostic = _,
      Scheduler = _ .


ambient_suspending(ReadyChildren,
		   PublicChannels, PrivateChannels, SharedChannels,
		   Ready) :-

    known(ReadyChildren) |
	copy_public_channels,
	remove_all_communications(Channels?, Reply1),
	remove_all_communications(PrivateChannels, Reply2),
	remove_all_communications(SharedChannels, Reply3),
	suspended_when_ready.

  suspended_when_ready(Reply1, Reply2, Reply3, Ready) :-

    known(Reply1), known(Reply2), known(Reply3) :
      Ready = done.
    

ambient_tree(Kind, Children, NewChildren, SubTrees) + (Done = done) :-

    known(Done),
    Children ? Child,
    read_vector(AMBIENT_ID, Child, ChildId) :
      write_vector(AMBIENT_CONTROL, tree(Kind, Done', SubTree), Child, Child'),
      NewChildren ! Child',
      SubTrees ! Kind(ChildId, Done'?, SubTree?) |
	self;

    known(Done),
    Children ? ClosedChild,
    read_vector(AMBIENT_ID, ClosedChild, ChildId),
    otherwise :
      /* write_vector failed - child closed. */
      NewChildren ! ClosedChild,
      SubTrees ! Kind(ChildId, [], []) |
	self;

    known(Done),
    Children =?= [] :
      Kind = _,
      NewChildren = [],
      SubTrees = [].


concatenate_lists(Lists, Out) :-

    Lists = [List | Rest],
    List ? Item, Item =\= [], Item =\= [_ | _] :
      Out ! Item,
      Lists' = [List' | Rest] |
	self;

    Lists = [List | Rest],
    List ? Item, Item =\= [], Item =?= [_ | _] :
      Lists' = [Item, List' | Rest] |
	self;

    Lists = [List | Rest],
    List ?  [] :
      Lists' = [List' | Rest] |
	self;

    Lists ? [] |
	concatenate_lists;

    Lists =?= [] :
      Out = [].

    
control_children(Control, Children, NewChildren, Ready) + (Done = done) :-

    known(Done),
    Children ? Child :
      NewChildren ! Child',
      write_vector(AMBIENT_CONTROL, Control(Done'), Child, Child') |
	self;

    known(Done),
    Children = [] :
      Control = _,
      NewChildren = [],
      Ready = true.


copy_public_channels(PublicChannels, Channels) :-
    PublicChannels ? _Head |
	copy_interior.

  copy_interior(PublicChannels, Channels) :-

    PublicChannels ? Entry,
    PublicChannels' =\= [],
    arg(2, Entry, Channel) :
      Channels ! Channel |
	self;

    PublicChannels = [_] :
      Channels = [].


remove_all_communications(Channels, Reply) :-

    Channels ? Channel,
    read_vector(SPI_SEND_ANCHOR, Channel, SendQueue),
    read_vector(SPI_RECEIVE_ANCHOR, Channel, ReceiveQueue),
    arg(SPI_MESSAGE_LINKS, SendQueue, SendLinks),
    arg(SPI_MESSAGE_LINKS, ReceiveQueue, ReceiveLinks) :
      store_vector(SPI_NEXT_MS, SendQueue, SendLinks),
      store_vector(SPI_PREVIOUS_MS, SendQueue, SendLinks),
      store_vector(SPI_SEND_WEIGHT, 0, Channel),
      store_vector(SPI_NEXT_MS, ReceiveQueue, ReceiveLinks),
      store_vector(SPI_PREVIOUS_MS, ReceiveQueue, ReceiveLinks),
      store_vector(SPI_RECEIVE_WEIGHT, 0, Channel) |
	self;

    Channels =?= [] :
      Reply = true.

remove_shared_communications(Ambient, Channels, Scheduler, Reply) + 
			(Removed = false) :-

    Channels ? Channel,
    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    bitwise_and(Type, SPI_TYPE_MASK, MaskedType),
    MaskedType =\= SPI_HOMODIMERIZED,
    read_vector(SPI_SEND_ANCHOR, Channel, SendQueue),
    read_vector(SPI_RECEIVE_ANCHOR, Channel, ReceiveQueue) |
	remove_from_queue(Ambient, Channel, Scheduler, SPI_SEND_WEIGHT,
				SendQueue, Removed, Removed'),
	remove_from_queue(Ambient, Channel, Scheduler, SPI_RECEIVE_WEIGHT,
				ReceiveQueue, Removed', Removed''),
	self;

    Channels ? Channel,
    read_vector(SPI_CHANNEL_TYPE, Channel, Type),
    bitwise_and(Type, SPI_TYPE_MASK, MaskedType),
    MaskedType =?= SPI_HOMODIMERIZED,
    read_vector(SPI_DIMER_ANCHOR, Channel, DimerQueue) |
	remove_from_queue(Ambient, Channel, Scheduler, SPI_DIMER_WEIGHT,
				DimerQueue, Removed, Removed'),
	self;

    Channels =?= [] :
      Ambient = _,
      Scheduler = _ |
	unify_without_failure(Reply, Removed).

  remove_from_queue(Ambient, Channel, Scheduler, Index, Anchor,
			Removed, NewRemoved) + (PreviousMs = Anchor) :-

    arg(SPI_MESSAGE_LINKS, PreviousMs, Links),
    read_vector(SPI_NEXT_MS, Links, PreviousMs'),
    PreviousMs' =\= Anchor,
    arg(SPI_AMBIENT_CHANNEL, PreviousMs', MsAmbient),
    MsAmbient =\= Ambient |
	self;

    arg(SPI_MESSAGE_LINKS, PreviousMs, PreviousLinks),
    read_vector(SPI_NEXT_MS, PreviousLinks, CurrentMs),
    CurrentMs =\= Anchor,
    arg(SPI_AMBIENT_CHANNEL, CurrentMs, MsAmbient),
    MsAmbient =?= Ambient,
    arg(SPI_MESSAGE_LINKS, CurrentMs, CurrentLinks),
    read_vector(SPI_NEXT_MS, CurrentLinks, NextMs),
    arg(SPI_MESSAGE_LINKS, NextMs, NextLinks),
    read_vector(Index, Channel, Weight),
    arg(SPI_MS_MULTIPLIER, CurrentMs, Multiplier),
    Weight -= Multiplier :
      Removed = _,
      Removed' = true,
      store_vector(Index, Weight', Channel),
      store_vector(SPI_NEXT_MS, NextMs, PreviousLinks),
      store_vector(SPI_PREVIOUS_MS, PreviousMs, NextLinks) |
	self;

    arg(SPI_MESSAGE_LINKS, PreviousMs, Links),
    read_vector(SPI_NEXT_MS, Links, NotMs),
    NotMs =?= Anchor :
      Ambient = _,
      Channel = _,
      Index = _,
      Scheduler = _,
      NewRemoved = Removed;

    otherwise,
    read_vector(SPI_CHANNEL_NAME, Channel, Name) :
      Anchor = _,
      Channel = _,
      PreviousMs = _,
      NewRemoved = Removed |
      /* This should never happen - internal failure!? */
	ambient_diagnostic((Ambient:remove(Name)-failed(Index)), Scheduler).


resume_ambient_when_ready(In, Events, FromSub, Done,
			  Ambient, Parent, Children,
			  PublicChannels, PrivateChannels,
			  Requests, Controls, AmbientDone,
			  Scheduler, Debug,
			  Ready) :-

    known(Ready) :
      Controls ! resume |
	DEBUG(resume_when_ready, detach(PrivateChannels, Channels)),
	detach_channel_list(PrivateChannels, Scheduler),
	copy_public_channels,
	detach_channel_list(Channels, Scheduler),
	serve_ambient0.


resume_controls_when_ready(Reply1, Reply2, Reply3, Ready,
				Controls, NewControls) :-
    known(Reply1), known(Reply2), known(Reply3) :
      Ready = true,
      Controls = [resume | NewControls].

/***************************** formatting ***********************************/
/*

format_channel_tuple(Tuple, Out) :-

    arity(Tuple, Arity),
    make_tuple(Arity, OutTuple) :
     Out = OutTuple |
	format_channel_tuple1(Tuple, Out, 1).

  format_channel_tuple1(Tuple, Out, Index) :-

    Index++ =< arity(Tuple),
    arg(Index, Tuple, Variable),
    arg(Index, Out, OutVariable),
    we(Variable) :
      OutVariable = Variable |
	self;

    Index++ =< arity(Tuple),
    arg(Index, Tuple, Channel),
    arg(Index, Out, OutChannel),
    known(Channel) |
	format_channel(Channel, OutChannel),
	self;

    otherwise :
      Index = _,
      Out = _,
      Tuple = _.

format_channel_list(List, Out) :-

    List ? Channel,
    vector(Channel),
    arity(Channel, CHANNEL_SIZE) :
      Out ! CH |
	format_channel(Channel, CH),
	self;

    List ? Other,
    otherwise :
      Out ! not_a_channel(Other) |
	self;

    List =?= [] :
      Out = [];

    List =\= [_|_], List =\= [] :
      List' = [List] |
	self.

format_channel(Channel, CH) :-

    vector(Channel),
    read_vector(SPI_BLOCKED,          Channel, Blocked),
    read_vector(SPI_CHANNEL_TYPE,     Channel, Type),
    read_vector(SPI_CHANNEL_RATE,     Channel, Rate),
    read_vector(SPI_CHANNEL_REFS,     Channel, Refs),
    read_vector(SPI_SEND_ANCHOR,      Channel, Sends),
    read_vector(SPI_SEND_WEIGHT,      Channel, WeightS),
    read_vector(SPI_RECEIVE_ANCHOR,   Channel, Receives),
    read_vector(SPI_RECEIVE_WEIGHT,   Channel, WeightR),
    read_vector(SPI_CHANNEL_NAME,     Channel, Name) :
      CH = Name(TypeRate, Send, Receive) - BlockedRefs |
	format_typerate(Type, Rate, TypeRate),
	format_blockedrefs(Blocked, Refs, BlockedRefs),
	format_send(Sends, WeightS, Send),
	format_receive(Receives, WeightR, Receive);

    otherwise :
      CH = Channel.

format_typerate(Type, Rate, TypeRate) :-

    Type =:= SPI_CHANNEL_ANCHOR :
      Rate = _,
      TypeRate = anchor;

    Type =:= SPI_UNKNOWN :
      TypeRate = unknown(Rate);

    Type =:= SPI_BIMOLECULAR :
      TypeRate = bimolecular(Rate);

    Type =:= SPI_BIMOLECULAR + SPI_RANDOM_MESSAGE :
      TypeRate = 'bimolecular"'(Rate);

    Type =:= SPI_BIMOLECULAR_PRIME :
      TypeRate = "bimolecular'"(Rate);

    Type =:= SPI_BIMOLECULAR_PRIME + SPI_RANDOM_MESSAGE :
      TypeRate = "bimolecular'"""(Rate);

    Type =:= SPI_HOMODIMERIZED :
      TypeRate = homodimerized(Rate);

    Type =:= SPI_HOMODIMERIZED + SPI_RANDOM_MESSAGE :
      TypeRate = 'homodimerized"'(Rate);

    Type =:= SPI_HOMODIMERIZED_PRIME :
      TypeRate = "homodimerized'"(Rate);

    Type =:= SPI_HOMODIMERIZED_PRIME + SPI_RANDOM_MESSAGE :
      TypeRate = "homodimerized'"""(Rate);

    Type =:= SPI_INSTANTANEOUS :
      Rate = _,
      TypeRate = instantaneous;

    Type =:= SPI_SINK :
      Rate = _,
      TypeRate = sink;

    otherwise :
      TypeRate = other(Type, Rate).

format_blockedrefs(Blocked, Refs, BlockedRefs) :-

    Blocked =?= TRUE :
      BlockedRefs = blocked(Refs);

    Blocked =?= FALSE :
      BlockedRefs = Refs;

    otherwise :
      BlockedRefs = blockedrefs(Blocked, Refs).


format_send(Sends, Weight, Send) :-

    arg(SPI_MESSAGE_LINKS, Sends, NextLink),
    read_vector(SPI_NEXT_MS, NextLink, Message),
    Message =\= Sends :
      Send = sends(Weight);

    otherwise :
      Sends = _,
      Weight = _,
      Send = no_sends.

format_receive(Receives, Weight, Receive) :-

    arg(SPI_MESSAGE_LINKS, Receives, NextLink),
    read_vector(SPI_NEXT_MS, NextLink, Message),
    Message =\= Receives :
      Receive = receives(Weight);

    otherwise :
      Receives = _,
      Weight = _,
      Receive = no_receives.
*/
