/*

Domain Server of Distributed Logix System

Bill Silverman

Last update by		$Author: bill $
		       	$Date: 2004/08/17 04:44:16 $
Currently locked by 	$Locker:  $
			$Revision: 1.2 $
			$Source: /home/qiana/Repository/Logix/domain_server.cp,v $

Copyright (C) 1989, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-language(dfcp).
-mode(trust).
-export([domain/4, process_dictionary/2]).

Stream, In, Out ::= [Any].
Channel ::= Vector.

/* Input from Hierarchy Server, Input to Domain Server,
   Input Dictionary Requests, Output to Hierarchy Server */

IHS, IDS, IDR ::= Stream.

FPS ::= [find(Id, Reply)].

OHS ::= [HierarchyRequest].
HierarchyRequest ::= get_service(Id, Answer).

/* Dictionary Request, Domain Server, Service Server   Channel */

DRC, DSC, SSC ::= Channel. 


SCC, UCC ::= {Stream, Any, Any, Channel}.


ServiceId ::= [String].	% move to self.cp

% Dictionary Entry.

Locus ::= remote ; "local".
Close ::= close.

Service ::= Locus(Close).

PM ::= Module .


procedure domain(IHS, SCC, OHS, PM).

domain(IHS, SCC, OHS, PM) :-
    string(PM) |		% The best I can do in dfcp!
	process_dictionary(FPS?, CLS),
	close_services(CLS?),
	splitCC(SCC, SCC', SCC''),
	dictionary(DRC?, SCC'?, OHS, DSC!, FPS, PCH!),
	activate_module(PCH?, PM?, [processor], ModuleKind, DSC!),
	module_kind(ModuleKind?, SCC''?, _PCH, _Kind),
	domain_server(IHS, DSC?, DRC!).


procedure close_services(CLS).

close_services(CLS) :-

    CLS ? close(_, Locus(SSC)) |
	close_service(Locus, SSC),
	self;

    CLS = [] |
	true.

procedure close_service(Locus, SSC).

close_service(Locus, SSC) :-

    Locus = "local",
    channel(SSC) |
	close_channel(SSC);

    otherwise |
	Locus = _, SSC = _ .

/********************** D O M A I N   S E R V E R ****************************/

procedure domain_server(IHS, IDS, DRC).
procedure domain_server(IHS, IDS, DRC, Close).

/* Serve Goals from within domain */

domain_server(IHS, IDS, DRC) + (Close, Closed) :-

    initially | Closed = Close? ;

    IDS ? export(CallInfo, Scope, Goals, UCC),		/* export/4 */
    channel(DRC),
    listener(Closed) |
	domain_goals(Goals, UCC, DRC, CallInfo, Scope, Closed, domain),
	self;

    IDS ? reduce(CallInfo, Scope, Goals, UCC),		/* reduce/4 */
    channel(DRC),
    listener(Closed) |
	domain_goals(Goals, UCC, DRC, CallInfo, Scope, Closed, domain),
	self;

    IDS ? Goal,						/* Other */
    Goal =\= reduce(_,_,_,_), Goal =\= export(_,_,_,_),
    channel(DRC) |
	write_channel(Goal, DRC, DRC'),
	self;

/* Serve goals from outside domain */

    IHS ? export(CallInfo, Scope, Goals, UCC),		/* export/4 */
    channel(DRC),
    listener(Closed) |
	domain_goals(Goals, UCC, DRC, CallInfo, Scope, Closed, domain),
	self;

    IHS ? filter(IDS', IDS^) |				/* filter/2 */
	self;

    IHS ? Goal,						/* Other */
    Goal =\= export(_,_,_,_), Goal =\= filter(_,_),
    channel(DRC) |
	write_channel(Goal, DRC, DRC'),
	self;

    IHS = [],
    channel(DRC) | IDS = _, Closed = _,
	Close = close,
	close_channel(DRC).

/***************** D I C T I O N A R Y   S E R V E R *************************/

procedure dictionary(IDR, SCC, OHS, DSC, FPS, PCH).

dictionary(IDR, SCC, OHS, DSC, FPS, PCH) :-

    IDR ? find(ServiceId, SSC),
    ServiceId = [processor | _],
    channel(PCH) |
	SSC = PCH,
	self;

    IDR ? find(ServiceId, SSC),
    ServiceId =\= [processor | _],
    listener(ServiceId) |
	FPS ! find(ServiceId, Result),
	found_service(Result?, ServiceId, SSC, Reply),
	added_service(Reply?, SCC, SCC', DSC, DSC', OHS, OHS'?),
	self;

    IDR ? add_service(ServiceId, Data, SystemReply),
    listener(ServiceId),
    listener(Data),
    listener(DSC) |
	FPS ! find(ServiceId, Result),
	OHS ! add_service(ServiceId, Data, SystemReply),
	OHS'' ! get_service(ServiceId, AnswerH),
	add_service_result(Result?, Locus?(SSC?), Close?),
	add_service_answer(Data, AnswerD, OHS', OHS''?),
	merge_added(AnswerD?, AnswerH?, Answer),
	splitCC(SCC, SCC', SCC''),
	new_service(Answer?, SCC'?, DSC, ServiceId, Locus, SSC, Close),
	self;

    IDR ? local_service(ServiceId, Answer, true^),
    listener(DSC),
    listener(ServiceId) |
	FPS ! find(ServiceId, Result),
	add_service_result(Result?, Locus?(SSC?), Close?),
	splitCC(SCC, SCC', SCC''),
	new_service(Answer, SCC'?, DSC, ServiceId, Locus, SSC, Close),
	self;

    IDR ? hierarchy(Goal) |
	OHS ! Goal,
	self;

    IDR ? domain_channel(Domain),
    listener(DSC) |
	Domain = DSC,
	self;

    IDR ? link(LinkName, ServiceId, Stream),
    listener(DSC) |
	no_server(Stream, ServiceId@LinkName, DSC, _),
	self;

    IDR ? Failed,
    Failed = failed(ServiceId, Goal, Reason),
    SCC = {CO, CL, CR, CC},
    channel(CC) |
	write_channel(request(ServiceId, failed(Goal, Reason), CL, CM), CC, CC'),
	SCC' = {CO, CM?, CR, CC'?},
	self;

    otherwise,
    IDR ? Other,
    SCC = {CO, CL, CR, CC},
    channel(CC) |
	write_channel(request(["_?_"], failed(Other, unknown), CL, CM), CC, CC'),
	SCC' = {CO, CM?, CR, CC'?},
	self;

    SCC = {CO, CL, CR, CC},
    CO ? suspend, known(CO') |
	SCC' = {CO', CL, CR, CC},
	self;

    SCC = {CO, CL, CR, CC},
    CO ? resume |
	SCC' = {CO', CL, CR, CC},
	self;

    SCC = {CO, CL, CR, CC},
    CL ? X |
	CR ! X,
	SCC' = {CO, CL', CR', CC},
	self;

    IDR = [],
    channel(DSC),
    channel(PCH) |
	close_channel(DSC),
	close_channel(PCH),
	OHS = [],
	FPS = [],
	closeCC(SCC).

/************************ find/2 procedures **********************************/

procedure found_service(Result, ServiceId, SSC, Reply).

found_service(Result, ServiceId, SSC, Reply) :-

    Result = found(Locus(FNDC), Locus(FNDC)^, proceed^),
    listener(FNDC) |
	SSC = FNDC,
	ServiceId = _,
	Reply = done ;

    Result = not_found(Locus?(SSC1?)^, open(Close?)^) |
	copy_listener(NewSSC?, SSC, SSC1),
	Reply = new(ServiceId, Locus, NewSSC, Close) ;

    otherwise | Result = _,
	Reply = none(ServiceId, SSC) .


procedure added_service(Reply, AddP1, AddP2).

added_service(Reply, SCC1, SCC2, DSC1, DSC2, OHS1, OHS2) :-

    Reply = done |
	SCC2 = SCC1,
	DSC2 = DSC1,
	OHS1 = OHS2 ;

    Reply = new(ServiceId, Locus?, SSC?, Close),
    listener(ServiceId),
    channel(DSC1) |
	splitCC(SCC1, SCC2, SCC),
	DSC2 = DSC1,
	OHS1 = [get_service(ServiceId, Answer) | OHS2],
	new_service(Answer?, SCC?, DSC1, ServiceId, Locus, SSC, Close);

    Reply = none(ServiceId, Nos!),
    channel(DSC1) |
	SCC2 = SCC1,
	DSC2 = DSC1,
	OHS1 = OHS2,
	no_server(Nos?, ServiceId, DSC1, _).

/********************* add_service/3 procedures ******************************/

procedure add_service_answer(Data, Answer, Done1, Done2).

add_service_answer(Data, Answer, Done1, Done2) :-
		/***	need	module(Binary) 	***/
    Data = module(Binary, UID), string(Binary), string(UID) |
	Answer = module(Binary, _, _),
	Done1 = Done2 ;

    Data = module(MagicNumber, UID), integer(MagicNumber), string(UID) |
	Answer = module(MagicNumber, _, _),
	Done1 = Done2 ;

/**/otherwise | Data = _,
	Answer = director(_, _, _),
	Done1 = Done2 .


procedure add_service_result(Result, Service, Close).

add_service_result(Result, Service, Close) :-

    Result = not_found(Service^, open(Close)^) |
	true;

    Result = found(OldLocus(OldSSC), Service^, replace(Close)^) |
	close_service(OldLocus, OldSSC).

merge_added(AnswerD, AnswerH, Answer) :-

    AnswerD = module(_,_,_),
    listener(AnswerD) |
	unify_without_failure(AnswerD, AnswerH),
	Answer = AnswerD ;

    AnswerH = director(_,_,_),
    listener(AnswerH) |
	unify_without_failure(AnswerD, AnswerH),
	Answer = AnswerH ;

    otherwise | AnswerD = _,
	Answer = AnswerH .

/************************* common procedures *********************************/

procedure new_service(Answer, SCC, DSC, ServiceId, Locus, SSC, Close).

new_service(Answer, SCC, DSC, ServiceId, Locus, SSC, Close) :-

    Answer = module(Module, Kind, CloseModule) |
	SSC = IMS!,
	Locus = "local",
	Close = CloseModule?,
	activate_module(IMS?, Module, ServiceId, ModuleKind, DSC),
	module_kind(ModuleKind?, SCC, IMS!, Kind);

    Answer = director(Module, Kind, CloseModule) :
	SSC = IMS!,
	Locus = "local",
	Close = CloseModule?,
	activate_module(IMS?, Module, [self | ServiceId], ModuleKind, DSC),
	module_kind(ModuleKind?, SCC, IMS!, Kind);

    Answer = director(CloseModule) |
	SSC = In!,
	Locus = "local",
	Close = CloseModule?,
	director(In?, ServiceId, DSC),
	closeCC(SCC);

    Answer = import([], CloseModule) :
	SSC = NOs!,
	Locus = "local",
	copy_listener(CloseModule?, CloseModule1, CloseModule2),
	Close = CloseModule1?,
	closeCC(SCC),
	no_server(NOs?, ServiceId, DSC, CloseModule2?);

    otherwise | ServiceId = _,
	Locus = remote,
	closeCC(SCC),
	non_local_service(Answer, DSC, SSC, Close).

procedure module_kind(ModuleKind, SCC, SSC, Kind).

module_kind(ModuleKind, SCC, SSC, Kind) :-

    ModuleKind = monitor(SCC^) |
	Kind = monitor(SSC) ;

    otherwise | SSC = _,
	Kind = ModuleKind,
	closeCC(SCC).

procedure non_local_service(NonLocal, DSC, SSC, Close).

non_local_service(NonLocal, DSC, SSC, Close) :-

    NonLocal = import(ID, CloseModule),
    channel(DSC) |
	Close = CloseModule?,
	write_channel(find(ID, SSC), DSC) ;

    NonLocal = relay(SSC^, CloseModule) | DSC = _,
	Close = CloseModule? ;

    NonLocal = director(MSC, _, CloseModule),
    channel(MSC) |	% redundant check for system test!
	DSC = _,
	SSC = MSC,
	Close = CloseModule? .


/************************ director filter ************************************/

procedure director(In, ServiceId, Domain).

director(In, ServiceId, Domain) :-

    In ? _Functor(CallInfo, _Scope, Goals, UCC),
    listener(ServiceId),
    channel(Domain) |
	write_channel(export(CallInfo, ServiceId, Goals, UCC), Domain),
	self;

    In ? filter(In'', OldIn) |
	OldIn = In',
	self;

    In ? Other,
    otherwise,
    listener(ServiceId),
    channel(Domain) |
	write_channel(failed(ServiceId, Other, unrecognized), Domain),
	self;

    In = [] | ServiceId = _, Domain = _ .

/************************ no service server **********************************/

procedure no_server(NOs, Id, DSC, Closed).

no_server(NOs, Id, DSC, Closed) :-

    NOs ? export(CallInfo, Scope, Goals, UCC),		/* export/4 */
    listener(Id),
    listener(Closed) |
	domain_goals(Goals, UCC, _, CallInfo, Scope, Closed, no_service(Id)),
	self;

    NOs ? reduce(CallInfo, Scope, Goals, UCC),		/* reduce/4 */
    listener(Id),
    listener(Closed) |
	domain_goals(Goals, UCC, _, CallInfo, Scope, Closed, no_service(Id)),
	self;

    NOs ? filter(NOs'', NOs'^) |				/* filter/2 */
	self;

    NOs ? Goal,						/* Other */
    otherwise,
    listener(Id),
    channel(DSC) |
	write_channel(failed(Id, Goal, unrecognized), DSC),
	self;

    NOs = [] : Id = _, DSC = _, Closed = _ ;

    Closed = close : NOs = _, Id = _, DSC = _ .

/*************** D O M A I N   G O A L S   S E R V E R ***********************/

procedure domain_goals(Goals, UCC, DRC, CallInfo, Scope, Closed, Serve).

domain_goals(Goals, UCC, DRC, CallInfo, Scope, Closed, Serve) :-

    Serve = domain,
    Goals = attributes([director]^) | DRC = _, CallInfo = _, Scope = _,
				      Closed = _,
	closeCC(UCC);

    Serve = domain,
    Goals = service_id(Scope^) | DRC = _, CallInfo = _, Closed = _,
	closeCC(UCC);

    Serve = domain,
    Goals = "_unique_id"(RID),
    channel(DRC) | CallInfo = _, Closed = _,
	RID = UID?,
	write_channel(hierarchy(locate(Scope, UID)), DRC),
	closeCC(UCC);

    Serve = domain,
    Goals = "_close"(Name, CloseReply),
    string(Name),
    channel(DRC) | CallInfo = _, Closed = _,
	CloseReply = SystemReply?,
	write_channel(hierarchy(close([Name | Scope], SystemReply)), DRC),
	closeCC(UCC);

    Serve = domain,
    Goals = "_private_goals"(Input, ServiceId, In),
    listener(Input),
    listener(ServiceId) |
	In = In'?,
	Id = "_private_goals"(Input, ServiceId, In'),
	Request = "_private_goals"(Input, ServiceId, _),
	valid_id_target(ServiceId, Id?, Request?, Serve'),
	self;

    Serve = "_private_goals"(Input, ServiceId, In),
    channel(DRC) |
	Goals = _, Scope = _, Closed = _,
	write_channel(domain_channel(Domain), DRC),
	private_goals(Input, ServiceId, CallInfo, UCC, In, Domain?);

    Serve = illegal_request(_)  | Goals = _, Closed = _,
	remote_procedure_call(event(Serve), UCC, DRC, CallInfo, Scope, 
				computation
	);

    Serve = domain,
    Goals = Designated # Goal | Closed = _,
	remote_procedure_call(Goal, UCC, DRC, CallInfo, Scope, Designated);

    Serve = domain,
    Goals = Goals' @ LinkName,
    channel(DRC) | Closed = _,
	write_channel(domain_channel(Domain), DRC),
	link_goal(CallInfo, [self | Scope], Goals', UCC, Domain?, LinkName);

    Serve = domain,
    tuple(Goals), arg(1, Goals, Rpc), Rpc = Designated # Goal | Closed = _,
	copy_goal_args(Goals, Goal, Goal'),
	remote_procedure_call(Goal'?, UCC, DRC, CallInfo, Scope, Designated);

    Goals ? Goals'',
    listener(DRC),
    listener(CallInfo),
    listener(Scope),
    listener(Closed),
    listener(Serve) |
	self,
	domain_goals(Goals', UCC'?, DRC, CallInfo, Scope, Closed, Serve),
	splitCC(UCC, UCC', UCC'');

    Goals = clause(Goals', Body) |
	unify_without_failure(Body, true),
	domain_goals;

    Goals = clause(Goal, Body, Id, Result) |
	declause(Goal, Body, _(Id, Result), Goals'),
	domain_goals;

    Goals = [] | DRC = _, CallInfo = _, Scope = _, Closed = _, Serve = _,
	closeCC(UCC);

    Serve = domain,
    Goals = true | DRC = _, CallInfo = _, Scope = _, Closed = _,
	closeCC(UCC);

    Serve = domain,
    otherwise,
    listener(Scope) |
	Goals' = super # Goals,
	self,
	delegate_call(CallInfo, Scope, CallInfo');

    Serve = no_service(Id),
    Id =\= processor, Id =\= _@_,
    otherwise,
    UCC = {_, CL, CR, CC},
    channel(CC) | DRC = _, Scope = _, Closed = _,
	write_channel(request(CallInfo, failed(Id # Goals, no_service), CL, CR),
		      CC
	) ;

    Serve = no_service(processor),
    otherwise,
    UCC = {_, CL, CR, CC},
    channel(CC) | DRC = _, Scope = _, Closed = _,
	write_channel(request(CallInfo, failed(processor # Goals, discarded),
				 CL, CR
		      ),
		      CC
	) ;

    Serve = no_service(Id @ LinkName),
    otherwise,
    UCC = {_, CL, CR, CC},
    channel(CC) | DRC = _, Scope = _, Closed = _,
	write_channel(request(CallInfo, failed(Id # Goals @ LinkName, no_link),
				CL, CR
		      ),
		      CC
	) ;

    UCC = {[abort | _], Quit, Quit^, _} : Goals = _, DRC = _, CallInfo = _,
					  Scope = _, Closed = _, Serve = _ ;

    UCC = {CO, CL, CR, CC},
    CO ? suspend, known(CO') |
	UCC' = {CO', CL, CR, CC},
	domain_goals;

    UCC = {CO, CL, CR, CC},
    CO ? resume |
	UCC' = {CO', CL, CR, CC},
	domain_goals;

    UCC = {CO, CL, CR, CC},
    CL ? X |
	CR ! X,
	UCC' = {CO, CL', CR', CC},
	domain_goals;

    Closed = close : Scope = _ |
	residue(DRC, Serve, CallInfo, Goals),
	closeCC(UCC).

procedure delegate_call(CallInfo, Scope, CallInfo).

delegate_call(CallInfo1, Scope, CallInfo2) :-

    CallInfo1 = (_|_) | Scope = _,
	CallInfo2 = CallInfo1 ;

    otherwise |
	CallInfo2 = (CallInfo1 | Scope) .

procedure residue(DRC, Serve, CallInfo, Goals).

residue(DRC, Serve, CallInfo, Goals) :-
    Goals = [] | DRC = _, Serve = _, CallInfo = _ .	% Debugging aid.


procedure remote_procedure_call(Goal, TCC, DRC, CallInfo, Scope, Designated).

TCC ::= UCC ; abort.

remote_procedure_call(Goal, UCC, DRC, CallInfo, Scope, Designated) :-

    Designated = super,
    Scope = [_ | SuperScope],
    channel(DRC) |
	write_channel(find(SuperScope, SSC), DRC),
	write_channel(export(CallInfo, Scope, Goal, UCC), SSC?);

    Designated = self,
    listener(Scope),
    channel(DRC) |
	write_channel(find(Scope, SSC), DRC),
	write_channel(export(CallInfo, Scope, Goal, UCC), SSC?);

    Designated = computation,
    UCC = {CO, CL, CR, CC} : DRC = _, Scope = _ |
	serve_computation_goals(Goal, CL, CR, CO, CallInfo, CC);

    Designated = [computation | _],
    UCC = {CO, CL, CR, CC} : DRC = _, Scope = _ |
	serve_computation_goals(Goal, CL, CR, CO, CallInfo, CC);

    Designated = processor,
    channel(DRC) |
	write_channel(find([processor], PCH), DRC),
	write_channel(export(CallInfo, Scope, Goal, UCC), PCH?);

    Designated = [processor | _],
    channel(DRC) |
	write_channel(find(Designated, PCH), DRC),
	write_channel(export(CallInfo, Scope, Goal, UCC), PCH?);

    Designated = Designated' # B |
	Goal' = B # Goal,
	self;

    Designated =\= self, Designated =\= super,
    Designated =\= computation, Designated =\= processor,
    Designated =\= [computation | _], Designated =\= [processor | _],
    Designated =\= _#_,
    listener(Designated),
    listener(Scope),
    listener(Goal) |	% This is true in principle (only one Goal is used)
	valid_service_target(Designated, [Designated | Scope],
				Designated#Goal, Target
	),
	send_rpc_goal(Goal, UCC, DRC, CallInfo, Scope, Target?);

    otherwise,		% Designated = super, Scope = []
    known(Goal),
    UCC = {_, CL, CR, CC},
    channel(DRC) | Designated = _, Scope = _,
	write_channel(request(CallerId?, failed(CalledId? # Goal, unknown),
				CL, CR
		      ),
		      CC
	),
	failed_goal_call(CallInfo, CallerId, CalledId);

    unknown(Designated),
    UCC =\= abort,
    listener(Designated) |
	self,
	reply_when_known(Designated, UCC, UCC', Designated, Designated');

    Designated = {Test, _, _}, unknown(Test),
    listener(Designated) |
	self,
	reply_when_known(Test, UCC, UCC', Designated, Designated');

    UCC = abort | Goal = _, DRC = _, CallInfo = _, Scope = _, Designated = _ .

failed_goal_call(CallingId, CallerId, CalledId) :-

    CallingId = (CallerId^ | CalledId^) | true;

    otherwise |
	CallerId = CallingId,
	CalledId = [] .

procedure reply_when_known(Any, UCC, TCC, Any, Any).

reply_when_known(Test, UCC1, UCC2, Target1, Target2) :-

    unknown(Test),
    UCC1 = {CO, CL, CR, CC},
    CL ? X |
	CR ! X,
	UCC1' = {CO, CL', CR', CC},
	self;

    unknown(Test),
    UCC1 = {CO, CL, CR, CC},
    CO ? suspend, known(CO') |
	UCC1' = {CO', CL, CR, CC},
	self;

    unknown(Test),
    UCC1 = {CO, CL, CR, CC},
    CO ? resume |
	UCC1' = {CO', CL, CR, CC},
	self;

    UCC1 = {[abort | _], CL, CL^, _} | Test = _, Target1 = _, Target2 = _,
	UCC2 = abort;

    known(Test) |
	UCC2 = UCC1,
	Target2 = Target1 .


procedure valid_service_target(Designated, ServiceId, Request, Target).

valid_service_target(Designated, ServiceId, Request, Target) :-

    string(Designated) | Request = _,
	Target = ServiceId ;

    channel(Designated) | ServiceId = _, Request = _,
	Target = Designated ;

    otherwise,
    listener(Designated) | ServiceId = _,
	valid_id_target(Designated, Designated, Request, Target).

procedure valid_id_target(List, Id, Request, Target).

valid_id_target(List, Id, Request, Target) :-

    List ? String,
    string(String) |
	self;

    List = [] | Request = _,
	Target = Id;

    otherwise | List = _, Id = _,
	Target = illegal_request(Request) .

procedure send_rpc_goal(Goal, TCC, DRC, CallInfo, Scope, Target).

send_rpc_goal(Goal, UCC, DRC, CallInfo, Scope, Target) :-

    channel(Target),
    arity(Target) =:= 1 | DRC = _, CallInfo = _, Scope = _,
	write_channel(Goal, Target),	% This should be moved to the tell!
	closeCC(UCC);

    channel(Target),			% This clause only works if the
    arity(Target) =:= 1,		% write_channel above is moved to
    otherwise |				% tell (i.e. is atomic in the reduction).
	remote_procedure_call(event(blocked(Target#Goal)), UCC, DRC, CallInfo,
				Scope, computation
	);

    otherwise,
    channel(DRC) |
	write_channel(find(Target, SSC), DRC),
	write_channel(export(CallInfo, Scope, Goal, UCC), SSC?);

    unknown(Target),
    listener(Target),
    UCC =\= abort |
	self,
	reply_when_known(Target, UCC, UCC', Target, Target');

    UCC = abort | Goal = _, DRC = _, CallInfo = _, Scope = _, Target = _ .

/************************ A U X I L I A R Y **********************************/

procedure splitCC(CC1, CC2, CC3).

splitCC(CC1, CC2, CC3) :-
    CC1 = {CO, CL, CR, CC},
    listener(CO),
    listener(CC) |
	CC2 = {CO, CL, CM, CC},
	CC3 = {CO, CM?, CR, CC}.

procedure closeCC(CCC).

closeCC(CCC) :-
    CCC = {_, L, R, _} |
	R = L .

procedure link_goal(CallInfo, ServiceId, Goal, UCC, Domain, LinkName).

link_goal(CallInfo, ServiceId, Goal, UCC, Domain, LinkName) :-

    LinkName = self |
	transmit(CallInfo, ServiceId, Goal, UCC, Domain, LinkName);

    LinkName = self(Ok) |
	transmit(CallInfo, ServiceId, Goal, UCC, Domain, self),
	unify_without_failure(Ok, true);

    LinkName =\= self,
    ServiceId = [Name | Scope], Name =\= self,
    UCC = {_, _, _, CC},
    channel(CC) | Domain = _,
	write_channel(link(LinkName, ServiceId,	% refer to computation
			   [export(CallInfo, Scope, Goal, UCC)]
		      ),
		      CC) ;

    LinkName =\= self,
    ServiceId = [self | Scope],
    listener(Scope),
    UCC = {_, _, _, CC},
    channel(CC) | Domain = _,
	write_channel(link(LinkName, Scope,	% refer to computation
			   [export(CallInfo, Scope, Goal, UCC)]
		      ),
		      CC) .

/******************************************************************************

                      M O D U L E   S E R V E R S

 *****************************************************************************/

procedure activate_module(In, Module, ServiceId, Kind, Domain).

activate_module(In, Module, ServiceId, Kind, Domain) :-
    listener(Module),
    listener(ServiceId),
    listener(Domain) |
	test_activate(Module, export(attributes(As)), Arg2, Ok),
	module_variation(Ok?, As?, Arg2?, Attributes, ActivateControls),
	copy_listener(Attributes?, Attributes1, Attributes2),
	imports(Attributes1?, Imports),
	distributor(Imports?, ServiceId, Distributor, Domain),
	activated(In, Module, ServiceId, Kind, Domain, Attributes2?,
			Distributor?, ActivateControls?
	).

procedure module_variation(Ok, Attributes, Arg2, Attributes, ActivateControls).

module_variation(Ok, As, Arg2, Attributes, ActivateControls) :-

    Ok = true |
	Attributes = As, 
	ActivateControls = Arg2 ;

    Ok = false(Reason) | As = _, Arg2 = _,
	Attributes = [],
	ActivateControls = Reason .


procedure imports(Attributes, Imports).

imports(Attributes, Imports) :-

    Attributes ? import(Importing) | Attributes' = _,
	Imports = Importing ;

    Attributes ? Other,
    Other =\= import(_) |
	self;

    otherwise | Attributes = _,
	Imports = [] .


procedure distributor(Imports, ServiceId, Distributor, Domain).
procedure distributor(Imports, ServiceId, Distributor, Domain, Index, Tuple).

distributor(Imports, ServiceId, Distributor, Domain)
 + (Index = 0, VectorTuple, Tuple = VectorTuple?) :-

    Imports ? ServiceName, string(ServiceName),
    Index++,
    listener(ServiceId),
    listener(Domain),
    listener(Tuple) |
	arg(Index', Tuple, Stream),
	cache_import(Stream?, ServiceName, ServiceId, Domain),
	self;

    Imports ? link(self) |
	Imports'' = [self | Imports'],
	self;

    Imports ? link(LinkName), string(LinkName), LinkName =\= self,
    Index++,
    listener(ServiceId),
    listener(Tuple) |
	arg(Index', Tuple, Stream),
	cache_link(Stream?, LinkName, ServiceId),
	self;

    otherwise | Imports = _, ServiceId = _, Domain = _, Tuple = _,
	make_distributor(Index, Distributor, VectorTuple).

procedure make_distributor(Arity, Distributor, VectorTuple).

make_distributor(Arity, Distributor, VectorTuple) :-

    Arity = 0 | VectorTuple = _,
	Distributor = {[], _, _} ;

    Arity > 0 |
	make_vector(Arity, Vector, VectorTuple),
	copy_listener(Vector?, Vector1, Vector2),
	Distributor = {Vector1?, Arity, Close},
	close_DV(Close?, Vector2?).

procedure close_DV(Index, Vector).

close_DV(Index, Vector) :-

    Index-- > 0,
    vector(Vector) |
	close_vector(Index, Vector),
	self;

    Index =< 0 | Vector = _ .

procedure cache_import(Stream, Import, ServiceId, Domain).

cache_import(Stream, Import, ServiceId, Domain) :-

    Stream = [] | Import = _, ServiceId = _, Domain = _ ;

    list(Stream),
    Import = super,
    ServiceId = [Name | Scope], Name =\= self,
    channel(Domain) |
	write_channel(find(Scope, SSC), Domain, Domain'),
	serve_import(Stream, Import, ServiceId, Domain'?, SSC?);

    list(Stream),
    Import = super,
    ServiceId = [self, _ | Scope],
    channel(Domain) |
	write_channel(find(Scope, SSC), Domain, Domain'),
	serve_import(Stream, Import, ServiceId, Domain'?, SSC?);

    list(Stream),
    Import = self,
    ServiceId = [Name | _], Name =\= self,
    channel(Domain) |
	write_channel(find(ServiceId, SSC), Domain, Domain'),
	serve_import(Stream, Import, ServiceId, Domain'?, SSC?);

    list(Stream),
    Import = self,
    ServiceId = [self | Scope],
    channel(Domain) |
	write_channel(find(Scope, SSC), Domain, Domain'),
	serve_import(Stream, Import, ServiceId, Domain'?, SSC?);


    Import = computation | Domain = _,
	serve_import_computation(Stream, ServiceId);

    otherwise,
    list(Stream),
    ServiceId = [_ | Scope],
    listener(Import),
    channel(Domain) |
	write_channel(find([Import | Scope], SSC), Domain, Domain'),
	serve_import(Stream, Import, ServiceId, Domain'?, SSC?);

    otherwise,			% Domain has been closed - what to do ??
    listener(Domain) |
	SSC = Domain,
	serve_import.

procedure serve_import(Stream, Import, ServiceId, Domain, SSC).

serve_import(Stream, Import, ServiceId, Domain, SSC) :-

    Stream ? RPC,
    channel(SSC) |
	write_channel(RPC, SSC, SSC'),	% This should be moved to the tell!
	self;

    list(Stream),			% This clause only works if the
    channel(SSC),			% write_channel above is moved to
    otherwise,				% tell (i.e. is atomic in the reduction).
    SSC =\= Domain |		% SSC has been closed
	cache_import(Stream, Import, ServiceId, Domain);

    Stream = [] | Import = _, ServiceId = _, Domain = _, SSC = _ ;

    otherwise,
    Domain = SSC  | Stream = _, Import = _, ServiceId = _ .
		% Domain has been closed -
		%   perhaps close all calls instead?

procedure cache_link(Stream, LinkName, ServiceId).

cache_link(Stream, LinkName, ServiceId) :-

    list(Stream) |
	cached_links(Stream, LinkName, ServiceId,
		     [], [], [], [], _, _, _, _
	);

    Stream = [] | LinkName = _, ServiceId = _ .

cached_links(Stream, LinkName, ServiceId,
	     C4, C3, C2, C1, S4, S3, S2, S1
) :-

    Stream ? Export, Export = _(_, _, UCC),
    UCC = {_, _, _, C1} |
	S1 ! Export,
	self;

    Stream ? Export, Export = _(_, _, UCC),
    UCC = {_, _, _, C2} |
	S2 ! Export,
	cached_links(Stream', LinkName, ServiceId,
		    C4, C3, C1, C2, S4, S3, S1, S2'
	);

    Stream ? Export, Export = _(_, _, UCC),
    UCC = {_, _, _, C3} |
	S3 ! Export,
	cached_links(Stream', LinkName, ServiceId,
		    C4, C2, C1, C3, S4, S2, S1, S3'
	);

    Stream ? Export, Export = _(_, _, UCC),
    UCC = {_, _, _, C4} |
      S4 ! Export,
	cached_links(Stream', LinkName, ServiceId,
		    C3, C2, C1, C4, S3, S2, S1, S4'
	);

    Stream ? Export, Export = _(_CallInfo, Scope, _Goal, UCC),
    UCC = {_, _, _, Cn},
    Cn =\= C1, Cn =\= C2, Cn =\= C3, Cn =\= C4,
    listener(LinkName) |
	S4 = [],
	write_channel(link(LinkName, Scope, Sn?), Cn, Cn'),
	Sn ! Export,
	cached_links(Stream', LinkName, ServiceId,
		    C3, C2, C1, Cn'?, S3, S2, S1, Sn'
	);

    Stream = [] | LinkName = _, ServiceId = _,
	C1 = _,  C2 = _,  C3 = _,  C4 = _,
	S1 = [], S2 = [], S3 = [], S4 = [] .
    

procedure activated(In, Module, ServiceId, Kind, Domain, Attributes, 
			Distributor, ActivateControls
).

activated(In, Module, ServiceId, Kind, Domain, Attributes, 
			Distributor, ActivateControls
) :-

    ActivateControls = Monitor(Input?, ML, MR, Output!),
    Monitor =\= procedures,
    Monitor =\= meta_suspend,
    Distributor = Vector(Left, Right?),
    listener(ServiceId),
    listener(Domain) | Module = _,
	Kind = monitor(CCC),
	monitor(In, Domain, MR, done([]), Stop, Monitor, Attributes,
		ServiceId, Input!, Output!
	),
	close_IC(Stop?, Input!),
	output([system], ServiceId, Output?, CCC?, ML,
		Vector, Left?, Right, relay, Domain
	);

    ActivateControls = procedures(_, _, _) | Attributes = _,
	Kind = procedures,
	in_server(In, Module, failsafe(abort, relay), ServiceId,
			Domain, Distributor
	);

    ActivateControls = procedures(_, _, _, _) | Attributes = _,
	Kind = procedures,
	in_server(In, Module, interrupt(interrupt, serve), ServiceId,
			Domain, Distributor
	);

    ActivateControls = MK(_, _, _), MK = meta,
    Distributor = _(L, R) | Attributes = _,
	R = L,
	Kind = procedures,
	meta(In, Module, ServiceId, Domain, MK);

    ActivateControls = MK(_, _, _, _), MK = meta_suspend,
    Distributor = _(L, R) | Attributes = _,
	R = L,
	Kind = procedures,
	meta(In, Module, ServiceId, Domain, MK);

    string(ActivateControls),
    listener(ServiceId),
    channel(Domain) | Module = _, Attributes = _, Distributor = _,
	Kind = procedures,
	write_channel(failed(ServiceId, "ACTIVATE", ActivateControls),
			Domain, Domain'
	),
	no_server(In, ServiceId, Domain'?, _).

procedure close_IC(Stop, IC).

close_IC(Stop, IC) :-
    Stop = stop,
    channel(IC) |
	close_channel(IC).
	
/********************** M O N I T O R   S E R V E R **************************/

procedure monitor(In, Domain, ML, MR, Stop,  Monitor, Attributes,
			ServiceId, InChannel, OutChannel
).

monitor(In, Domain, ML, MR, Stop,  Monitor, Attributes,
	ServiceId, InChannel, OutChannel
) :-

    In ? _Functor(CallInfo, _Scope, Goals, SCC),
    listener(Domain),
    listener(Monitor),
    listener(Attributes),
    listener(ServiceId),
    listener(InChannel),
    SCC = {CO, CL, CR, CC},
    listener(CO),
    listener(CC) |
	copy_listener(Stop'?, Stop, Stop1),
	monitor_goals(Goals, Domain, ML'?, ML, Stop1?, Monitor, Attributes,
			ServiceId, InChannel,
			CallInfo, CO, CL, CR, CC
			
	),
	self;

    In ? filter(In'', OldIn) |
	OldIn = In',
	self;

    In = [] | Domain = _, Monitor = _, Attributes = _,
		ServiceId = _, InChannel = _, OutChannel = _,
	ML = MR,
	Stop = stop ;

    In ? Other,
    otherwise,
    channel(OutChannel),
    channel(Domain) |
	write_channel(exception(unknown, service_id(Id), ML'?, ML), OutChannel),
	write_channel(failed(Id?, Other, unrecognized), Domain),
	self.


procedure monitor_goals(Goals, Domain, ML, MR, Stop, Monitor, Attributes,
			ServiceId, InChannel,
			CallInfo, CO, CL, CR, CC
).

monitor_goals(Goals, Domain, ML, MR, Stop, Monitor, Attributes,
		ServiceId, InChannel,
		CallInfo, CO, CL, CR, CC
) :-

    Goals = attributes(Attributes^) | Stop = _, Monitor = _, Domain = _,
	ServiceId = _, InChannel = _,
	CallInfo = _, CO = _, CC = _,
	MR = ML,
	CR = CL ;

/******** Preserve the computation controls for delegation | failure. ********/

    Goals = service_id(_) | Stop = _, Monitor = _, Attributes = _,
	InChannel = _,
	MR = ML,
	unknown(CallInfo, ServiceId, Goals, {CO, CL, CR, CC}, Domain);

    Goals = "_unique_id"(_) | Stop = _, Monitor = _, Attributes = _,
	InChannel = _,
	MR = ML,
	unknown(CallInfo, ServiceId, Goals, {CO, CL, CR, CC}, Domain);

    Goals = "_close"(_,_) | Stop = _, Monitor = _, Attributes = _,
	InChannel = _,
	MR = ML,
	unknown(CallInfo, ServiceId, Goals, {CO, CL, CR, CC}, Domain);

    Goals = _#_ | Stop = _, Monitor = _, Attributes = _,
	InChannel = _,
	MR = ML,
	unknown(CallInfo, ServiceId, Goals, {CO, CL, CR, CC}, Domain);

/* This doesn't seem necessary, unless a local
   module with this ServiceId exists @LinkName. */
    Goals = Goal @ LinkName | Stop = _, Monitor = _, Attributes = _,
	InChannel = _,
	MR = ML,
	link_goal(CallInfo, ServiceId, Goal, {CO, CL, CR, CC}, Domain,
			LinkName
	);

/*****************************************************************************/

    Goals ? Goals1,
    listener(Domain),
    listener(Attributes),
    listener(CallInfo),
    listener(CO),
    listener(CC),
    listener(Stop),
    listener(Monitor),
    listener(InChannel),
    listener(ServiceId) |
	monitor_goals(Goals1, Domain, ML, ML', Stop, Monitor, Attributes,
			ServiceId, InChannel,
			CallInfo, CO, CL, CL', CC
	),
	self;

    Goals = clause(Goals', Body) |
	unify_without_failure(Body, true),
	self;

   Goals = clause(Goal, Body, Id, Result) |
	declause(Goal, Body, _(Id, Result), Goals'),
	self;

    Goals = true |
	Stop = _, Monitor = _, Attributes = _, Domain = _,
	ServiceId = _, InChannel = _,
	CallInfo = _, CO = _, CC = _,
	MR = ML,
	CR = CL ;

% kluge null call to common_interface so that it wont be elided by inherit ***
    Goals = false |
	Stop = _, Monitor = _, Attributes = _, Domain = _,
	ServiceId = _, InChannel = _,
	CallInfo = _, CC = _,
	MR = ML,
	CR = CL,
	common_interface(done, _, done([]), CO, []);

    Goals = [] |
	Stop = _, Monitor = _, Attributes = _, Domain = _,
	ServiceId = _, InChannel = _,
	CallInfo = _, CO = _, CC = _,
	MR = ML,
	CR = CL ;

    otherwise,
    Monitor = system,
    listener(CO),
    channel(InChannel) | Stop = _, Attributes = _,
	MR = ML,
	write_channel(GoalCommon?, InChannel),
	shared_common_interface(Goals, GoalCommon, Done, CO, COC!),
	output(CallInfo, ServiceId, COC?, {CO, CL, CR, CC}, Done?, [], [], [],
			relay, Domain
	);

    otherwise,
    Monitor = user,
    channel(InChannel) | Stop = _, Attributes = _, Domain = _,
	ServiceId = _,
	CallInfo = _, CO = _, CC = _,
	write_channel(Goals, InChannel),
	MR = ML,
	CR = CL ;

    CL ? MS |
	CR ! MS,
	self;

    CO ? abort | Goals = _,
	Stop = _, Monitor = _, Attributes = _, Domain = _,
	ServiceId = _, InChannel = _,
	CallInfo = _, CO' = _, CC = _,
	MR = ML,
	CL = CR ;

    CO ? suspend, known(CO') |
	self;

    CO ? resume |
	self;

    Stop = stop, ServiceId =\= [processor],
    listener(CallInfo),
    listener(CO),
    listener(CC),
    channel(Domain) | Monitor = _, Attributes = _, InChannel = _,
	MR = ML,
        write_channel(find(Id1?, SSC), Domain),
	unknown(CallInfo, ServiceId, service_id(Id),
		{CO, CL, CL', CC}, Domain
	),
	copy_listener(Id?, Id1, Id2),
	unify_without_failure(Id2?, [_ | Scope]),
	write_channel(export(CallInfo, Scope?, Goals, {CO, CL'?, CR, CC}), SSC?);

    Stop = stop, ServiceId = [processor] |
	Goals = _, Domain = _, Monitor = _, Attributes = _,
	InChannel = _, CallInfo = _, CO = _, CC = _,
	MR = ML,
	CR = CL .


procedure private_goals(Input, ServiceId, CallInfo, UCC, In, Domain).

private_goals(Input, ServiceId, CallInfo, UCC, In, Domain) :-

    Input ? goal(GoalInfo, Goal, CCC),
    CCC = {CO, _, _, _},
    listener(ServiceId),
    listener(Domain),
    listener(CO) |
	In ! GoalCommon?,
	shared_common_interface(Goal, GoalCommon, Done, CO, COC!),
	output(GoalInfo, ServiceId, COC?, CCC, Done?, [], [], [],
			relay, Domain
	),
	self;

    Input ? goal(GoalInfo, Goal, CCC),
    CCC = {CO, CL, CR, CC},
    writable(CO) |		% Temporary kluge - for non-dfcp ***
	Input'' = [goal(GoalInfo, Goal, {CO?, CL, CR, CC}) | Input'],
	self;

    Input = [] | ServiceId = _, CallInfo = _, Domain = _,
	In = [],
	closeCC(UCC);

    Input =\= [], Input =\= [_|_] |
	Input' = [Input],
	self;

    otherwise,
    Input ? Other,
    listener(CallInfo),
    listener(ServiceId),
    listener(Domain) |
	splitCC(UCC, UCC', UCC''),
	transmit(CallInfo, ServiceId, failed(Other, unknown), 
			UCC'?, Domain, computation
	),
	self;

    UCC = {[abort | _], Quit, Quit^, _} |
	Input = _, ServiceId = _, CallInfo = _, Domain = _,
	In = [] ;

    UCC = {CO, CL, CR, CC},
    CO ? suspend, known(CO') |
	UCC' = {CO', CL, CR, CC},
	self;

    UCC = {CO, CL, CR, CC},
    CO ? resume |
	UCC' = {CO', CL, CR, CC},
	self;

    UCC = {CO, CL, CR, CC},
    CL ? X |
	CR ! X,
	UCC' = {CO, CL', CR', CC},
	self.


procedure common_interface(Common, LO, RO, Signals, COC).

common_interface(Common, LO, RO, Signals, COC) :-

    Common = directed_interface(In, Out) |
	directed_interface;

    Common = computation(Request, Common'),
    channel(COC) |
	write_channel(transmit(computation, Request, LO'?, LO), COC, COC'),
	self;

    Common = context(Request, Common'),
    channel(COC) |
	write_channel(transmit(super, Request, LO'?, LO), COC, COC'),
	self;

    Common = super(Request, Common'),
    channel(COC) |
	write_channel(transmit(super, Request, LO'?, LO), COC, COC'),
	self;

    Common = self(Request, Common'),
    channel(COC) |
	write_channel(transmit(self, Request, LO'?, LO), COC, COC'),
	self;

    Common = processor(Request, Common'),
    channel(COC) |
	write_channel(transmit(processor, Request, LO'?, LO), COC, COC'),
	self;

    Common = exception(Status, Goal),
    channel(COC) | Signals = _,
	write_channel(exception(Status, Goal, RO, LO), COC);

    Common = fork(Common', Common''),
    listener(Signals),
    listener(COC) |
	common_interface(Common', LO, LO'?, Signals, COC),
	self;

    Common = reply(Ready, In, Out, Common'),
    listener(Signals),
    listener(COC) |
	shared_common_reply(Ready, LO, LO'?, Signals, COC, In, Out),
	self;

    Common = done | Signals = _, COC = _,
	LO = RO;

    otherwise,
    channel(COC) | Signals = _,
	write_channel(exception(invalid_command, Common, RO, LO), COC) ;

    Signals ? abort | Signals' = _, COC = _,
	Common = abort,
	LO = RO;

    Signals ? Other,
    Other =\= abort |
	self.

procedure common_reply(Ready, LO, RO, Signals, COC, In, Out).

/* moved to reserved_text - see LO = RO, In = Out */

directed_interface(In, LO, RO, Signals, COC, Out) :-

    In ? computation(Request),
    channel(COC) |
	write_channel(transmit(computation, Request, LO'?, LO), COC, COC'),
	self;

    In ? context(Request),
    channel(COC) |
	write_channel(transmit(super, Request, LO'?, LO), COC, COC'),
	self;

    In ? super(Request),
    channel(COC) |
	write_channel(transmit(super, Request, LO'?, LO), COC, COC'),
	self;

    In ? self(Request),
    channel(COC) |
	write_channel(transmit(self, Request, LO'?, LO), COC, COC'),
	self;

    In ? processor(Request),
    channel(COC) |
	write_channel(transmit(processor, Request, LO'?, LO), COC, COC'),
	self;

    In ? exception(Status, Goal),
    channel(COC) |
	write_channel(exception(Status, Goal, LO'?, LO), COC),
	self;

    In ? fork(In1, Out1),
    listener(Signals),
    listener(COC) |
	directed_interface(In1, LO, LO'?, Signals, COC, Out1),
	self;

    In ? reply(Ready, Left, Right),
    listener(Signals),
    listener(COC) |
	shared_common_reply(Ready, LO, LO'?, Signals, COC, Left, Right),
	self;

    In = [] | COC = _,
	LO = RO,
	Out = Signals;

    In =\= [], In =\= [_|_] |
	In' = [In],
	self;

    otherwise,
    channel(COC) |
	Out = Signals,
	write_channel(exception(invalid_input, In, RO, LO), COC) ;

    Signals ? abort |
	Out ! abort,
	self;

    Signals ? Other,
    Other =\= abort |
	self.


/*********************** L A Y E R   S E R V E R *****************************/

procedure in_server(In, Module, ServerMode, ServiceId, Domain, Distributor).

in_server(In, Module, ServerMode, ServiceId, Domain, Distributor) :-

    In ? Functor(CallInfo, _Scope, Goals, UCC),
    Distributor = Vector(Left, Right),
    UCC = {Signals, _, _, _},
    ServerMode = Mode(Alert, Circuit),
    listener(Module),
    listener(ServiceId),
    listener(Domain),
    listener(Vector) |
	Distributor' = Vector(Left'?, Right),
	computation_signals(Signals, Done1?, Alert, Interrupt),
	layer_goals(Goals, Done, done([]), Mode(Functor), Interrupt?,
			Module, CO!
	),
	copy_listener(Done?, Done1, Done2),
	output(CallInfo, ServiceId, CO?, UCC, Done2?, Vector, Left, Left',
			Circuit, Domain
	),
	self;

    In ? filter(In'', OldIn) |
	OldIn = In',
	self;

    otherwise,
    In ? Other,
    listener(ServiceId),
    channel(Domain) |
	write_channel(failed(ServiceId, Other, unrecognized), Domain),
	self;

    In = [],
    Distributor =  _(L,R) | Module = _, ServerMode = _,
			    ServiceId = _, Domain = _,
	R = L .

procedure computation_signals(Signals, Done, Alert, Interrupt).

computation_signals(Signals, Done, Alert, Interrupt) :-

    Signals ? abort | Signals' = _, Done = _, Alert = _,
	Interrupt = abort ;

    Signals ? suspend,
    Alert = interrupt | Signals' = _, Done = _,
	Interrupt = suspend ;

    Signals ? _,
    otherwise |
	self;

    Done = done(_) | Signals = _, Alert = _, Interrupt = _ .


procedure layer_goals(Goals, LO, RO, ModeFunctor, Interrupt, Module, CO).

layer_goals(Goals, LO, RO, ModeFunctor, Interrupt, Module, CO) :-

    Goals =\= [_|_],
    Goals =\= [],
    Goals =\= clause(_,_),
    Goals =\= clause(_,_,_,_),
    ModeFunctor = interrupt(Functor) |
	activate(Module, Functor(Goals), procedures(Interrupt?, LO, RO, CO)) ;

    Goals =\= [_|_],
    Goals =\= [],
    Goals =\= clause(_,_),
    Goals =\= clause(_,_,_,_),
    ModeFunctor = failsafe(_) | Interrupt = _,
	activate(Module, export(Goals), procedures(LO, RO, CO)) ;

    Goals ? Goal,
    listener(ModeFunctor),
    listener(Interrupt),
    listener(Module),
    listener(CO) |
	layer_goals(Goal, LO, LO'?, ModeFunctor, Interrupt, Module, CO),
	layer_goals;

    Goals = [] | ModeFunctor = _, Interrupt = _, Module = _, CO = _,
	LO = RO ;

    Interrupt = abort | Goals = _, ModeFunctor = _, Module = _, CO = _,
	LO = RO ;

    Interrupt = suspend,
    RO = Done(List) | ModeFunctor = _, Module = _, CO = _,
	LO = Done([[Goals] | List]) ;

    Goals = clause(Goals', Body) |
	unify_without_failure(Body, true),
	layer_goals;

    Goals = clause(Goal, Body, Id, Result) |
	declause(Goal, Body, _(Id, Result), Goals'),
	layer_goals.

/************************ M E T A   S E R V E R ******************************/

procedure meta(In, Module, ServiceId, Domain, MetaKind).

meta(In, Module, ServiceId, Domain, MK) :-

    In ? Functor(CallInfo, _Scope, Goals, UCC),
    UCC = {Signals, _, _, _},
    listener(Signals),
    listener(Module),
    listener(ServiceId),
    listener(Domain),
    listener(MK) |
	computation_signals(Signals, Done1?, interrupt, Interrupt),
	copy_listener(Done?, Done1, Done2),
	meta_goals(Goals, Done, done([]), Functor, Interrupt?,
			Module, CO!, MK
	),
	output(CallInfo, ServiceId, CO?, UCC, Done2?, [], [], [],
			serve, Domain
	),
	self;

    In ? filter(In'', In'^) |
	self;

    In = [] | Module = _, ServiceId = _, Domain = _, MK = _ ;

    otherwise,
    In ? Other,
    listener(ServiceId),
    channel(Domain) |
	write_channel(failed(ServiceId, Other, unrecognized), Domain, Domain'),
	self.


procedure meta_goals(Goals, LO, RO, Functor, Interrupt, Module, CO, MetaKind).

meta_goals(Goals, LO, RO, Functor, Interrupt, Module, CO, MK) :-

    Goals =\= clause(_,_),
    Goals =\= clause(_,_,_,_),
    Goals =\= [_|_],
    Goals =\= [],
    Goals =\= _#_,
    Goals =\= true,
    listener(Goals) |
	shared_activate_reduce(Module, Functor, Goals, MK, LO, RO,
				Interrupt, CO
	);

    Goals ? Goal,
    unknown(Interrupt),
    listener(Functor),
    listener(Interrupt),
    listener(Module),
    listener(CO),
    listener(MK) |
	meta_goals(Goal, LO, LO'?, Functor, Interrupt, Module, CO, MK),
	self;

    Goals = _#_,
    channel(CO) | Functor = _, Interrupt = _, Module = _, MK = _,
	write_channel(exception(unknown, Goals, LO, RO), CO) ;	% fast path

    Goals = clause(Goal, Body),
    Goal =\= "_close"(_),
    Goal =\= "_unique_id"(_),
    Goal =\= service_id(_),
    Goal =\= clause(_,_),
    Goal =\= clause(_,_,_,_),
    Goal =\= [_|_],
    Goal =\= [],
    Goal =\= true | Functor = _, CO = _,
	shared_activate_clause_MR(Module, Goal, Body, MK, LO, RO, Interrupt,
					_Result, Goals, _Id
	);

    Goals = clause(Goal, Body, Id?, Result),
    Goal =\= "_close"(_),
    Goal =\= "_unique_id"(_),
    Goal =\= service_id(_),
    Goal =\= clause(_,_),
    Goal =\= clause(_,_,_,_),
    Goal =\= [_|_],
    Goal =\= [],
    Goal =\= true | Functor = _, CO = _,
	shared_activate_clause_MR(Module, Goal, Body, MK, LO, RO, Interrupt,
					Result, Goals, Id
	);

    Goals = [] | Functor = _, Interrupt = _, Module = _, CO = _, MK = _,
	LO = RO ;

    Goals = true | Functor = _, Interrupt = _, Module = _, CO = _, MK = _,
	LO = RO ;

% kluge null calls clause/reduce so that they wont be elided by inherit ***
    Goals = false | Functor = _, Interrupt = _,
	reduce(true,LO,LO'?,_,[],[],_,Module,CO,MK),
	clause(_,LO',RO,0,[],_,none, _);

    tuple(Goals),
    unknown(Interrupt),
    otherwise,
    listener(MK) |
	declauser(Goals, Goals'),
	self;

    known(Interrupt),
    RO = Done(List) | Functor = _, Module = _, CO = _, MK = _,
	LO = Done([[Goals] | List]) .

procedure declauser(Clause, Clause).

declauser(DClause, Clause) :-

    DClause = clause(Clause^, Body) |
	unify_without_failure(Body, true);

    DClause = clause(IClause, Body, Id, Result) |
	declause(IClause, Body, _(Id, Result), Clause).

procedure declause(Goal, Body, Controls, Goals).

declause(Goal1, Body, Controls, Goal2) :-

    Controls = Suspense(Id, Result) |
	info(5, Time),
	Goal2 = Goal1,
	Id = 0,
	unify_without_failure(Body, true),
	unify_without_failure(Suspense, reduce),
	unify_without_failure(Result, Time?);

    Controls = _(Id, Result), string(Id) |
	Goal2 = true,
	unify_without_failure(Body, true),
	unify_without_failure(Result, failed(Goal1, aborted));

    otherwise | Controls = _,
	Goal2 = Goal1,
	unify_without_failure(Body, true).


procedure clause(Id, LO, RO, MetaResult, Interrupt, Result, Clause, Suspend).

clause(Id, LO, RO, MR, Interrupt, Result, Clause, Suspend) :-

    number(MR) | Id = _, Interrupt = _, Clause = _, Suspend = _,
	Result = MR,
	LO = RO ;

    string(MR),
    Clause = clause(_, Body) | Id = _, Interrupt = _, Result =  _, Suspend = _,
	LO = RO,
	unify_without_failure(Body, true);

    string(MR), Clause =\= clause(_, _) | Id = _, Interrupt = _, Suspend = _,
	info(5, T),
	arg(2, Clause, Goal),
	LO = RO,
	unify_without_failure(Result, MR(Goal?, T?));

    known(Interrupt),
    known(Suspend),
    RO = Done(List) | Clause = _, Id = _, Interrupt = _, Result = _,
	MR = abort,
	LO = Done([Suspend | List]);

    string(Id)  | Interrupt = _, Suspend = _,
	arg(2, Clause, Goal),
	MR = abort,
	unify_without_failure(LO, RO),
	unify_without_failure(Result, failed(Goal?, aborted)).


/******************** M E T A - I N T E R P R E T E R ************************/

procedure reduce(Goals, LO, RO, Result, Called, Suspend, Interrupt, Module, CO,
			MetaKind
).

reduce(Goals, LO, RO, Result, Called, Suspend, Interrupt, Module, CO, MK) :-

    Goals = (Goal, Goals'),
    Goal =\= true,
    Goal =\= _#_,
    Goal =\= _@_,
    listener(Goal),			% Called is never written in
    listener(Interrupt),
    listener(Module),
    listener(CO),
    listener(MK) | Interrupt = _,
	shared_activate_reduce(Module, reduce, Goal, MK, LO, LO'?,
				Interrupt, CO
	),
	self;

    unknown(Interrupt),
    Goals =\= true,
    Goals =\= _#_,
    Goals =\= _@_,
    Goals =\= (_,_) | Result = _, Called = _, Suspend = _,
	shared_activate_reduce(Module, reduce, Goals, MK, LO, RO,
				Interrupt, CO
	);

    Goals = (true, Goals') |
	self;

    Goals = (Target # Goal, Goals'),
    channel(CO) |
	write_channel(transmit(Target, Goal, LO, LO'?), CO, CO'),
	self;

    Goals = (Goal @ LinkName, Goals'),
    channel(CO) |
	write_channel(link(LinkName, Goal, LO, LO'?), CO, CO'),
	self;

    Goals = true | Result = _, Called = _, Interrupt = _,
		   Module = _, CO = _, MK = _, Suspend = _,
	unify_without_failure(LO, RO);

    Goals = Target # Goal,
    channel(CO) | Result = _, Called = _, Interrupt = _,
		  Module = _, MK = _, Suspend = _,
	write_channel(transmit(Target, Goal, LO, RO), CO) ;

    Goals = Goal @ LinkName,
    channel(CO) | Result = _, Called = _, Interrupt = _,
		  Module = _, MK = _, Suspend = _,
	write_channel(link(LinkName, Goal, LO, RO), CO) ;

    known(Interrupt),
    Goals =\= true,
    Goals =\= _#_,
    Goals =\= _@_,
    Goals =\= (_,_),
    RO = Done(List) |  Result = _, Called = _,
			Module = _, CO = _, MK = _, Suspend = _,
	LO = Done([Goals | List]) ;

    known(Interrupt),
    writable(Result),
    RO = Done(List) |  Called = _, Goals = _, Interrupt = _,
			Module = _, CO = _, MK = _,
	Result = abort,		% this must be atomic in the reduction!
	LO = Done([Suspend | List]) ;

    string(Result),
    channel(CO) | Goals = _, Interrupt = _, Module = _, MK = _, Suspend = _,
	write_channel(exception(Result, Called, LO, RO), CO) .

/************ C O M P U T A T I O N   C A L L   S E R V E R ******************/

procedure serve_import_computation(Stream, ServiceId).

serve_import_computation(Stream, ServiceId) :-

    Stream ? export(_, _, Goals, {CO, CL, CR, CC}),
    listener(ServiceId) |
	serve_computation_goals(Goals, CL, CR, CO, ServiceId, CC),
	self;

    Stream = [] | ServiceId =  _ .


procedure serve_computation_goals(Goals, CL, CR, CO, ServiceId, CC).

serve_computation_goals(Goals, CL, CR, CO, ServiceId, CC) :-

    Goals =\= [_|_], Goals =\= [], Goals =\= true,
    channel(CC) | CO = _,
	write_channel(request(ServiceId, Goals, CL, CR), CC) ;

    Goals ? Goal,
    listener(CO),
    listener(ServiceId),
    listener(CC) |
	serve_computation_goals(Goal, CL, CL', CO, ServiceId, CC),
	self;

    Goals = true | CO = _, ServiceId = _, CC = _,
	CR = CL ;

    Goals = [] | CO = _, ServiceId = _, CC = _,
	CR = CL ;

    CO ? abort | Goals = _, CO' = _, ServiceId = _, CC = _,
	CR = CL ;

    CO ? Other, Other =\= abort |
	self;

    CL ? X |
	CR ! X,
	self.

/*********************** O U T P U T   S E R V E R ***************************/

procedure output(CallInfo, ServiceId, Output, UCC, Done, Vector, Left, Right,
			Circuit, Domain
).

output(CallInfo, ServiceId, Output, UCC, Done, Vector, Left, Right,
		Circuit, Domain
) :-

    Output ? distribute(DX, Goal, DL, DR),
    ServiceId = [_ | Scope],
    UCC = {CO, CL, CR, CC},
    listener(CO),
    listener(CC),
    integer(DX),
    vector(Vector) |
	unify_without_failure(DL, DR),
	UCC' = {CO, CL, CM, CC},
	write_vector(DX, export(ServiceId, Scope, Goal, {CO, CM?, CR, CC}),
		     Vector, Vector'),
	self;

    Output ? transmit(Target, Goal, TL, TR),
    UCC = {CO, CL, CR, CC},
    listener(CO),
    listener(CC),
    listener(ServiceId),
    listener(Domain) |
	unify_without_failure(TL, TR),
	UCC' = {CO, CL, CM, CC},
	self,
	transmit(ServiceId, ServiceId, Goal, {CO, CM?, CR, CC}, Domain,
			Target?
	);

    Output ? link(LinkName, Goal, TL, TR),
    UCC = {CO, CL, CR, CC},
    listener(ServiceId),
    listener(Domain),
    listener(CO),
    listener(CC) |
	unify_without_failure(TL, TR),
	UCC' = {CO, CL, CM, CC},
	self,
	link_goal(ServiceId, ServiceId, Goal, {CO, CM?, CR, CC}, Domain,
			LinkName
	);

    Output ? exception(Reason, Goal, TL, TR),
    listener(CallInfo),
    listener(ServiceId),
    listener(Domain) |
	unify_without_failure(TL, TR),
	splitCC(UCC, UCC'', UCC'),
	self,
	exception(CallInfo, ServiceId, Goal, UCC'?, Domain, Reason);

    UCC = {CO, CL, CR, CC},
    CL ? X,
    Circuit = relay |
	CR ! X,
	UCC' = {CO, CL', CR', CC},
	self;

    Circuit = relay,
    UCC = {CO, CL, CR, CC},
    CO ? resume |
	UCC' = {CO', CL, CR, CC},
	self;

    Circuit = relay,
    UCC = {CO, CL, CR, CC},
    CO ? suspend, known(CO') |
	UCC' = {CO', CL, CR, CC},
	self;

    UCC = {[abort | _], CL, CL^, _},
    Circuit = relay | CallInfo = _, ServiceId = _, Output = _,
                      Done = _, Vector = _, Domain = _,
	unify_without_failure(Left, Right) ;

    Done = done(Suspended),
    unknown(Output) | Vector = _, Circuit = _,
	output_done(CallInfo, ServiceId, Suspended, UCC, Domain),
	unify_without_failure(Left, Right).


procedure exception(CallInfo, ServiceId, Goal, UCC, Domain, Reason).

exception(CallInfo, ServiceId, Goal, UCC, Domain, Reason) :-

    Reason =\= unknown,
    listener(ServiceId) |
	transmit(CallInfo, ServiceId, failed(ServiceId # Goal, Reason), 
			UCC, Domain, computation
	);

    Reason = unknown |
	unknown(CallInfo, ServiceId, Goal, UCC, Domain).


procedure unknown(CallInfo, ServiceId, Goal, UCC, Domain).

unknown(CallInfo, ServiceId, Goal, UCC, Domain) :-

    Goal = service_id(ServiceId^),
    ServiceId =\= [self | _] | CallInfo = _, Domain = _,
	closeCC(UCC);

    Goal = Target # Goal' |
	transmit(CallInfo, ServiceId, Goal', UCC, Domain, Target?);

    Goal = Goal' @ LinkName |
	link_goal(CallInfo, ServiceId, Goal', UCC, Domain, LinkName);

    Goal = true | CallInfo = _, ServiceId = _, Domain = _,
	closeCC(UCC);

    tuple(Goal), arg(1, Goal, Rpc), Rpc = Target # MetaVar |
	copy_goal_args(Goal, MetaVar, Goal'),
	transmit(CallInfo, ServiceId, Goal'?, UCC, Domain, Target?);

    otherwise |
	delegate(CallInfo, ServiceId, Goal, UCC, Domain).

procedure delegate(CallInfo, ServiceId, Goal, UCC, Domain).

delegate(CallInfo, ServiceId, Goal, UCC, Domain) :-

    CallInfo =\= (_|_), ServiceId = [self | Scope],
    listener(Scope),
    channel(Domain) |
	write_channel(export((CallInfo | Scope), Scope, Goal, UCC), Domain) ;

    CallInfo = (_|_), ServiceId = [self | Scope],
    channel(Domain) |
	write_channel(export(CallInfo, Scope, Goal, UCC), Domain) ;

    ServiceId = [Name | _], Name =\= self |
	transmit(CallInfo, ServiceId, failed(ServiceId # Goal, unknown),
			UCC, Domain, computation
	).


procedure transmit(CallInfo, ServiceId, Goal, TCC, Domain, Target).

transmit(CallInfo, ServiceId, Goal, UCC, Domain, Target) :-

    Target = computation,
    UCC = {CO, CL, CR, CC} | ServiceId = _, Domain = _,
	serve_computation_goals(Goal, CL, CR, CO, CallInfo, CC);

    Target = processor,
    channel(Domain) |
	write_channel(find([processor], PCH), Domain),
	write_channel(export(CallInfo, ServiceId, Goal, UCC), PCH?);

    Target = super,
    ServiceId ? Name, Name =\= self,
    listener(ServiceId'),
    channel(Domain) |
	write_channel(find(ServiceId', SSC), Domain),
	write_channel(export(CallInfo, ServiceId', Goal, UCC), SSC?);

    Target = self,
    ServiceId ? Name, Name =\= self,
    channel(Domain) |
	write_channel(find(ServiceId, SSC), Domain),
	write_channel(export(CallInfo, ServiceId', Goal, UCC), SSC?);

    Target = self, ServiceId ? self,
    listener(ServiceId'),
    channel(Domain) |
	write_channel(find(ServiceId', SSC), Domain),
	write_channel(export(CallInfo, ServiceId', Goal, UCC), SSC?);

    Target = Target' # Service |
	Goal' = Service # Goal,
	self;

    otherwise,
    ServiceId ? _,
    channel(Domain) |
	write_channel(export(CallInfo, ServiceId', Target # Goal, UCC), Domain) ;

    unknown(Target),
    UCC =\= abort,
    listener(Target) |
	transmit,
	reply_when_known(Target, UCC, UCC', Target, Target');

    Target = {Test, _, _},
    unknown(Test),
    listener(Test) |
	transmit,
	reply_when_known(Test, UCC, UCC', Target, Target');

    UCC = abort | CallInfo = _, ServiceId = _, Goal = _,
                  Domain = _, Target = _ .

/************ C O M P U T A T I O N   O U T P U T   D O N E ******************/

procedure output_done(CallInfo, ServiceId, Goals, UCC, Domain).

output_done(CallInfo, ServiceId, Goals, UCC, Domain) :-

    Goals =  [],
    UCC = {_, Left, Right, _} | CallInfo = _, ServiceId = _, Domain = _,
        Right = Left ;

    list(Goals),
    UCC = {CO, CL, CR, CC} |
	output_interrupt(CallInfo, ServiceId, Goals, CO, Domain, CL, CR, CC).


procedure output_interrupt(CallInfo, ServiceId, Goals, CO, Domain, CL, CR, CC).

output_interrupt(CallInfo, ServiceId, Goals, CO, Domain, CL, CR, CC) :-

    CO ? resume,
    ServiceId = [Name | Scope], Name =\= self,
    channel(Domain) |
	write_channel(find(ServiceId, SSC), Domain),
	write_channel(reduce(CallInfo, Scope, Goals, {CO', CL, CR, CC}), SSC?);

    CO ? resume,
    ServiceId = [self, Name | Scope],
    listener(Scope),
    channel(Domain) |
	write_channel(find([Name | Scope], SSC), Domain),
	write_channel(reduce(CallInfo, Scope, Goals, {CO', CL, CR, CC}), SSC?);

    CO ? abort | CallInfo = _, ServiceId = _, Goals = _, CO' = _, Domain = _,
		 CC = _,
	CR = CL ;

    CO ? suspend |
	output_suspend(CallInfo, ServiceId, Goals, CO', Domain, CL, CR, CC).

procedure output_suspend(CallInfo, ServiceId, Goals, CO, Domain, CL, CR, CC).

output_suspend(CallInfo, ServiceId, Goals, CO, Domain, CL, CR, CC) :-

    CL ? state(Ss),
    ServiceId =\= [self | _],
    listener(ServiceId),
    listener(Goals) |		% Not exactly true, but what can I do? ***
	Ss ! ServiceId # Goals,
	CR ! state(Ss'),
	self;

    CL ? state(Ss),
    ServiceId ? self,
    listener(ServiceId'),
    listener(Goals) |		% Not exactly true, but what can I do? ***
	Ss ! ServiceId' # Goals,
	CR ! state(Ss'),
	self;

    CL ? extract(N),
    listener(ServiceId) |
	self,
	output_extract(N, Goals, Goals', ServiceId, CR, CR'?);

    otherwise, CL ? X |
	CR ! X,
	self;

    known(CO) |
	output_interrupt(CallInfo, ServiceId, Goals, CO, Domain, CL, CR, CC).


procedure output_extract(N, Goals1, Goals2, ServiceId, CR1, CR2).

output_extract(N, Goals1, Goals2, ServiceId, CR1, CR2) :-

    Goals1 ? Goal,
    N-- =\= 1 |
	Goals2 ! Goal,
	self;

    Goals1 = [Goal | Goals2'],
    N = 1,
    ServiceId =\= [self | _] |
	Goals2 ! true,
	CR1 = [found(ServiceId # Goal) | CR2] ;

    Goals1 = [Goal | Goals2'],
    N = 1,
    ServiceId ? self |
	Goals2 ! true,
	CR1 = [found(ServiceId' # Goal) | CR2] ;
	
    Goals1 = [] | ServiceId = _,
	Goals2 = Goals1,
	CR1 = [extract(N) | CR2] .

/******************** P R O C E S S   D I C T I O N A R Y ********************/

/*
Manage a dynamic hash table of Processes, indexed by Key.

messages:

	find(Key, Status^)
	close(Residue^)

The hash-table grows and shrinks by powers of 2, in the range 8 .. 256,
whenever the number of entries is more then twice or less then half the
number of buckets.

*/

procedure process_dictionary(Commands, Closes).

process_dictionary(Commands, Closes) :-
	Closes = CloseC?,
	resume(Commands, CloseC!).

procedure resume(Commands, CloseC).
procedure resume(Commands, CloseC, Dim).

resume(Commands, CloseC) :-
    resume1(Commands, CloseC, 32).
resume1(Commands, CloseC, Dim) :-
    integer(Dim),
    listener(CloseC) |
	make_vector(Dim, Vector, Tuple),
	buckets(Dim, Tuple?, HashC!, CloseC),
	hasher(HashIn?, Dim),
	dictionary_server(Commands, CloseC, HashC?, HashIn, Vector?).

procedure buckets(Index, Tuple, HashC, CloseC).

buckets(Index, Tuple, HashC, CloseC) :-

    Index-- > 0,
    listener(Tuple),
    listener(HashC),
    listener(CloseC) |
	self,
	arg(Index, Tuple, In),
	bucket_end(In?, HashC, CloseC);

    Index = 0 | Tuple = _, HashC = _, CloseC = _ .


procedure dictionary_server(Commands, CloseC, Syncs, Hasher, Vector).

dictionary_server(Commands, CloseC, Syncs, Hasher, Vector) :-

    Commands ? find(Key, Reply),
    listener(Key) |
	Hasher ! hash(Key, 0, Index),
	self,
	find_key(Index?, Key, Reply, Vector, Vector');

    Commands ? services(Key, (List?)^) |
	self,
	services(Key, List, Vector, Vector');

    Commands ? rehash(Dim, Dim') | Syncs = _,
	Hasher = [close(_)],
	rehash(Dim, Vector, Continue, Commands'),
	resume1(Continue?, CloseC, Dim');

    Commands = [] | CloseC = _, Syncs = _,
	Hasher = [close(Dim)],
	close_all(Dim?, Vector);

    Syncs ? Sync,
    unknown(Commands) |
	Hasher ! Sync(Commands, Commands'),
	self.


procedure find_key(Index, Key, Reply, Vector1, Vector2).

find_key(Index, Key, Reply, Vector1, Vector2) :-

    Index > 0,
    vector(Vector1) |
	Vector2 = Vector1'?,
	write_vector(Index, find(Key, Reply), Vector1, Vector1') ;

    Index = 0 |
	Reply = invalid_key(Key),
	Vector2 = Vector1 .


procedure services(Key, List, Vector1, Vector2).
procedure services(Key, List, Vector1, Vector2, Index).

services(Key, List, Vector1, Vector2) + (Index = 1) :-

    vector(Vector1),
    Index++ =< arity(Vector1),
    listener(Key) |
	write_vector(Index, services(Key, List, List'?), Vector1, Vector1'),
	self;

    otherwise | Key = _, Index = _,
	List = [],
	Vector2 = Vector1 .


procedure hasher(HashIn, Dim).
procedure hasher(HashIn, Dim, Entries, Unit).

hasher(HashIn, Dim) + (Entries = 0, Unit = 1) :-

    HashIn ? hash([Name1, Name2, Name3 | More], Cumulant, Index),
    More =\= [],
    Cumulant += string_hash(Name1) + string_hash(Name2) + string_hash(Name3) |
	HashIn'' = [hash(More, Cumulant', Index) | HashIn'],
	self;

    HashIn ? hash([Name1, Name2, Name3], Cumulant, Index),
    Index^ := (string_hash(Name1) + string_hash(Name2) + string_hash(Name3) +
	       Cumulant)\Dim + 1 |
	self;

    HashIn ? hash([Name1, Name2], Cumulant, Index),
    Index^ := (string_hash(Name1) + string_hash(Name2) + Cumulant)\Dim + 1 |
	self;

    HashIn ? hash([Name], Cumulant, Index),
    Index^ := (string_hash(Name) + Cumulant)\Dim + 1 |
	self;

    HashIn ? hash([], 0, 1^) |
	self;

    HashIn ? hash(Name, Cumulant, Index),
    string(Name),
    Index^ := (string_hash(Name) + Cumulant)\Dim + 1 |
	self;

    otherwise,
    HashIn ? hash(_, _, 0^) |
	self;

    HashIn ? increment(Cs, [rehash(Dim, Dim') | Cs]^),
    Entries + Unit =:= 2*Dim, Dim < 256,
    Dim' := Entries + Unit |
	Unit' = 0,
	self;

    HashIn ? increment(Cs, Cs^),
    otherwise,
    Entries += Unit |
	self;

    HashIn ? decrement(Cs, [rehash(Dim, Dim') | Cs]^),
    Entries - Unit =:= Dim/2, Dim > 8,
    Dim' := Entries - Unit |
	Unit' = 0,
	self;

    HashIn ? decrement(Cs, Cs^),
    otherwise,
    Entries -= Unit |
	self;

    HashIn ? close(Dim^) | HashIn' = _, Entries = _, Unit = _ .


procedure rehash(Index, Vector, Continue, Commands).

rehash(Index, Vector, Continue, Commands) :-

    Index-- > 0,
    vector(Vector) |
	write_vector(Index, extract(Continue, Continue'?), Vector, Vector'),
	self;

    Index = 0 | Vector = _,
	Continue = Commands .

procedure close_all(Index, Vector).
procedure close_all(Index, Vector, Done).

close_all(Index, Vector) + (Done = done) :-

    known(Done),
    Index-- > 1,
    vector(Vector) |
	write_vector(Index, close(_, Done'), Vector, Vector'),
	self;

    known(Done),
    Index = 1,
    vector(Vector) |
	write_vector(Index, close(HashC, CloseC), Vector),
	close_channel(HashC?),
	close_channel(CloseC?).


procedure bucket_end(In, HashC, CloseC).

bucket_end(In, HashC, CloseC) :-

    In ? find(Key, not_found(Data, Proceed)^),
    listener(HashC),
    listener(CloseC) |
	node_stub(In', HashC, CloseC, Key, In'', Data?, Proceed?),
	self;

    In ? find(Key, not_found(Data, Proceed)),
    listener(HashC),
    listener(CloseC) |
	node_stub(In', HashC, CloseC, Key, In'', Data?, Proceed?),
	self;

    In ? find(Key, stub(Data, Proceed, Close, Hold, Tail)),
    listener(HashC),
    listener(CloseC) |
	node_stub1(In', HashC, CloseC, Key, In'', Data, Proceed,
			Close, Hold, Tail
	),
	self;

    In ? services(_, List1, List2) |
	List1 = List2,
	self;

    In ? extract(Continue^, Continue) | In' = _, HashC = _, CloseC = _ ;

    In ? close(HashC^, CloseC^) | In' = _ .


procedure node_stub(In, HashC, CloseC, Key, Out, Data, Proceed).
procedure node_stub1(In, HashC, CloseC, Key, Out, Data, Proceed,
		     Close, Hold, Tail
).

node_stub(In, HashC, CloseC, Key, Out, Data, Proceed) :-
	node_stub1(In, HashC, CloseC, Key, Out, Data, Proceed, _, Hold?, Hold).
node_stub1(In, HashC, CloseC, Key, Out, Data, Proceed, Close, Hold, Tail) :-

    In ? find(Key, Answer) |
	Tail ! find(Key, Answer),
	self;

    In ? find(OtherKey, Answer),
    Key =\= OtherKey |
	Out ! find(OtherKey, Answer),
	self;

    In ? services(DirKey, List1, List2),
    Key = [Name | DirKey] |
	List1 ! Name,
	Out ! services(DirKey, List1', List2),
	self;

    In ? extract([find(Key, stub(Data, Proceed, Close, Hold, Tail)) | Es1?]^,
		 Es2
	 ) | HashC = _, CloseC = _,
	Out ! extract(Es1, Es2),
	Out' = In' ;

    In = [close(_, _) | _] | HashC = _, CloseC = _,
				Key = _, Data = _, Close = _,
	Proceed = abort,
	Tail = In,
	Out = Hold ;

    In ? Other,
    otherwise |
	Out ! Other,
	self;

    Proceed = open(Close'),
    channel(HashC) | Close = _,
	write_channel(increment, HashC),
	Tail = In,
	node_process(Hold, HashC, CloseC, Key, Out, Data, Close');

    Proceed = replace(Close') | Close = _,
	Tail = In,
	node_process(Hold, HashC, CloseC, Key, Out, Data, Close');

    Proceed = proceed |
	Tail = In,
	node_process(Hold, HashC, CloseC, Key, Out, Data, Close);

    Proceed = close,
    channel(HashC),
    channel(CloseC) | Close = _,
	write_channel(decrement, HashC),
	write_channel(close(Key, Data), CloseC),
	Out = Hold,
	Tail = In ;

    Proceed = abort | HashC = _, CloseC = _, Key = _, Data = _, Close = _,
	Tail = In,
	Out = Hold .


procedure node_process(In, HashC, CloseC, Key, Out, Data, Close).

node_process(In, HashC, CloseC, Key, Out, Data, Close) :-

    In ? find(Key, found(Data, Data', Proceed)^) |
	node_stub1(In', HashC, CloseC, Key, Out, Data'?, Proceed?,
			Close, Hold?, Hold
	);

    In ? services(DirKey, List1, List2),
    Key = [Name | DirKey] |
	List1 ! Name,
	Out ! services(DirKey, List1', List2),
	self;

    In ? extract([find(Key, stub(Data, open(Close), _, Hold?, Hold)) | Es1?]^,
		 Es2
	 ) | HashC = _, CloseC = _,
	Out ! extract(Es1, Es2),
	Out' = In' ;

    In = [close(_, _) | _],
    channel(CloseC) | HashC = _, Close = _,
	write_channel(close(Key, Data), CloseC),
	Out = In ;

    In ? Other,
    otherwise |
	Out ! Other,
	self;

    Close = close,
    channel(CloseC),
    channel(HashC) |
	write_channel(close(Key, Data), CloseC),
	write_channel(decrement, HashC),
	Out = In.
