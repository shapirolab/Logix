/* $Header: /home/qiana/Repository/Logix/system/widgets/alias.cp,v 1.1 1999/07/09 07:03:22 bill Exp $ */
/*
   Create a local service which is a new activation of a prototype service

   alias # new(Path, Name, Reply^)

   Path designates the prototype service.
   Name is the local service name (in the computation's context).
   Reply is a SystemReply - true or false(Reason).

   For instance, to create a local input monitor service, with name "in",
   call:

	alias # new(input, in, _)

   Note that the local service may have the same name as the prototype, if
   they are in different contexts.

   Note that aliasing services is not transitive.  Reason in Reply may be
   "alias", indicating that Path designated a service which was not itself
   a prototype - i.e. it was created by a previous call to alias # new/3 .

   A directory of prototype services is maintained, to help conserve heap
   space by avoiding unnecessary duplication of binary modules
*/

-monitor(initial).
-mode(system).
-export([new/3]).
-language(compound).

Path ::= {`"#", Any, Path} ; Name.

procedure new(Path, Alias, SystemReply).


In ::= [Request].
Request ::= new(Path, Alias, SystemReply).

procedure initial(In).

initial(In) :-
	dynamic_server(In, []).

procedure select(Request, Directory, Directory, Common).

select(Request, Directory1, Directory2, Common) :-

    Request = new(Path, Alias, Reply),
    string(Alias) :
      Reply = Reply'?,
      Common = context(computation_utils # path_id(Path, SID, Result),
			Common'
		) |
	alias(Result, SID, Alias, Reply', Directory1, Directory2, Common');

    otherwise,
    unknown(Common) :
      Directory1 = Directory2 |
	unknown(Request, Common);

    known(Common) : Request = _,
      Directory1 = Directory2.

DirectoryEntry ::= entry(SID, String, Module).
Directory ::= [DirectoryEntry].

procedure alias(SystemReply, ServiceId, String, SystemReply,
		Directory, Directory, Common
).

alias(Result, SID, Alias, Reply, Directory1, Directory2, Common) :-

    Result = true,
    Directory1 ? Entry,
    Entry = entry(SID, UID, Module) :
      Directory2 = [Entry | Directory1'] |
	complete_request(Module, UID, Alias, Reply, Common);

    Result = false(_) : Alias = _, SID = _,
      Common = done,
      Reply = Result,
      Directory1 = Directory2 ;

    otherwise,
    Directory1 ? Other :
      Directory2 ! Other |
	alias;

    Directory1 = [],
    Result = true,
    SID ? Name :
      Common = context(SID' # "_unique_id"(DUID),
		  context(get_module#binary(SID', Name, ModuleResult), Common')
		) |
	add(ModuleResult, DUID, SID, Alias, Reply, Directory2, Common');

    known(Common) : Result = _, SID = _, Alias = _,
      Reply = false(aborted),
      Directory1 = Directory2.

Answer ::= Any.

ModuleResult ::= module(Module, String, Any) ; FailedResult.

EndDirectory ::= [DirectoryEntry | Nil] ; [].

procedure add(ModuleResult, String, ServiceId, String,
		SystemReply, EndDirectory, Common
).

add(Result, DUID, SID, Alias, Reply, EndDirectory, Common) :-

    Result = module(Module, NameBin, _) :
      EndDirectory = [entry(SID, SUID, Module)],
      Common = context(utils#append_strings([DUID, NameBin], SUID), Common') |
	complete_request(Module, SUID, Alias, Reply, Common');

    Result =\= module(_, _, _) : DUID = _, SID = _, Alias = _,
      EndDirectory = [] |
	get_failed(Result, Reply),
	unify_without_failure(Common, done);

    known(Common) : Result = _, DUID = _, SID = _, Alias = _,
      EndDirectory = [],
      Reply = false(aborted).


NG ::= false(Any).
FailedResult ::= import ; NG ; Any.

procedure get_failed(FailedResult, NG).

get_failed(Failed, Reply) :-
    Failed = import :
      Reply = false(alias);

    Failed = false(_) :
      Reply = Failed;

    otherwise :
      Reply = false(Failed).

procedure complete_request(Module, String, String, SystemReply, Common).

complete_request(Module, SUID, Alias, Reply, Common) :-

    true :
      AID ! Alias,
      Common = context(utils#append_strings([SUID, "=", Alias], AUID),
			computation([service_id(AID') | Add], Common')
		) |
	when(AID', AUID,
	     Add, "_domain"(add_service(AID, module(Module, AUID), Reply)),
	     Common'
	);

    known(Common) : Module = _, SUID = _, Alias = _,
      Reply = false(aborted).

procedure when(ServiceId, String, Any, Any, Common).

when(SID, String, Add, Request, Common) :-

    known(SID), string(String) :
      Common = done,
      Add = Request ;

    known(Common) : SID = _, String = _,
      Add = [] |
	unify_without_failure(Request, _(_, _, false(aborted))).
