/* $Header: /home/qiana/Repository/Logix/system/transform/dfcp.cp,v 1.1 1999/07/09 07:03:16 bill Exp $ */
-language([compound,colon]).
-export([clauses/3, clauses/4, transform/5]).
-mode(trust).


procedure transform(Any, Terms, Any, Terms, Errors).

transform(Attributes1, Source, Attributes2, Terms, Errors) :-
	attribute_exceptions,
	clauses1(Source, Terms, Exceptions?, Errors).


  attribute_exceptions(Attributes1, Attributes2, Exceptions) :-

    Attributes1 ? dfcp_exceptions(E) :
      Exceptions = E,
      Attributes2 = Attributes1' ;

    Attributes1 ? Other, Other =\= dfcp_exceptions(_) :
      Attributes2 ! Other |
	self;

    Attributes1 = [] :
      Attributes2 = [],
       Exceptions = none .

clauses(Source, Terms, Errors) + (Exceptions = none) :-
	clauses1(Source, Terms, Errors, Exceptions).

clauses1(Source, Terms, Exceptions, Errors)
 + (Procedure = "_"/'*'-0, PDict = [], BDict = PDictN?, PDictN) :-

    Source ? Clause, tuple(Clause), arg(1, Clause, Functor), string(Functor),
    Clause =\= (_ ::= _), Clause =\= procedure(_) :
      Terms ! Clause' |
	parse_clause(Clause, Head, Guard, Body),
	procedure_id(Head, Procedure, Procedure', PDict, PDict'),
	diagnostics(Procedure'?, Errors, Errors'?, RDiags?),
	requests(HeadRs?, Exceptions, Vars, Dictionary, RDiags),
	transform_head(Head?, Head', Assigns, GuardAssigns?),
	term(Head'?, Head'', HeadRs, AskRs?),
	transform_guard(Guard?, Exceptions, Asks, Asks'?,
			GuardAssigns, BodyAssigns?, AskRs, [vars | VarRs?]
	),
	guard_vars(Vars?, 1, Asks', BodyAsks?, Tells', Tells?,
			VarRs, VarRs'?),
	transform_body(Body, BodyAsks, BodyAssigns, BodyGoals,
			VarRs', BodyRs'?, BDict),
	term(BodyGoals?, Bodies, BodyRs', [assign | AssignRs?]),
	term(Assigns?, Tells, AssignRs, []),
	diagnostics(Procedure'?, Errors', Errors''?, ADiags?),
	make_clause(Head''?, Asks?, Tells'?, Bodies?, Dictionary?, Exceptions,
			Clause', ADiags
	),
	self;

    Source ? Declaration,
    otherwise :
      Terms ! Declaration |
	self;

    Source = [] : Exceptions = _, Procedure = _, BDict = _,
      Terms = [],
      Errors = [],
      PDictN = PDict .


diagnostics(PId, Errors1, Errors2, Diags) :-
	terminus(DDict),
	serve_diagnostics(Diags, DDict, collect(Ds, [])),
	filter_diagnostics(Ds, PId, Errors1, Errors2).

serve_diagnostics(Diags, DD, Collect) :-
    Diags ? Diagnostic :
      DD ! lookup(Diagnostic, _, _) |
	self;

    Diags = [] :
      DD = Collect .

filter_diagnostics(Ds, PId, E1, E2) :-

    Ds ? _(D) :
      E1 ! (PId - D) |
	self;

    Ds = [] : PId = _,
      E1 = E2 .


guard_vars(Vars, Index, Asks, BodyAsks, Tells1, Tells2, VarRs, EndVarRs) :-

    Vars ? var(VId, VO), VId =\= "_" :
      Asks ! var(VO),
      VarRs ! find(VId, Reply) |
	self,
	guard_var(Reply, VId, VO, Index, Index', Tells1, Tells1');

    Vars ? var("_", VO) :
      VO = `"_" |
	self;

    Vars ? (VId ! VO),
    Index++ :
      VO = `dfcp(Index),
      Asks ! var(VO),
      Tells1 ! (VO = `dfcp_outport(VId)) |
	self;

    Vars ? {VId??, VOId},
    Index++ :
      VOId = dfcp(Index),
      VO = `VOId,
      Asks ! var(VO),
      Tells1 ! (VO = (`VId)??) |
	self;

    Vars = [] : Index = _,
      Asks = BodyAsks,
      Tells1 = Tells2,
      VarRs = EndVarRs .

guard_var(Reply, VId, VO, Index1, Index2, Tells1, Tells2) :-

    Reply = "_ro" :	% Only one occurence in head - dont rename.
      Index2 = Index1,
      VO = `VId,
      Tells1 = Tells2 ;

    otherwise,		% Must be "both" in head - rename and unify.
    Index2^ := Index1 + 1 : Reply = _,
      VO = `dfcp(Index1),
      Tells1 = [(`VId = VO) | Tells2] .


parse_clause((Head :- Guard | Body), Head^, Guard^, Body^) :-
    Guard =\= (_ : _) | true.
parse_clause((Head :- Ask : Tell | Body), Head^, Ask^, (Tell, Body)^).
parse_clause((Head :- Body) , Head^, true^, Body^) :-
    Body =\= (_ | _), Body =\= (_ : _) |
	true.
parse_clause((Head :- Ask : Tell), Head^, Ask^, Tell^).
parse_clause(Head, Head^, true^, true^) :-
    Head =\= (_ :- _) |
	true.


procedure_id(Head, Procedure1, Procedure2, Dict1, Dict2) :-

    string(Head),
    Procedure1 = Head/0 - N++ :
      Procedure2 = Head/0 - N',
      Dict2 = Dict1 ;

    string(Head),
    otherwise : Procedure1 = _,
      Procedure2 = Head/0 - 1 |
	add_procedure(Head, 0, Dict1, Dict2);

    tuple(Head),
    A := arity(Head) - 1,
    arg(1, Head, Functor),
    Procedure1 = Functor/A - N++ :
      Procedure2 = Functor/A - N',
      Dict2 = Dict1 ;

    tuple(Head),
    A := arity(Head) -1,
    arg(1, Head, Functor), string(Functor),
    Procedure1 =\= Functor/A - _ :
      Procedure2 = Functor/A - 1 |
	add_procedure(Functor, A, Dict1, Dict2);

    otherwise,
    Procedure1 = Name/_ - N++ : Head = _,
      Procedure2 = Name/'*' - N',
      Dict2 = Dict1 .


transform_head(Head1, Head2, Assigns, MoreAssigns) :-

    N := arity(Head1),
    arg(1, Head1, A1), string(A1),
    make_tuple(N, Head),
    arg(1, Head, A1) :
      Head2 = Head |
	head_args(Head1, Head2, Assigns, MoreAssigns, 1, N);

    otherwise :
      Head1 = Head2,
      Assigns = MoreAssigns .

head_args(Head1, Head2, Assigns, MoreAssigns, I, N) :-

    N > 1,
    arg(N, Head1, Arg),
    Arg = `_,
    arg(N, Head2, Arg^),
    N' := N - 1 |
	self;

    N > 1,
    arg(N, Head1, Arg),
    Arg = ?_,
    arg(N, Head2, Arg^),
    N' := N - 1 |
	self;

    N > 1,
    arg(N, Head1, {Hat, Term}), Hat = "^",
    arg(N, Head2, (?dfcp_head(I))^),
    N' := N - 1,
    I' := I + 1 :
      Assigns ! `dfcp_head(I) = Term |
	self;

    N > 1,
    arg(N, Head1, Term),
    otherwise,
    arg(N, Head2, Term'),
    N' := N - 1 |
	head_term(Term, Term', dfcp_head, I, I', MoreAssigns', MoreAssigns),
	self;

    N = 1 : Head1 = _, Head2 = _, I = _,
      Assigns = MoreAssigns .

head_term(Term1, Term2, VarFunc, I1, I2, Assigns1, Assigns2) :-

    constant(Term1) : VarFunc = _,
      Term1 = Term2,
      I1 = I2,
      Assigns1 = Assigns2 ;

    Term1 ? Term :
      Term2 ! Term' |
	head_term(Term, Term', VarFunc, I1, I1', Assigns1, Assigns1'),
	self;

    Term1 = `_ : VarFunc = _,
      Term1 = Term2,
      I1 = I2,
      Assigns1 = Assigns2 ;

    Term1 = ?_ : VarFunc = _,
      Term1 = Term2,
      I1 = I2,
      Assigns1 = Assigns2 ;

    Term1 = {Hat, Term}, Hat = "^",
    I2^ := I1 + 1 :
      Term2 = ?VarFunc(I1),
      Assigns1 = [(`VarFunc(I1) = Term) | Assigns2] ;

    tuple(Term1),
    otherwise,
    N := arity(Term1),
    make_tuple(N, T) |
	head_tuple(Term1, Term2, VarFunc, I1, I2, Assigns1, Assigns2, T, N).

head_tuple(T1, T2, F, I1, I2, H1, H2, T, N) :-

    N > 0,
    N' := N - 1,
    arg(N, T1, A1),
    arg(N, T, (A2?)^) |
	head_term(A1, A2, F, I2', I2, H2', H2),
	self;

    N = 0 : T1 = _, F = _,
      T2 = T,
      I1 = I2,
      H1 = H2 .


transform_guard(Guard, Exceptions, Asks, Vars,
		Assigns, BodyAssigns, AskRs, EndAskRs
) :-
	partition_guard(Guard, Headers, Tests),
	headers(Headers, Asks, Guards, Assigns, BodyAssigns, AskRs, GuardRs),
	guard_tests(Tests, Exceptions, Vars, Guards, GuardRs, EndAskRs).

partition_guard(Guard, HEs, TGs) + (Ts = [], Hs = []) :-

    Guard = (Guard', More) |
	self,
	partition_guard(More, Hs', Ts', Ts, Hs);

    Guard = (`T1 = Term), T1 =\= "_", Term =\= `"_" :
      HEs = [Guard | Hs],
      TGs = Ts ;

    Guard = (Term = `T1), T1 =\= "_", Term =\= `"_" :
      HEs = [(`T1 = Term) | Hs],
      TGs = Ts ;

    Guard = (`VId ? Term), string(VId),
    Q := ascii("'"),
    string_to_dlist(VId, DL, [Q]) :
      HEs = [(`VId = [Term | `VId']) | Hs],
      TGs = Ts |
	list_to_string(DL, VId');

    Guard = (`VId := Expression) :
      HEs = [compute(VId, Expression) | Hs],
      TGs = Ts ;

    Guard = ({Hat, `VId} := Expression), Hat = '^' :
      HEs = [assign(VId, Expression) | Hs],
      TGs = Ts ;

    Guard = (_ =:= _) :
      HEs = [ground(expression, Guard) | Hs],
      TGs = Ts ;

    Guard = (_ > _) :
      HEs = [ground(expression, Guard) | Hs],
      TGs = Ts ;

    Guard = (_ < _) :
      HEs = [ground(expression, Guard) | Hs],
      TGs = Ts ;

    Guard = (_ >= _) :
      HEs = [ground(expression, Guard) | Hs],
      TGs = Ts ;

    Guard = (_ =< _) :
      HEs = [ground(expression, Guard) | Hs],
      TGs = Ts ;

    Guard = integer(`_) :
      HEs = [ground(ground, Guard) | Hs],
      TGs = Ts ;

    Guard = real(`_) :
      HEs = [ground(ground, Guard) | Hs],
      TGs = Ts ;

    Guard = number(`_) :
      HEs = [ground(ground, Guard) | Hs],
      TGs = Ts ;

    Guard = string(`_) :
      HEs = [ground(ground, Guard) | Hs],
      TGs = Ts ;

    Guard = constant(`_) :
      HEs = [ground(ground, Guard) | Hs],
      TGs = Ts ;

    Guard = vector(`_) :
      HEs = [ground(ground, Guard) | Hs],
      TGs = Ts ;

    Guard = channel(`_) :
      HEs = [ground(ground, Guard) | Hs],
      TGs = Ts ;

    Guard = ground(`_) :
      HEs = [ground(ground, Guard) | Hs],
      TGs = Ts ;

    Guard = arg(_Integer, `Tuple, `X),
    Tuple =\= '_', X =\= "_" :
      HEs = [Guard | Hs],
      TGs = Ts ;

    Guard = known(`X), X =\= "_" :
      TGs = [test(Guard) | Ts],
      HEs = Hs ;

    Guard = tuple(`X), X =\= "_" :
      TGs = [test(Guard) | Ts],
      HEs = Hs ;

    Guard = list(`X), X =\= "_" :
      TGs = [test(Guard) | Ts],
      HEs = Hs ;

    Guard = compound(`X), X =\= "_" :
      TGs = [test(Guard) | Ts],
      HEs = Hs ;

    Guard = writable(`X), X =\= "_" :
      TGs = [meta_test(Guard) | Ts],
      HEs = Hs ;

    Guard = read_only(`X), X =\= "_" :
      TGs = [test(Guard) | Ts],
      HEs = Hs ;

    Guard = unknown(`X), X =\= "_" :		% For ease of conversion
      TGs = [meta_test(Guard) | Ts],
      HEs = Hs ;

    Guard = listener(Var), Var = `X, X =\= "_" :
      HEs = Hs,
      TGs = [Guard | Ts] ;

    Guard = invalid(`X), X =\= "_" :
      TGs = [test(Guard) | Ts],
      HEs = Hs ;

    Guard = (`"_" = _) :
      HEs = Hs,
      TGs = [comparison(Guard) | Ts] ;

    Guard = (_ = `"_") :
      HEs = Hs,
      TGs = [comparison(Guard) | Ts] ;

    Guard = (A = B),
    A =\= `_, B =\= `_ :
      HEs = Hs,
      TGs = [comparison(Guard) | Ts] ;

    Guard = (X @< Y), X =\= `"_", Y =\= `"_" :
      TGs = [comparison(Guard) | Ts],
      HEs = Hs ;

    Guard = (X =\= Y), X =\= `"_", Y =\= `"_" :
      TGs = [comparison(Guard) | Ts],
      HEs = Hs ;

    Guard = true :
      TGs = Ts,
      HEs = Hs ;

    Guard = otherwise :
      TGs = [test(Guard) | Ts],
      HEs = Hs ;

    otherwise :
      HEs = Hs,
      TGs = [unknown(Guard) | Ts] .


headers(Headers, Asks, Guards, Assigns, BodyAssigns, AskRs, EndReadRs)
	+ (EndAskRs = Rs?, ReadRs = Rs,
	   I = 1, Iterate = false, Residue = Head, Recycle = Head) :-

    Headers ? Equal, Equal = (`VId = Term) :
      AskRs ! find(VId, Answer),
      Headers'' = [connect(VId, Term, Answer) | Headers'] |
	self;

    Headers ? connect(VId, {Hat, Term}, head_var), Hat = "^" :
      Asks ! var(`VId),
      Assigns ! (`VId = Term) |
	self;

/****** The following are all (extended) head variables ******/

    Headers ? connect(VId, Term, head_var),
    Term = {Functor, _}, Functor =\= "^" |
	connecteq;

    Headers ? connect(VId, Term, head_var),
    Term =\= {_, _} |
	connecteq;

    Headers ? connect(VId, Term, head_reader),
    Term = {Functor, _}, Functor =\= "^" |
	connecteq;

    Headers ? connect(VId, Term, head_reader),
    Term =\= {_, _} |
	connecteq;

    Headers ? connect(VId, Term, declared_listener),
    Term = {Functor, _}, Functor =\= "^" |
	connecteq;

    Headers ? connect(VId, Term, declared_listener),
    Term =\= {_, _} |
	connecteq;

    Headers ? connect(VId, Term, many),
    Term = {Functor, _}, Functor =\= "^" |
	connecteq;

    Headers ? connect(VId, Term, many),
    Term =\= {_, _} |
	connecteq;

    Headers ? connect(VId, Term, many(_)),
    Term = {Functor, _}, Functor =\= "^" |
	connecteq;

    Headers ? connect(VId, Term, many(_)),
    Term =\= {_, _} |
	connecteq;

/*************************************************************/

    Headers ? connect(VId, Term, ground) : Iterate = _,
      Asks ! (`VId = Term'),
      Iterate' = true |
	ground_term(test, Term, Term', AskRs, AskRs'),
	self;

    Headers ? connect(VId, Term, Other),
    Other =\= head_var, Other =\= ground,
    Other =\= many, Other =\= many(_),
    Other =\= head_reader, Other =\= declared_listener :
      Recycle ! (`VId = Term) |
	self;

    Headers ? Arg, Arg = arg(Integer, `VId, `AId) :
      AskRs ! find(VId, Answer),
      Headers'' = [connect_arg(Arg, AId, Connected?) | Headers'] |
	arg_arguments(Answer, Integer, AskRs', AskRs''?, Connected),
	self;

    Headers ? connect_arg(Arg, AId, true) :
      AskRs ! lookup(AId, Answer, Status) |
	valid_arg_var(Answer, Asks, Asks'?, AskRs', AskRs''?,
			Iterate, Iterate', `AId, Status, Arg
	),
	self;

    Headers ? connect_arg(Arg, _AId, false) :
      Recycle ! Arg |
	self;

    Headers ? assign(VId, Term),
    I++ :
      VId' = dfcp_copy(I),
      Headers'' = [compute(VId', Term) | Headers'],
      Asks ! var(`VId),
      Assigns ! (`VId = `VId') |
	self;

    Headers ? compute(VId, Term) : Iterate = _,
      AskRs ! ground(VId, Var),
      Asks ! Term',
      Iterate' = true |
	ground_term(expression, (Var := Term), Term', AskRs', AskRs''),
	self;

    Headers ? ground(test, Term) :
      Headers'' = [ground(Answer, Term) | Headers'] |
	groundable_term(Term, AskRs, AskRs', ground, Answer),
	self;

    Headers ? ground(unknown, Term) :
      Recycle ! ground(test, Term) |
	self;

    Headers ? ground(Kind, Term),
    Kind =\= test, Kind =\= unknown : Iterate = _,
      Iterate' = true,
      Asks ! Term' |
	ground_term(Kind, Term, Term', AskRs, AskRs'),
	self;

    Headers = [], Iterate = false : I = _,
      Asks = Guards,
      Assigns = BodyAssigns,
      AskRs ! guard,
      ReadRs = EndReadRs,
      Recycle = [] |
	unconnected_headers(Residue, AskRs', EndAskRs);

    Headers = [], Iterate = true :
      Recycle = [],
      Headers' = Residue,
      Iterate' = false,
      Recycle' = Residue' |
	self.

arg_arguments(Answer, Integer, AskRs1, AskRs2, Connected) :-

    Answer =\= new,
    integer(Integer) :
      AskRs1 = AskRs2,
      Connected = true ;

    Answer =\= new,
    Integer = `VId, VId =\= '_' :
      AskRs1 ! find(VId, Answer'),
      Integer' = 1 |
	self;

    otherwise : Answer = _, Integer = _,
      AskRs1 = AskRs2,
      Connected = false .

valid_arg_var(Answer, Asks1, Asks2, AskRs1, AskRs2, Iterate1, Iterate2,
		Var, Status, Arg
) :-

    Answer =\= ground, Answer =\= head_var, Answer =\= many, Answer =\= many(_),
    Answer =\= head_reader, Answer =\= declared_listener, Answer =\= new : Arg = _,
      Asks1 = Asks2,
      AskRs1 = [conflict(arg_variable-Var) | AskRs2],
      Iterate2 = Iterate1,
      Status = Answer ;

    Answer = ground : Var = _, Iterate1 = _,
      Asks1 = [Arg | Asks2],
      AskRs1 = AskRs2,
      Iterate2 = true,
      Status = Answer ;

    Answer = new : Iterate1 = _,
      Asks1 = [Arg, read_only(Var) | Asks2],
      AskRs1 = AskRs2,
      Iterate2 = true,
      Status = declared_listener ;

    otherwise : Answer = _, Iterate1 = _, Var = _,
      Asks1 = [Arg | Asks2] |
      AskRs1 = AskRs2,
      Iterate2 = true,
      Status = declared_listener .

    
connecteq(Headers,  Asks, Guards, Assigns, BodyAssigns, AskRs, EndReadRs,
	  EndAskRs, ReadRs, I, Iterate, Residue, Recycle, VId, Term
) :-
    true :
      Asks ! (`VId = Term'?),
      ReadRs ! lookup(VId, Answer, Status) |
	connect_term(Term, Term', Asks', Asks''?, I, I', Assigns, Assigns'?,
			Items, []
	),
	connections(Items, AskRs, [lookup(VId, HeadVar, Bound) | AskRs'],
			Connections
	),
	bind_connections(HeadVar, Connections, Bound, Iterate, Iterate'),
	headers,
	head_reader.

head_reader(Answer, Status) :-

    Answer = head_var :
      Status = head_reader ;

    Answer =\= head_var :
      Status = Answer .


groundable_term(Term, AskRs1, AskRs2, Last, Answer) :-

    constant(Term) :
      AskRs1 = AskRs2,
      Last = Answer ;

    Term ? Element |
	groundable_term(Element, AskRs1, AskRs1', Last, Last'),
	self;

    tuple(Term),
    Term =\= `_, Term =\= (`_)??, Term =\= (`_)!,
    A := arity(Term) |
	ground_head_tuple_args(Term, AskRs1, AskRs2, Last, Answer, 1, A);

    Term = `VId, VId =\= "_" :
      AskRs1 = [find(VId, Reply) | AskRs2] |
	ground_head_var(Reply, Last, Answer);

    Term = (`_)?? :
      AskRs1 = AskRs2,
      Last = Answer ;

    Term = (`_)! :
      AskRs1 = AskRs2,
      Last = Answer ;

    Term = `VId, VId = "_" :
      AskRs1 = AskRs2,
      Last = Answer .

ground_head_tuple_args(Tuple, AskRs1, AskRs2, Last, Answer, N, A) :-

    N < A,
    arg(N, Tuple, Arg),
    N' := N + 1 |
	self,
	groundable_term(Arg, AskRs1, AskRs1', Last, Last');

    N >= A,
    arg(N, Tuple, Arg) |
	groundable_term(Arg, AskRs1, AskRs2, Last, Answer).

ground_head_var(Reply, Last, Answer) :-

    Reply = head_var :
      Last = Answer ;

    Reply = ground :
      Last = Answer ;

    Reply = many :
      Last = Answer ;

    Reply = _(_) :
      Last = Answer ;

    otherwise : Reply = _, Last = _,
      Answer = unknown .


ground_term(Kind, Term1, Term2, AskRs1, AskRs2) :-

    constant(Term1) : Kind = _,
      Term1 = Term2,
      AskRs1 = AskRs2 ;

    tuple(Term1) |
	ground_tuple(Kind, Term1, Term2, AskRs1, AskRs2);

    Term1 ? Term :
      Term2 ! Term' |
	ground_term(Kind, Term, Term', AskRs1, AskRs1'),
	ground_term.

ground_tuple(Kind, Term1, Term2, AskRs1, AskRs2) :-

    Term1 = ?_ :
      Term2 = `"_",
      AskRs1 = [NA(Term1) | AskRs2] |
	not_allowed(Kind, NA);

    Term1 = {Hat, _}, Hat = "^" :
      Term2 = `"_",
      AskRs1 = [NA(Term1) | AskRs2] |
	not_allowed(Kind, NA);

    Term1 = `"_", Kind =\= test :
      Term2 = `"_",
      AskRs1 = [NA(Term1) | AskRs2] |
	not_allowed(Kind, NA);

    Term1 = `"_", Kind = test :
      Term2 = Term1,
      AskRs1 = AskRs2 ;

    Term1 = (`_)??, Kind = test :
      Term2 = Term1,
      AskRs1 = AskRs2 ;

    Term1 = (`_)!, Kind = test :
      Term2 = Term1,
      AskRs1 = AskRs2 ;

    Term1 = `VId,
    otherwise : Kind = _,
      AskRs1 = [ground(VId, Term2) | AskRs2] ;

    Term1 = arity(_), Kind = expression :
      Term2 = Term1,
      AskRs1 = AskRs2 ;

    otherwise,
    A := arity(Term1),
    make_tuple(A, Term2^) |
	ground_tuple_args(Kind, Term1, Term2, AskRs1, AskRs2, 1, A).

not_allowed(ground, not_ground^).
not_allowed(expression, not_allowed_in_expression^).
not_allowed(test, not_allowed_in_test^).


ground_tuple_args(Kind, Term1, Term2, AskRs1, AskRs2, I, A) :-

    I < A,
    arg(I, Term1, Arg),
    arg(I, Term2, Arg'),
    I' := I + 1 |
	self,
	ground_term(Kind, Arg, Arg', AskRs1, AskRs1');

    I >= A,
    arg(I, Term1, Arg),
    arg(I, Term2, Arg') |
	ground_term(Kind, Arg, Arg', AskRs1, AskRs2).

connect_term(Term1, Term2, Asks1, Asks2, I1, I2, Assigns1, Assigns2,
			Items, EndItems
) :-

    Term1 = `VId, VId =\= "_" :
      Asks1 = Asks2,
      I1 = I2,
      Assigns1 = Assigns2,
      Items = ["_var"(VId, Term2) | EndItems] ;

    Term1 = `"_" :
      Term2 = Term1,
      Asks1 = Asks2,
      I1 = I2,
      Assigns1 = Assigns2,
      Items = [variable | EndItems] ;

    Term1 = ?VId :
      Asks1 = Asks2,
      I1 = I2,
      Assigns1 = Assigns2,
      Items = ["_ro"(VId, Term2) | EndItems] ;

    Term1 = {Hat, Term}, Hat = "^",
    I2^ := I1 + 1 :
      Term2 = `dfcp_guard(I1),
      Asks1 = [var(Term2) | Asks2],
      Assigns1 = [(Term2 = Term) | Assigns2],
      Items = [variable | EndItems] ;

    Term1 = (`_)??,
    I2^ := I1 + 1 :
      Term2 = `dfcp_guard(I1),
      Asks1 = [var(Term2) | Asks2],
      Assigns1 = [(Term2 = Term1) | Assigns2],
      Items = [variable | EndItems] ;

    Term1 = (`_)!,
    I2^ := I1 + 1 :
      Term2 = `dfcp_guard(I1),
      Asks1 = [var(Term2) | Asks2],
      Assigns1 = [(Term2 = Term1) | Assigns2],
      Items = [variable | EndItems] ;

    tuple(Term1), A := arity(Term1), A =\= 2,
    make_tuple(A, Term2^) |
	connect_tuple(Term1, Term2, Asks1, Asks2, I1, I2, Assigns1, Assigns2,
			Items, EndItems, A
	);

    Term1 = {Arg1, _},
    Term1 =\= (`_)??, Term1 =\= (`_)!, Term1 =\= `_, Arg1 =\= "^",
    make_tuple(2, Term2^) |
	connect_tuple(Term1, Term2, Asks1, Asks2, I1, I2, Assigns1, Assigns2,
			Items, EndItems, 2
	);

    Term1 ? Car :
      Term2 ! Car' |
	connect_term(Car, Car', Asks1, Asks1', I1, I1', Assigns1, Assigns1',
			Items, Items'
	),
	self;

    otherwise :
      Term1 = Term2,
      Asks1 = Asks2,
      I1 = I2,
      Assigns1 = Assigns2,
      Items = EndItems .
    
connect_tuple(Term1, Term2, Asks1, Asks2, I1, I2, Assigns1, Assigns2,
			Items, EndItems, Arity
) :-

    Arity > 0,
    arg(Arity, Term1, Arg),
    arg(Arity, Term2, Arg'),
    Arity' := Arity - 1 |
	self,
	connect_term(Arg, Arg', Asks2', Asks2, I2', I2, Assigns2', Assigns2,
			EndItems', EndItems
	);

    Arity =< 0 : Term1 = _, Term2 = _,
      Asks1 = Asks2,
      I1 = I2,
      Assigns1 = Assigns2,
      Items = EndItems .

connections(Items, AskRs1, AskRs2, VIds) + (Summary = ground) :-

    Items ? variable : Summary = _,
      Summary' = variable |
	self;

    Items ? T, T = "_var"(VId, Answer) :
      AskRs1 ! T |
	ground_connection(Answer, VId, VIds, VIds', Summary, Summary'),
	self;

    Items ? T, T = "_ro"(VId, _) : Summary = _,
      AskRs1 ! T,
      VIds ! VId,      Summary' = variable |
	self;

    Items = [], Summary = ground :
      VIds = Summary,
      AskRs1 = AskRs2 ;

    Items = [], Summary = variable :
      VIds = Items,
      AskRs1 = AskRs2 .

ground_connection(Answer, VId, VIds, VIds', Summary, Summary') :-

    Answer = ground : VId = _,
      VIds = VIds',
      Summary' = Summary ;

    otherwise : Answer = _, Summary = _,
      VIds ! VId,
      Summary' = variable .

bind_connections(HeadVar, Connections, Bound, Iterate, ReIterate) :-

    Connections = ground : HeadVar = _, Iterate = _,
      Bound = Connections,
      ReIterate = true ;

    Connections = [] :
      Bound = HeadVar,
      ReIterate = Iterate ;

    list(Connections), HeadVar = head_var : Iterate = _,
      Bound = many(Connections),
      ReIterate = true ;

    list(Connections), HeadVar = many : Iterate = _,
      Bound = many(Connections),
      ReIterate = true ;

    list(Connections),
    otherwise :
      Bound = HeadVar,
      ReIterate = Iterate .
    
unconnected_headers(Residue, AskRs, EndAskRs) :-

    Residue = [] :
      AskRs = EndAskRs ;

    Residue ? (`XId = _) :
      AskRs ! "_var"(XId, _) |		% Diagnose "new" variable only
	self;

    Residue ? Other, Other =\= (_=_) |
	unconnected_headers,
	term(Other, _, AskRs, AskRs').	% Generate diagnostics for whole term


guard_tests(Tests, Exceptions, Vars, Guards, GuardRs, EndAskRs) :-

    Tests ? test(Test) : Exceptions = _,
      Guards ! Test' |
	self,
	term(Test, Test', GuardRs, GuardRs');

    Tests ? meta_test(Test) :
      Guards ! Test |
	self;

    Tests ? listener(Var),
    Var = `VId :
      GuardRs ! lookup(VId, Answer, Status?) |
	self,
	valid_listener(Answer?,	 Exceptions, Guards, Guards'?, GuardRs', GuardRs''?,
			Var, Status);

    Tests ? comparison(Comparison) :
      Guards ! Comparison' |
	self,
	term(Comparison, Comparison', GuardRs, GuardRs');

    Tests ? Unknown, Unknown = unknown(_) :
      GuardRs ! Unknown |
	self;

    Tests = [] : Exceptions = _,
      GuardRs = EndAskRs,
      Guards = Vars .

valid_listener(Answer, Exceptions, Gs1, Gs2, Rs1, Rs2, Var, Status) :-

    Answer =\= ground, Answer =\= head_var, Answer =\= many, Answer =\= many(_),
    Answer =\= head_reader, Answer =\= declared_listener : Exceptions = _,
      Gs1 = Gs2,
      Rs1 = [conflict(declared_listener-Var) | Rs2],
      Status = Answer ;

    Answer = ground : Var = _, Exceptions = _,
      Gs1 = Gs2,
      Rs1 = Rs2,
      Status = Answer ;

    Answer = head_var, Exceptions = none :
      Gs1 = [read_only(Var) | Gs2],
      Rs1 = Rs2,
      Status = declared_listener ;

    Answer = many, Exceptions = none :
      Gs1 = [read_only(Var) | Gs2],
      Rs1 = Rs2,
      Status = declared_listener ;

    Answer = many(_), Exceptions = none :
      Gs1 = [read_only(Var) | Gs2],
      Rs1 = Rs2,
      Status = declared_listener ;

    otherwise : Answer = _, Var = _, Exceptions = _,
      Gs1 = Gs2,
      Rs1 = Rs2,
      Status = declared_listener .



transform_body(Body, BodyAsks, Assigns, Bodies, 
		VarRs, EndListenRs, Dict
) :-
    true :
      EndAsks = [],
      EndAssigns = [],
      EndBodies = [],
      EndVarRs = [body | BodyRs?],
      EndBodyRs = ListenRs? |
	transform_body1.

transform_body1(Body, BodyAsks, EndAsks, Assigns, Bodies, 
		VarRs, EndVarRs, BodyRs, EndBodyRs,
		ListenRs, EndListenRs, EndAssigns, EndBodies, Dict
) :-

    Body = (Goal, Body') |
	transform_body1(Goal, BodyAsks, BodyAsks'?, Assigns, Bodies,
			VarRs, VarRs'?, BodyRs, BodyRs'?, 
			ListenRs, ListenRs'?, Assigns'?, Bodies'?, Dict
	),
	self;

    Body = `VId : Dict = _,
      Assigns = EndAssigns,
      VarRs = EndVarRs,
      ListenRs = EndListenRs,
      BodyRs ! find(VId, Answer) |
	legal_meta_call(Answer?, Body, VId, BodyAsks, EndAsks,
			Bodies, EndBodies, BodyRs', EndBodyRs
	);

    tuple(Body),
    arg(1, Body, `VId) : Dict = _,
      Assigns = EndAssigns,
      VarRs = EndVarRs,
      ListenRs = EndListenRs,
      BodyRs ! find(VId, Answer) |
	legal_meta_call(Answer?, Body, VId, BodyAsks, EndAsks,
			Bodies, EndBodies, BodyRs', EndBodyRs
	);

    Body = (`_ = `"_") : Dict = _,
      BodyAsks = EndAsks,
      Assigns = EndAssigns,
      VarRs = EndVarRs,
      Bodies = [Body | EndBodies],
      ListenRs = EndListenRs,
      BodyRs = EndBodyRs ;

    Body = (`VId = Term), Term =\= `"_" : Dict = _,
      Bodies = EndBodies,
      VarRs = EndVarRs,
      ListenRs = EndListenRs,
      BodyRs ! find(VId, Answer) |

	legal_assignment(Body, BodyAsks, EndAsks, Assigns, Answer,
			 BodyRs', EndBodyRs, EndAssigns, Body
	);

    Body =  (LHS = _), LHS =\= `_ : Dict = _,
      BodyAsks = EndAsks,
      Assigns = EndAssigns,
      Bodies = EndBodies,
      VarRs = EndVarRs,
      ListenRs = EndListenRs,
      BodyRs = [illegal_assignment(Body) | EndBodyRs] ;

    Body = "_$listenee"(Var), Var = `VId, VId =\= "_" : Dict = _,
      ListenRs ! lookup(VId, Answer, Status?),
      BodyAsks = EndAsks,
      Assigns = EndAssigns,
      Bodies = EndBodies,
      VarRs = EndVarRs,
      BodyRs = EndBodyRs |
	valid_listenee(Answer, ListenRs', EndListenRs, Var, Status);

    Body = true : Dict = _,
      BodyAsks = EndAsks,
      Assigns = EndAssigns,
      Bodies = EndBodies,
      VarRs = EndVarRs,
      ListenRs = EndListenRs,
      BodyRs = EndBodyRs ;

    Body = deschedule :
      BodyAsks = EndAsks,
      ListenRs = EndListenRs,
      BodyRs = EndBodyRs,
      Tests = [] |
	find_procedure(deschedule, 0, Dict, Reply),
	tell_call;

    Body = melt(F, M, V) :
      BodyAsks = EndAsks,
      ListenRs = EndListenRs,
      BodyRs = EndBodyRs,
      Tests = [ground(F), var(M), var(V)] |
	find_procedure(melt, 3, Dict, Reply),
	tell_call;

    Body = make_channel(C, O) :
      BodyAsks = EndAsks,
      ListenRs = EndListenRs,
      BodyRs = EndBodyRs,
      Tests = [ground(C), var(O)] |
	find_procedure(make_channel, 2, Dict, Reply),
	tell_call;

    Body = write_channel(_T, C) :
      BodyAsks = EndAsks,
      ListenRs = EndListenRs,
      BodyRs = EndBodyRs,
      Tests = [ground(C)] |
	find_procedure(write_channel, 2, Dict, Reply),
	tell_call;

    Body = write_channel(_T, C, C') :
      BodyAsks = EndAsks,
      ListenRs = EndListenRs,
      BodyRs = EndBodyRs,
      Tests = [ground(C), var(C')] |
	find_procedure(write_channel, 3, Dict, Reply),
	tell_call;

    Body = close_channel(C) :
      BodyAsks = EndAsks,
      ListenRs = EndListenRs,
      BodyRs = EndBodyRs,
      Tests = [ground(C)] |
	find_procedure(close_channel, 1, Dict, Reply),
	tell_call;

    Body = make_vector(N, C, O) :
      BodyAsks = EndAsks,
      ListenRs = EndListenRs,
      BodyRs = EndBodyRs,
      Tests = [ground(N), ground(C), var(O)] |
	find_procedure(make_vector, 3, Dict, Reply),
	tell_call;

    Body = write_vector(I, _T, V) :
      BodyAsks = EndAsks,
      ListenRs = EndListenRs,
      BodyRs = EndBodyRs,
      Tests = [ground(I), ground(V)] |
	find_procedure(write_vector, 2, Dict, Reply),
	tell_call;

    Body = write_vector(I, _T, V, V') :
      BodyAsks = EndAsks,
      ListenRs = EndListenRs,
      BodyRs = EndBodyRs,
      Tests = [ground(I), ground(V), var(V')] |
	find_procedure(write_vector, 3, Dict, Reply),
	tell_call;

    Body = close_vector(I, V) :
      BodyAsks = EndAsks,
      ListenRs = EndListenRs,
      BodyRs = EndBodyRs,
      Tests = [ground(I), ground(V)] |
	find_procedure(close_vector, 2, Dict, Reply),
	tell_call;

    Body = priority(_Old, _New) :
      BodyAsks = EndAsks,
      ListenRs = EndListenRs,
      BodyRs = EndBodyRs,
      Tests = [] |			% Suspend until executed (or failed)
	find_procedure(priority, 2, Dict, Reply),
	tell_call;

    Body = ttyput_byte(Byte) :
      BodyAsks = EndAsks,
      ListenRs = EndListenRs,
      BodyRs = EndBodyRs,
      Tests = [ground(Byte)] |
	find_procedure(ttyput_byte, 1, Dict, Reply),
	tell_call;

    Body = ttyput_string(String) :
      BodyAsks = EndAsks,
      ListenRs = EndListenRs,
      BodyRs = EndBodyRs,
      Tests = [ground(String)] |
	find_procedure(ttyput_string, 1, Dict, Reply),
	tell_call;

    Body = ttyput_integer(Integer) :
      BodyAsks = EndAsks,
      ListenRs = EndListenRs,
      BodyRs = EndBodyRs,
      Tests = [ground(Integer)] |
	find_procedure(ttyput_integer, 1, Dict, Reply),
	tell_call;

    Body = make_shared(ro, _Ro, Id) :
      BodyAsks = EndAsks,
      ListenRs = EndListenRs,
      BodyRs = EndBodyRs,
      Tests = [ground(Id)] |
	find_procedure(make_shared, 3, Dict, Reply),
	tell_call;

    Body = execute(Offset, Tuple),
/* Should also permit prior tuple/1 test or Tuple = {...} comparison, etc. */
    tuple(Tuple), Tuple =\= `_, Tuple =\= ?_ :
      BodyAsks = EndAsks,
      ListenRs = EndListenRs,
      BodyRs = EndBodyRs,
      Tests = [ground(Offset)] |
	find_procedure(make_shared, 2, Dict, Reply),
	tell_call;

    Body = debug(Part, Mode) :
      Assigns = EndAssigns,
      ListenRs = EndListenRs,
      Tests = [ground(Part), ground(Mode)] |
	find_procedure(debug, 2, Dict, Reply),
	ask_call;

    Body = nth_char(Integer, String, Char) :
      Assigns = EndAssigns,
      ListenRs = EndListenRs,
      Tests = [ground(Integer), ground(String), var(Char)] |
	find_procedure(nth_char, 3, Dict, Reply),
	ask_call;

    Body = make_tuple(Arity, Tuple) :
      Assigns = EndAssigns,
      ListenRs = EndListenRs,
      Tests = [ground(Arity), new(Tuple)] |
	find_procedure(make_tuple, 2, Dict, Reply),
	ask_call;

    Body = list_to_string(List, String) :
      Assigns = EndAssigns,
      ListenRs = EndListenRs,
      Tests = [ground(List), var(String)] |
	find_procedure(list_to_string, 2, Dict, Reply),
	ask_call;

    Body = string_to_dlist(String, DList, Term) :
      Assigns = EndAssigns,
      ListenRs = EndListenRs,
      Tests = [ground(String), var(DList)] |
	find_procedure(string_to_dlist, 3, Dict, Reply),
	movable_ask_term(Term, Reply?, Reply'),
	ask_call;

    Body = freeze(Term, Frozen, Vars) :
      Assigns = EndAssigns,
      ListenRs = EndListenRs,
      Tests = [var(Frozen), var(Vars)] |
	find_procedure(freeze, 3, Dict, Reply),
	movable_ask_term(Term, Reply?, Reply'),
	ask_call;

    Body = freeze(Term, Options, Frozen, Vars), Options =\= {_,_,_} :
      Assigns = EndAssigns,
      ListenRs = EndListenRs,
      Tests = [ground(Options), var(Frozen), var(Vars)] |
	find_procedure(freeze, 4, Dict, Reply),
	movable_ask_term(Term, Reply?, Reply'),
	ask_call;

    Body = freeze(Term, {Depth, Length, StringLength}, Frozen, Vars) :
      Assigns = EndAssigns,
      ListenRs = EndListenRs,
      Tests = [ground(Depth), ground(Length), ground(StringLength),
	       var(Frozen), var(Vars)] |
	find_procedure(freeze, 4, Dict, Reply),
	movable_ask_term(Term, Reply?, Reply'),
	ask_call;

    Body = dfreeze(Term, Frozen, Vars, Types) :
      Assigns = EndAssigns,
      ListenRs = EndListenRs,
      Tests = [var(Frozen), var(Vars), var(Types)] |
	find_procedure(dfreeze, 4, Dict, Reply),
	movable_ask_term(Term, Reply?, Reply'),
	ask_call;

    Body = dfreeze(Term, [], Frozen, Vars, Types) :
      Assigns = EndAssigns,
      ListenRs = EndListenRs,
      Tests = [var(Frozen), var(Vars), var(Types)] |
	find_procedure(dfreeze, 5, Dict, Reply),
	movable_ask_term(Term, Reply?, Reply'),
	ask_call;

    Body = dfreeze(Term, Options, Frozen, Vars, Types), Options = `_ :
      Assigns = EndAssigns,
      ListenRs = EndListenRs,
      Tests = [ground(Options), var(Frozen), var(Vars),
	       var(Types)] |
	find_procedure(dfreeze, 5, Dict, Reply),
	movable_ask_term(Term, Reply?, Reply'),
	ask_call;

    Body = dfreeze(Term, {Limit, Size}, Frozen, Vars, Types),
			Limit =\= "_var", Limit =\= "_ro",
			Limit =\= "??", Limit =\= "!":
      Assigns = EndAssigns,
      ListenRs = EndListenRs,
      Tests = [ground(Limit), ground(Size), var(Frozen), var(Vars),
	       var(Types)] |
	find_procedure(dfreeze, 5, Dict, Reply),
	movable_ask_term(Term, Reply?, Reply'),
	ask_call;

    otherwise : Dict = _,
      BodyAsks = EndAsks,
      Assigns = EndAssigns,
      Bodies = [Body | EndBodies],
      VarRs = EndVarRs,
      ListenRs = EndListenRs,
      BodyRs = EndBodyRs .

legal_meta_call(Answer, Body, VId, As1, As2, Bs1, Bs2, Rs1, Rs2) :-

    Answer = declared_listener : VId = _,
      As1 = As2,
      Bs1 = [Body | Bs2],
      Rs1 = Rs2 ;

    Answer = head_var :
      As1 = [read_only(`VId) | As2],
      Bs1 = [Body | Bs2],
      Rs1 = Rs2 ;

    otherwise : Answer = _, VId = _,
      As1 = As2,
      Bs1 = Bs2,
      Rs1 = [illegal_meta_call(Body) | Rs2] .


valid_listenee(Answer, Rs1, Rs2, Var, Status) :-

    Answer = new : Var = _,
      Status = listenee,
      Rs1 = Rs2 ;

    Answer =\= new :
      Rs1 = [conflict(listenee-Var) | Rs2],
      Status = Answer .

tell_call(Reply, Tests, Body, Assigns, EndAssigns,
	  Bodies, EndBodies, VarRs, EndVarRs
) :-

    Reply = false,
    Tests = [] :
      Assigns = [Body | EndAssigns],
      Bodies = EndBodies,
      VarRs = EndVarRs ;

    Reply = false,
    Tests ? ground(Constant),
    constant(Constant) |
	self;

    Reply = false,
    Tests ? ground(`VId) :
      VarRs ! find(VId, Answer) |
	not_ground_argument(Answer?, Reply'),
	self;

    Reply = false,
    Tests ? var(`VId) :
      VarRs ! find(VId, Answer) |
	not_writable_argument(Answer?, Reply'),
	self;

    otherwise : Reply = _, Tests = _,
      Assigns = EndAssigns,
      Bodies = [Body | EndBodies],
      VarRs = EndVarRs .


movable_ask_term(Term, Reply, Reply1) :-
	term(Term, _, Rqs, []),
	annotated_variable.

/*
** annotated_variable/3 - forbid annotated variable (V?,V??,V!) in ask call.
*/

annotated_variable(Rqs, Reply, Reply1) :-

    Rqs ? "_var"(_,_) |
	self;

    Rqs ? Other, Other =\= "_var"(_,_) : Rqs' = _, Reply = _,
      Reply1 = true;

    Rqs = [] :
      Reply1 = Reply;

    Reply =\= true : Rqs = _,
      Reply1 = Reply.

ask_call(Reply, Tests, Body, BodyAsks, EndAsks,
	  Bodies, EndBodies, VarRs, EndVarRs,
	  BodyRs, EndBodyRs
) :-

    Reply = false,
    Tests = [] :
      BodyAsks = [Body'? | EndAsks],
      Bodies = EndBodies,
      VarRs = EndVarRs |
	term(Body, Body', BodyRs, EndBodyRs);

    Reply = false,
    Tests ? ground(Constant),
    constant(Constant) |
	self;

    Reply = false,
    Tests ? ground(`VId) :
      VarRs ! find(VId, Answer) |
	not_ground_argument(Answer?, Reply'),
	self;

    Reply = false,
    Tests ? var(Var), Var = `VId :
      VarRs ! find(VId, Answer) |
	not_writable_argument(Answer?, Reply'),
	self;

    Reply = false,
    Tests ? new(`VId) :
      VarRs ! find(VId, Answer) |
	not_new_argument(Answer?, Reply'),
	self;

    otherwise : Reply = _, Tests = _,
      BodyAsks = EndAsks,
      Bodies = [Body | EndBodies],
      VarRs = EndVarRs,
      BodyRs = EndBodyRs .


not_ground_argument(Answer, Reply) :-

    Answer =\= ground, Answer =\= many, Answer =\= many(_) :
      Reply = true;

    otherwise : Answer = _,
      Reply = false.

not_writable_argument(Answer, Reply) :-

    Answer =\= "_ro", Answer =\= new :
      Reply = true;

    otherwise : Answer = _,
      Reply = false.

not_new_argument(Answer, Reply) :-

    Answer =\= new :
      Reply = true;

    otherwise : Answer = _,
      Reply = false.


legal_assignment(Body, BodyAsks, EndAsks, Assigns, Answer, BodyRs, EndBodyRs,
			EndAssigns, Assign
) :-

    Answer =\= "_ro", Answer =\= new, Answer =\= head_var : Assign = _,
      BodyAsks = EndAsks,
      BodyRs = [assign_conflict(Body) | EndBodyRs],
      Assigns = EndAssigns ;

    Answer = head_var,
    Body = (Var = _), Var = `VId : Assign = _,
      BodyAsks = [var(Var) | EndAsks],
      BodyRs = [lookup(VId, _head_var, "_ro") | EndBodyRs],
      Assigns = [Assign | EndAssigns] ;

    otherwise : Answer = _, Body = _,
      BodyAsks = EndAsks,
      BodyRs = EndBodyRs,
      Assigns = [Assign | EndAssigns] .

make_guard(Guards, Tells, Guard) :-

    list(Tells) :
      Guard = (AskPart : TellPart) |
	make_part(Guards, AskPart),
	make_part(Tells, TellPart);

    Guards = [], Tells = [] :
      Guard = true ;

    otherwise : Tells = _ |	% []
	make_part(Guards, Guard).


make_part(Goals, Part) :-

    Goals = [Goal] :
      Part = Goal ;

    Goals ? Goal,
    otherwise :
      Part = (Goal, Part') |
	make_part;

    Goals = [] :
      Part = true .


make_clause(Head, Asks, Tells, Bodies, Dictionary, Exceptions, Clause, Diags) :-

    Dictionary = {_, [], []} : Exceptions = _,
      Diags = [] |
	make_guard(Asks, Tells, Guard),
	make_part(Bodies, Body),
	make_clause(Head, Guard, Body, Clause);

    Dictionary = {Variables, Ports, Listeners},
    otherwise |
	term((Head, Asks), (Head', Asks'), Requests, [body | Requests']),
	term((Tells, Bodies), (Tells', Bodies'), Requests', []),
	terminus(Dict),
	make_readers(Listeners, Variables, Exceptions, Dict, Dict',
			Reads, Listens, Diags
	),
	make_ports(Ports, Dict', Dict'', Makes, Tells'),
	note_headers(Asks, Dict'', Dict'''),
	substitute(Requests, Exceptions, Dict''', Listens, Bodies'),
	make_guard(Asks', Makes, Guard),
	make_part(Reads, Body),
	make_clause(Head', Guard, Body, Clause).	

make_readers(Listeners, Variables, Exceptions, Dict1, Dict2,
		Reads, Listens, Diags
) :-

    Listeners ? _(Listener) :
      Dict1 ! lookup(Listener, _, Status?) |
	search_for(Listener, Variables, VStatus),
	make_reader(Listener, VStatus, Exceptions, Status, Reads, Reads',
			Diags, Diags'
	),
	self;

    Listeners = [] : Variables = _, Exceptions = _,
      Dict1 = Dict2,
      Reads = Listens,
      Diags = [] .

search_for(Listener, Variables, VStatus) :-

    Variables ? Status(Listener) : Variables' = _,
      VStatus = Status ;

    Variables ? _(Other), Other =\= Listener |
	self;

    Variables = [] : Listener = _,
      VStatus = new .


make_reader(Listener, VStatus, Exceptions, Status,
		Reads, Reads1, Diags, Diags1
) :-

    VStatus = new, Exceptions = none :
      Status = VStatus,
      Reads = Reads1,
      Diags = [listener(no_variable, Listener) | Diags1];

    VStatus = new, Exceptions =\= none : Listener = _,
      Status = VStatus,
      Reads = Reads1,
      Diags = Diags1;

    VStatus = passed, Exceptions = none :
      Status = new,
      Reads = Reads1,
      Diags = [listener(ambiguous, Listener) | Diags1];

    VStatus = passed, Exceptions =\= none : Listener = _,
      Status = VStatus,
      Reads = Reads1,
      Diags = Diags1;

    VStatus = passed_reader, Exceptions = none :
      Status = new,
      Reads = Reads1,
      Diags = [listener(ambiguous, Listener) | Diags1];

    VStatus = passed_reader, Exceptions =\= none : Listener = _,
      Status = VStatus,
      Reads = Reads1,
      Diags = Diags1;

    VStatus = head_var, Exceptions = none :
      Status = new,
      Reads = Reads1,
      Diags = [listener(no_reader, Listener) | Diags1];

    VStatus = head_var, Exceptions =\= none : Listener = _,
      Status = VStatus,
      Reads = Reads1,
      Diags = Diags1;

    VStatus = ground : Exceptions = _, Listener = _,
      Status = VStatus,
      Reads = Reads1,
      Diags = Diags1;

    VStatus = declared_listener : Exceptions = _, Listener = _,
      Status = VStatus,
      Reads = Reads1,
      Diags = Diags1;

    VStatus = assigned_listenee : Exceptions = _, Listener = _,
      Status = VStatus,
      Reads = Reads1,
      Diags = Diags1;

    VStatus = body_var : Exceptions = _, Listener = _,
      Status = copier(1),
      Reads = Reads1,
      Diags = Diags1;

    VStatus = outport : Exceptions = _, Listener = _,
      Status = copych(1),
      Reads = Reads1,
      Diags = Diags1;

    VStatus = assigned, Exceptions =\= none : Listener = _,
      Status = VStatus,
      Reads = Reads1,
      Diags = Diags1;

    VStatus = many, Exceptions =\= none : Listener = _,
      Status = VStatus,
      Reads = Reads1,
      Diags = Diags1;

    VStatus = many(_), Exceptions =\= none : Listener = _,
      Status = VStatus,
      Reads = Reads1,
      Diags = Diags1;

    otherwise : VStatus = _, Exceptions = _,
      Status = reader(1),
      Reads = [dfcp_copy(?Listener, `dfcp_read(Listener),
			 `dfcp_listen(Listener))
	      | Reads1],
      Diags = Diags1 .


make_ports(Ports, Dict1, Dict2, Makes, Tells) :-

    Ports ? Port :
      Dict1 ! lookup(Port, Answer, Status),
      Makes ! make_channel(`dfcp_outport(Port), `Inport) |
	make_port(Port, Answer, Status, Inport),
	self;

    Ports = [] :
     Dict1 = Dict2,
     Makes = Tells .

make_port(Port, Answer, Status, Inport) :-

    Answer = new :
      Status = outport,
      Inport = Port ;

    Answer = copych(_) :
      Status = Answer,
      Inport = Port ;

    Answer = Reader(N), Reader =\= copier, Reader =\= copych :
      Status = readch(N),
      Inport = dfcp_read(Port) .

note_headers(Asks, Dict1, Dict2) :-

    Asks ? var(`VId) :
      Dict1 ! lookup(VId, Answer, Header?) |
	note_header(Answer?, Header),
	self;

    Asks ? Ask, Ask =\= var(_) |
	self;

    Asks = [] :
      Dict1 = Dict2 .

note_header(Answer, Header) :-

    Answer = reader(N) :
      Header = header(N) ;

    otherwise :
      Header = Answer .


substitute(Requests, Exceptions, Dict, Listens, Bodies) + (Region = head) :-

    Requests ? "_ro"(VId, VO) :
      Dict ! lookup(VId, Answer, Answer?) |
	substitute_reader(VId, Answer, VO),
	self;

    Requests ? {(`VId)??, VO}, Region = head :
      Dict ! lookup(VId, Answer, Status) |
	substitute_listener(VId, Answer, Status, VO, Listens, Listens'),
	self;

    Requests ? {(`VId)??, VO}, Exceptions = none, Region = body :
      Dict ! lookup(VId, Answer, Status) |
	substitute_listener(VId, Answer, Status, VO, Listens, Listens'),
	self;

    Requests ? {(`VId)??, VO}, Exceptions =\= none :
      Dict ! lookup(VId, Answer, Status) |
	substitute_body_listener(VId, Answer, Status, VO, Listens, Listens'),
	self;

    Region = head,
    Requests ? "_var"(VId, VO) :
      Dict ! lookup(VId, Answer, Answer?) |
	substitute_header(VId, Answer, VO),
	self;

    Requests ? body : Region = _,
      Region' = body |
	self;

    Region = body,
    Requests ? "_var"(VId, VO) :
      VO = `VId |
	self;

    Requests = [] : Exceptions = _, Region = _,
      Dict = [],
      Listens = Bodies .

substitute_reader(VId, Answer, VO) :-

    Answer = reader(_Count) :
      VO = ?dfcp_read(VId) ;

    Answer = header(_Count) :
      VO = ?dfcp_read(VId) ;

    otherwise : Answer = _,		% new or outport
      VO = ?VId .

substitute_body_listener(VId, Answer, Status, VO, Listens, Listens') :-

    Answer =\= head_var, Answer =\= head_reader,
    Answer =\= passed, Answer =\= passed_reader,
    Answer =\= many, Answer =\= many(_),
    Answer =\= declared_listener, Answer =\= assigned |
	substitute_listener(VId, Answer, Status, VO, Listens, Listens');

    otherwise :			% Treat it as a readonly reference - Oren knows!
      Status = Answer,
      VO = ?VId,
      Listens = Listens' .


substitute_listener(VId, Answer, Status, VO, Listens, Listens') :-

    Answer = copier(N++) :
      VO = ?dfcp_listen(VId, N),
      Status = copier(N'),
      Listens ! dfcp_listen(?VId, `dfcp_listen(VId, N)) ;

    Answer = reader(N++) :
      VO = ?dfcp_listen(VId, N),
      Status = reader(N'),
      Listens ! dfcp_listen(?dfcp_listen(VId), `dfcp_listen(VId, N)) ;

    Answer = copych(N++) :
      VO = ?dfcp_listen(VId, N),
      Status = copych(N'),
      Listens ! dfcp_listen(?VId, `dfcp_listen(VId, N)) ;

    Answer = readch(N++) :
      VO = ?dfcp_listen(VId, N),
      Status = readch(N'),
      Listens ! dfcp_listen(?dfcp_listen(VId), `dfcp_listen(VId, N)) ;

    Answer = header(N++) :
      VO = ?dfcp_listen(VId, N),
      Status = header(N'),
      Listens ! dfcp_listen(?dfcp_listen(VId), `dfcp_listen(VId, N)) ;

    Answer = ground :
      VO = `VId,
      Status = Answer,
      Listens = Listens' ;

    Answer = declared_listener :
      VO = `VId,
      Status = Answer,
      Listens = Listens' ;

    Answer = assigned_listenee :
      VO = ?VId,
      Status = Answer,
      Listens = Listens' ;

    otherwise : VId = _,		% new, therefor bad!
      VO = (?'_'),
      Status = Answer,
      Listens = Listens' .

substitute_header(VId, Answer, VO) :-

    Answer = header(_Count) :
      VO = `dfcp_read(VId) ;

    otherwise : Answer = _,		% new
      VO = `VId .


make_clause(Head, true, true, Head^).
make_clause(Head, true, Body, (Head :- Body)^) :-
    Body =\= true |
	true.
make_clause(Head, Guard, Body, (Head :- Guard | Body)^) :-
    otherwise |
	true.


term(Term1, Term2, RQs, EndRQs) :-

    constant(Term1) :
      Term1 = Term2,
      RQs = EndRQs ;

    Term1 = `VId :
      RQs = ["_var"(VId, Term2) | EndRQs] ;

    Term1 = ?VId :
      RQs = ["_ro"(VId, Term2) | EndRQs] ;

    Term1 = ((`VId)!), VId =\= "_" :
      RQs = [(VId ! Term2) | EndRQs] ;

    Term1 = ((`VId)??), VId =\= "_" :
      RQs = [{Term1, Term2} | EndRQs] ;

    Term1 ? Term :
      Term2 ! Term' |
	term(Term, Term', RQs, RQs'),
	self;

    tuple(Term1),
    otherwise,
    A := arity(Term1),
    make_tuple(A, Term2^) |
	untuple(Term1, Term2, RQs, EndRQs, A).


untuple(T1, T2, RQs1, RQs2, A) :-

    A > 0,
    arg(A, T1, Term),
    arg(A, T2, Term'),
    A' := A - 1 |
	term(Term, Term', RQs2', RQs2),
	self;

    A = 0 : T1 = _, T2 = _,
      RQs1 = RQs2 .


requests(Requests, Exceptions, Vars, Dictionary, Diags) :-
    true :
      Dictionary = {_, _, _},
      Part = head |
	terminus(Dict?),
	terminus(DictL?),
	serve_requests.

serve_requests(Requests, Exceptions, Vars, Dictionary, Diags, Part,
		Dict, DictL
) :-

    Requests ? String, string(String), String =\= vars : Part = _,
      Part' = String |
	self;

    Requests ? find(VId, Answer) :
      Dict ! find(VId, Answer, Answer) |
	self;

    Requests ? Lookup, Lookup = lookup(_, _, _) :
      Dict ! Lookup  |
	self;

    Requests ? "_var"("_", Reply) :
      Reply = `"_" |
	self;

    Requests ? "_ro"(VId, VO), Part = head :
      Dict ! lookup(VId, Answer, Status),
      Vars ! var(VOId, VO) |
	head_result(Answer, VId, VOId, Status, Diags, Diags'),
	self;

    Requests ? ground(VId, Reply),
    Part = head :
      Dict ! lookup(VId, Answer, Status) |
	ground_result(Answer, Exceptions, Requests'', Requests',
			Status, Result),
        diagnose(Result, ground(VId), Reply, Diags, Diags'),
	self;

    Requests ? {Listen, (`VOId)^}, Listen = (`VId)??,
    Part = head :
      Vars ! {VId??, VOId},
      DictL ! lookup(VId, Answer, Answer?) |
	self;

    Requests ? {Listen, Listen^}, Listen = (`VId)??,
    Part =\= head :
      DictL ! lookup(VId, Answer, Answer?) |
	self;

    Requests ? (VId ! VO),
    Part = head,
    Dictionary = {Vs, Ps, Ls} :
      Vars ! (VId ! VO),
      Dictionary' = {Vs, Ps', Ls},
      Dict ! lookup(VId, Answer, Status) |
	outport_result(Answer, Part, VId, Status, Ps, Ps', Diags, Diags'),
	self;

    Requests ? (VId ! Outport),
    Part =\= head,
    Dictionary = {Vs, Ps, Ls} :
      Outport = `dfcp_outport(VId),
      Dictionary' = {Vs, Ps', Ls},
      Dict ! lookup(VId, Answer, Status) |
	outport_result(Answer, Part, VId, Status, Ps, Ps', Diags, Diags'),
	self;

    Requests ? Functor(VId, Reply),
    otherwise :
      Dict ! lookup(VId, Answer, Status) |
	self,
	result(Answer, Exceptions, Part, Functor, Status, Result),
        diagnose(Result, Functor(VId), Reply, Diags, Diags');

    Requests ? Diagnostic(Arg), Diagnostic =\= (`_)?? :
      Diags ! Part(Diagnostic, Arg) |
	self;

    Requests ? vars : Vars' = _,
      Vars = [] |
	self;

    Requests = [] : Exceptions = _, Part = _,
      Dictionary = {Vs', [], Ls},
      Vars = [],
      Dict = collect(Vs, []),
      DictL = collect(Ls, []),
      Diags = [] |
	residual_variables(Vs, Vs').

residual_variables(Vs1, Vs2) :-

    Vs1 ? Other, Other =\= many(_), Other =\= {many(_), _} :
      Vs2 ! Other |
	self;

    Vs1 ? many(Id) :
      Vs2 ! ground(Id) |
	self;

    Vs1 ? {many(_), Id} :
      Vs2 ! ground(Id) |
	self;

    Vs1 = [] :
      Vs2 = [] .

head_result(Answer, VId, VOId, Status, Diags1, Diags2) :-

    Answer = new :
      VOId = VId,
      Status = "_ro",
      Diags1 = Diags2 ;

    Answer = head_var :
      VOId = VId,
      Status = both,
      Diags1 = Diags2 ;

    Answer = head_reader :
      VOId = VId,
      Status = both,
      Diags1 = Diags2 ;

    otherwise :		% "_ro" or both or ground or bound or
      VOId = "_",
      Status = Answer,
      Diags1 = [head(conflict, ?VId) | Diags2] .

ground_result(Answer, Exceptions, Requests2, Requests1, Status, Result) :-

    Answer = _(Connections) : Exceptions = _,
      Status = ground,
      Result = ok |
	ground_connections(Connections, Requests2, Requests1);

    Answer =\= _(_) :
      Requests1 = Requests2 |
	result(Answer, Exceptions, head, ground, Status, Result).

  ground_connections(Connections, Grounds, Requests) :-

    Connections ? VId :
      Grounds ! ground(VId, _) |
	self;

    Connections = [] :
      Grounds = Requests .


outport_result(Answer, Part, VId, Status, Ports, Ports', Diags, Diags') :-

    Answer = new : Part = _,
      Ports ! VId,
      Status = outport,
      Diags = Diags' ;

    Answer = "_ro" : Part = _,
      Ports ! VId,
      Status = ports,
      Diags = Diags' ;

    Answer = outport : VId = _, Part = _,
      Ports = Ports',
      Status = Answer,
      Diags = Diags' ;

    Answer = ports : VId = _, Part = _,
      Ports = Ports',
      Status = Answer,
      Diags = Diags' ;

    otherwise : VId = _,
      Ports = Ports',
      Status = Answer,
      Diags ! Part(invalid, (`VId)!) .


result(Answer, Exceptions, Part, Functor, Status, Result) :-

    Functor = "_var", Answer =\= ground : Exceptions = _ |
	writable_functor(Answer, Part, Status, Result);

    Functor = "_ro",
    Part =\= guard,			% cant be "_ro" in (extended) head,
    Exceptions =\= none |		% especially now!
	body_ro_exceptions;

    Functor = "_ro",
    Part =\= guard,
    Exceptions = none |
	readonly_functor;

    Functor = ground, Answer =\= ground : Part = _, Exceptions = _ |
	ground_functor;

    Answer = ground |
      Status = ground,
	ground_answer;

    Functor = "_ro", Part = guard : Exceptions = _ |
      Status = Answer,
      Result = not_allowed(guard) .

  body_ro_exceptions(Answer, Status, Result) :-

    Answer =\= head_var, Answer =\= head_reader,
    Answer =\= passed, Answer =\= passed_reader,
    Answer =\= many, Answer =\= many(_),
    Answer =\= declared_listener, Answer =\= assigned :
      Part = body |
	readonly_functor;

    otherwise : Answer = _,		% one of the above!
      Status = Answer,
      Result = ok.

  ground_answer(Part, Exceptions, Functor, Result) :-

    Exceptions = none, Functor = "_ro" :
      Result = Part(conflict);

    otherwise : Part = _, Exceptions = _, Functor = _,
      Result = ok .

  writable_functor(Answer, Part, Status, Result) :-

    Answer = new,
    Part = head :
      Status = head_var,
      Result = ok;

    Answer = new,
    Part = guard :
      Status = new,
      Result = new(guard);

    Answer = new,
    Part = assign :
      Status = body_var,
      Result = ok;

    Answer = new,
    Part = body :
      Status = body_var,
      Result = ok;

    Answer = head_var,
    Part = head :
      Status = many,
      Result = ok;

    Answer = head_var,
    Part = assign :
      Status = assigned,
      Result = ok;

    Answer = head_reader,
    Part = assign :
      Status = passed_reader,
      Result = ok;

    Answer = head_var,
    Part = body :
      Status = passed,
      Result = ok;

    Answer = head_reader,
    Part = body :
      Status = passed_reader,
      Result = ok;

    Answer = many : Part = _,
      Status = many,
      Result = ok;

    Answer = many(Connections): Part = _,
      Status = many(Connections),
      Result = ok;

    Answer = new,
    Part = guard :
      Status = new,
      Result = new(guard);

    Answer = declared_listener : Part = _,
      Status = declared_listener,
      Result = ok;

    Answer = listenee : Part = _,
      Status = assigned_listenee,
      Result = ok;

    Answer =\= new, Answer =\= "_ro",
    Part = guard :
      Status = Answer,
      Result = ok;

    Answer = "_ro",
    Part =\= guard :
      Status = both,
      Result = ok;

    otherwise :
      Status = Answer,
      Result = conflict(Part) .

  readonly_functor(Answer, Part, Status, Result) :-

    Answer = new : Part = _,
      Status = "_ro",
      Result = ok;

    Answer = outport : Part = _,
      Status = ports,
      Result = ok;

    Answer = head_var : Part = _,
      Status = both,
      Result = ok;

    Answer = head_reader : Part = _,
      Status = both,
      Result = ok;

    Answer = body_var : Part = _,
      Status = both,
      Result = ok;

    Answer = ground, Part = body :
      Status = ground,
      Result = ok;

    otherwise :
      Status = Answer,
      Result = conflict(Part).


  ground_functor(Answer, Status, Result) :-

    Answer =\= "_ro" :
      Status = ground,
      Result = ok;

    Answer = "_ro" :
      Status = "_ro",
      Result = conflict(guard) .


diagnose(ok, ground(VId), (`VId)^, Diags, Diags^).
diagnose(ok, VId, VId^, Diags, Diags^) :-
    VId =\= ground(_) | true.
diagnose(Diagnostic(Part), ground(VId), (`"_")^,
	 [Part(Diagnostic, `VId) | Diags]^, Diags
).
diagnose(Diagnostic(Part), VId, (`"_")^,
	 [Part(Diagnostic, VId) | Diags]^, Diags
) :-
    otherwise |
	true.

terminus(In) :-

    In ? lookup(Id, Answer, Result), Id =\= "_" :
      Answer = new |
	node(In', Id, Result, Left, Right),
	terminus(Left),
	terminus(Right);

    In ? Request(_, Answer, _), Request =\= lookup :
      Answer = new |
	self;

    In ? lookup("_", Answer, _) :
      Answer = new |
	self;

    In = [] | true;

    In = collect(Close1, Close2) :
      Close2 = Close1 .

  node(In, Id, Status, Left, Right) :-

    In ? _(Id, Answer, Status') :
      Answer = Status |
	self;

    In ? Lookup, Lookup = _(Less, _, _),
    Less @< Id :
      Left ! Lookup |
	self;

    In ? Lookup, Lookup = _(More, _, _),
    Id @< More :
      Right ! Lookup |
	self;

    In = [] : Id = _, Status = _,
      Left = [], Right = [] ;

    In = collect(Head, Tail) :
      Head ! Status(Id),
      Left = collect(Head', Middle),
      Right = collect(Middle, Tail).


add_procedure(Name, Arity, Dict1, Dict2) :-

    Dict1 = Name(Arity, _Left, _Right) :
      Dict2 = Dict1;

    Dict1 = Name(Arity1, Dict1', Right),
    Arity < Arity1 :
      Dict2 = Name(Arity1, Dict2'?, Right) |
	self;

    Dict1 = Name(Arity1, Left, Dict1'),
    Arity > Arity1 :
      Dict2 = Name(Arity1, Left, Dict2'?) |
	self;

    Dict1 = Name1(Arity1, Dict1', Right),
    Name @< Name1 :
      Dict2 = Name1(Arity1, Dict2'?, Right) |
	self;

    Dict1 = Name1(Arity1, Left, Dict1'),
    Name1 @< Name :
      Dict2 = Name1(Arity1, Left, Dict2'?) |
	self;

    Dict1 = [] :
      Dict2 = Name(Arity, [], []).


find_procedure(Name, Arity, Dict, Reply) :-

    Dict = Name(Arity, _Left, _Right) :
      Reply = true;

    Dict = Name(Arity1, Dict', _Right),
    Arity =< Arity1 |
	self;

    Dict = Name(Arity1, _Left, Dict'),
    Arity > Arity1 |
	self;

    Dict = Name1(_Arity1, Dict', _Right),
    Name @< Name1 |
	self;

    Dict = Name1(_Arity1, _Left, Dict'),
    Name1 @< Name |
	self;

    Dict = [] : Name = _, Arity = _,
      Reply = false.
