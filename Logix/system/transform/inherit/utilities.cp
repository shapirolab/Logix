-language(compound).
-export([update_attributes/2, extract_attributes/5, overrides, sort_ids,
	 variable_trees, note_variables, primed_arguments, append_list, or]
).


update_attributes(Attributes1, Attributes2) + (Depends = []) :-

   Attributes1 ? export(_) |
	self;

   Attributes1 ? inherit(_) |
	self;

   Attributes1 ? override(_) |
	self;

   Attributes1 ? depends(Others) |
	merge(Others, Depends, Depends'),
	self;

   Attributes1 ? A,
   otherwise :
      Attributes2 ! A |
	self;

   Attributes1 = [],
   Depends =\= [] :
      Attributes2 = [depends(Depends)] ;

   Attributes1 = [],
   Depends = [] :
      Attributes2 = [] .



extract_attributes(Attributes, SId, Inherits, OverRides, Exports)
 + (Heritage = []) :-

   Attributes ? service_id(Path) :
      SId' = _ |
	normalize_service_id(Path, SId),
	self;

   Attributes ?  override(Es) |
	self,
	normalize_overrides(Es, OverRides, OverRides'?);

   Attributes ? inherit(Inherit) |
	merge(Inherit, Heritage, Heritage'),
	self;

   Attributes ? export(Exports^) :
      Exports' = _ |
	self;

   Attributes ? _Other,
   otherwise |
	self;

   Attributes = [] :
      Inherits = Heritage,
      OverRides = [],
      SId ! self,
      Exports = all |			% Why not?
	computation # service_id(SId').

normalize_service_id(Path, SId) :-

   Path ? Name,				% Counter-kluge (hierarchy_server)
   channel(Path') :
      SId ! Name,
      write_channel(service_id(SId'), Path') ;

   otherwise :
      SId = Path .
	
normalize_overrides(Es, OverRides1, OverRides2) :-

   Es ? OverRide |
	normalize_override(OverRide, OverRides1, OverRides1'?),
	self;

   Es = [] :
      OverRides1 = OverRides2 ;

   Es =\= [_|_], Es =\= [] |
	normalize_override(Es, OverRides1, OverRides2).

normalize_override(OverRide, OverRides1, OverRides2) :-

   string(OverRide), OverRide =\= "" :
      OverRides1 = [OverRide | OverRides2] ;

   otherwise :
      OverRides1 = OverRides2 |
	computation # diagnostic((invalid_override : OverRide)).


merge(In, List1, List2) :-

   In ? Term |
	merge_term(Term, List1, List1'),
	self;

   In = [] :
      List2 = List1 ;

   In =\= [_|_], In =\= [] |
	merge_term(In, List1, List2).

merge_term(Term, List1, List2) :-

   List1 ? Term :
      List2 ! Term,
      List2' = List1'? ;

   List1 ? Other,
   Other =\= Term :
      List2 ! Other |
	self;

   List1 = [] :
      List2 = [Term] ;

   otherwise : Term = _,
      List2 = [] |
	computation # diagnostic((invalid_list : List1)).


overrides(Pairs, OverRides) :-

   OverRides = [_ | _],
   Pairs ? Functor(Result) :
      Head = Rest?,
      Tail = Rest |
	overrides1;

   Pairs = [] : OverRides = _ ;

   OverRides = [] |
	set_local.

overrides1(Pairs, OverRides, Functor, Result, Head, Tail) :-

   OverRides ? Functor :
      Result = override,
      Tail = OverRides',
      OverRides'' = Head |
	overrides;

   OverRides ? Other,
   otherwise :
      Tail ! Other |
	self;

   OverRides = [] :
      Functor = _,
      Result = "local",
      Tail = [],
      OverRides' = Head |
	overrides.

set_local(Pairs) :-

   Pairs ? _Functor("local"^) |
	self;

   Pairs = [] | true.


sort_ids(In, Out1) :-

   true : Out2 = [] |
	qsort_ids.

qsort_ids(In, Out1, Out2) :-

   In ? Id, Id = _/N |
	partition_ids(In'?, N, Smaller, In''),
	qsort_ids(Smaller?, Out1, [Id | Out1'?]),
	self;

   In = [] :
      Out1 = Out2 .
	       
partition_ids(In, N, Smaller, Larger) :-

   In ? Id, Id = _/J,
   J =< N :
      Smaller ! Id |
	self;

   In ? Id, Id = _/K,
   N < K :
      Larger ! Id |
	self;

   In = [] : N = _,
      Smaller = [],
      Larger = [] .


variable_trees(Implicit, Self, Head, Guard, Goals, ITree, STree) :-

   Implicit = true,
   Self = true :
      ITree = tree(VDict?),
      STree = tree(VDict?) |
	variables_dictionary;

   Implicit = true,
   Self = false :
      ITree = tree(VDict?),
      STree = none |
	variables_dictionary;

   Implicit = false,
   Self = true :
      ITree = none,
      STree = tree(VDict?) |
	variables_dictionary;

   Implicit = false,
   Self = false : Head = _, Guard = _, Goals = _,
      ITree = none,
      STree = none .


variables_dictionary(Head, Guard, Goals, VDict) :-
	body_variables(Goals, [], Dict),		% Do body first,
	note_variables(Guard, Dict?, Dict', guard),	% and heads last
	note_variables(Head, Dict'?, VDict, head).	% to over-ride annotation.

body_variables(Goals, Dict, VDict) :-

   Goals ? goal(_, Goal, _) |
	note_variables(Goal, Dict, Dict', body),
	self;

   Goals ? rpc(Service, Goal) |
	note_variables(Service, Dict, Dict', body),
	note_variables(Goal, Dict'?, Dict'', body),
	self;

   Goals ? implicit_rpc(_, _, _) |
	self;

   Goals ? primitive(Goal) |
	note_variables(Goal, Dict, Dict', body),
	self;

   Goals = [] |
      VDict = Dict .

note_variables(Term, Dict1, Dict2, Type) :-

   Term ? Car |
	note_variables(Car, Dict1, Dict1', Type),
	self;

   Term = `V |
	primed_variable;

   Term = ?V |
	primed_variable;

   Term = Term'!, Term' = `V : Type = _,
      Type' = body |
	primed_variable;

   Term = (`_)?? : Type = _,		% ignore everywhere
      Dict2 = Dict1 ;

   Term = Hat(Term'), Hat = "^" : Type = _,
      Type' = body |
	self;

   tuple(Term),
   Term =\= _(_) |
	note_tuple(Term, Dict1, Dict2, Type, 1);

   Term = Functor(_),
   Term =\= `_, Term =\= ?_, Term =\= (`_)??, Term =\= (`_)!, Functor =\= "^" |
	note_tuple(Term, Dict1, Dict2, Type, 1);

   otherwise : Term = _, Type = _,
      Dict2 = Dict1 .

note_tuple(Tuple, Dict1, Dict2, Type, Index) :-

   arg(Index++, Tuple, Arg) |
	note_variables(Arg, Dict1, Dict1', Type),
	note_tuple;

   otherwise : Type = _, Index = _, Tuple = _,
      Dict2 = Dict1 .

primed_variable(Term, Dict1, Dict2, Type, V) :-

   string(V), V =\= "_",
   nth_char(string_length(V), V) =:= ascii("'"),
   string_to_dlist(V, VL, []),
   string_to_dlist("'", Ps, Qs),
   string_to_dlist(V, VLPs, Rs) :
      Qs = Ps?,
      Rs = Ps? |
	split_variable_name(VL, VLPs, Ps, VH?, VH, VN, Primes),
	add_variable;

   string(V), V =\= "_",
   nth_char(string_length(V), V) =\= 39 :	% ascii("'") = 39
      Primes = [],
      VN = V |
	add_variable;

   otherwise : Term = _, Type = _, V = _,
      Dict2 = Dict1 .

split_variable_name(VL, VLPs, Ps, VH, VT, VN, Primes) :-

   VLPs = Ps :
     VT = [],
     Primes = VL |
	list_to_string(VH, VN);

   VLPs =\= Ps,
   VL ? C,
   VLPs ? _ :
      VT ! C |
	self.

add_variable(Term, VN, Primes, Dict1, Dict2, Type) :-

   Dict1 = VN(_Var, Primes, _, _), Type = body,
   Term = ?_ : Type = _,
      Dict2 = Dict1 ;

   Dict1 = VN(_Var, Primes, L, R), Type = body,
   Term  = `_ :				% Multiple and writer, same-primed:
      Dict2 = VN(?VN, Primes, L, R) ;	% force iterate reader

   Dict1 = VN(_Var, Primes, L, R), Type =\= body : Term = _,
      Dict2 = VN(`VN, Primes, L, R) ;	% But head/guard: force iterate writer.

   Dict1 = VN(_Var, VPrimes, _, _),
   Primes @< VPrimes : Term = _, Type = _,
      Dict2 = Dict1 ;

   Dict1 = VN(_Var, VPrimes, L, R), Type = body,
   VPrimes @< Primes,
   Term = `Primed :
      Dict2 = VN(?Primed, Primes, L, R);

   Dict1 = VN(_Var, VPrimes, L, R), Type =\= body,
   VPrimes @< Primes,
   Term = _(Primed) :
      Dict2 = VN(`Primed, Primes, L, R);

   Dict1 = VN(_Var, VPrimes, L, R),
   VPrimes @< Primes,
   Term = ?Primed : Type = _,
      Dict2 = VN(`Primed, Primes, L, R);

   Dict1 = V1(Var, VPrimes, Dict1', R),
   VN @< V1 :
      Dict2 = V1(Var, VPrimes, Dict2'?, R) |
	self;

   Dict1 = V1(Var, VPrimes, L, Dict1'),
   V1 @< VN :
      Dict2 = V1(Var, VPrimes, L, Dict2'?) |
	self;

   Dict1 = [],
   VN =\= "", VN =\= "_", 
   Term = `_, Type = body :
      Dict2 = VN(?VN, Primes, [], []) ;

   Dict1 = [],
   VN =\= "", VN =\= "_", 
   Term = `_, Type =\= body :
      Dict2 = VN(`VN, Primes, [], []) ;

   Dict1 = [],
   VN =\= "", VN =\= "_", 
   Term = ?_ : Type = _,
      Dict2 = VN(`VN, Primes, [], []) ;

   Dict1 = [],
   otherwise : Term = _,  VN = _, Primes = _, Type = _,	% Primed "_" or primed "" ?
      Dict2 = Dict1 .


primed_arguments(N, In, VTree, Out) :-

   N-- > 0,
   In ? VI, VI = _(Index), integer(Index) :
      Out ! VI |
	self;

   N-- > 0,
   In ? _(Name), string(Name) :
      Out ! Rename? |
	self,
	primed_replacement;

   N =< 0 : In = _, VTree = _,
      Out = [] .

primed_replacement(Name, VTree, Rename) :-

   VTree = N1(_, _, VTree', _),
   Name @< N1 |
	self;

   VTree = N1(_, _, _, VTree'),
   N1 @< Name |
	self;

   VTree = Name(Annotation(_), [], _Left, _Right) :
      Rename = Annotation(Name) ;

   VTree = Name(Annotation(_), Primes, _Left, _Right),
   Primes =\= [],
   string_to_dlist(Name, NL, Ps) :
      Ps = Primes,
      Rename = Annotation(Name'?) |
	list_to_string(NL, Name');

   VTree = [] :
      Rename = `Name .


append_list(Hs, Ts, As) :-

   Ts =\= [],
   Hs ? A :
      As ! A |
	self;

   Ts = [] :
      As = Hs ;

   Hs = [] :
      As = Ts .


or(A, B, C) :-
   A = true : B = _,
      C = true;
   B = true : A = _,
      C = true;
   A =\= true, B =\= true :
      C = false .
