-language([inherit, dfcp]).
-export(extract).
-mode(trust).

procedure extract(Integer, Ids, Goals).

Ids ::= Id ; [Id] .

	Id ::= ModuleName ; ModuleName # Procs .
		Procs ::= ProcedureName ; [ProcedureName].
		ProcedureName ::= String .

Goals ::= [Goal] ; [] .

	Goal ::= ModuleName # Predicate.
		Predicate ::= Tuple.

ModuleName ::= String.


extract(Computation, Ids, Display) :-
	computation # shell(resolvent, Computation, Resolvent, Ok),
	resolve(Ok?, Ids, Display, Resolvent?).	/* Resolvent is a list of lists */

resolve(Ok, Ids, Display, Resolvent) :-         

    Ok = true |
	res1(Resolvent, Ids, Display, []);

    Ok =\= true |
	Resolvent = _,
	Ids = _,
	Display = [Ok].

res1(Resolvent, Ids, Display1, Display2) :-

    Ids ? IdSet,
    listener(Resolvent) |
	res2(Resolvent, IdSet, Display1, Display1'?),
	self;

    Ids =\= [_|_], Ids =\= [] |
	Ids' = [Ids],
	self;
  
    Ids = [] |
	Resolvent = _,
	Display1 = Display2.

res2(Resolvent, IdSet, Display1, Display2) :-

    Resolvent ? Path # Goals,
    Path = [Name | _],
    IdSet = Name |
	res5(Name, Goals, Display1, Display1'?),
	self;

    Resolvent ? Path # _Goals,
    Path = [Name | _],
    IdSet = Name # Procs,
    Procs = [_|_],
    listener(Resolvent)|
	res3,
	res1(Resolvent, Ids?, Display1, Display1'?),
	self;

    Resolvent ? Path # Goals,
    Path = [Name | _],
    IdSet = Name # Proc,
    Proc =\= [_|_] |
	res4,
	res5(Name, SubSet?, Display1, Display1'?),
	self;

    Resolvent ? Path # _Goals,
    Path = [Name | _],
    IdSet =\= Name, IdSet =\= Name # _ |
	self;   

    Resolvent = [] |
	IdSet = _,
	Display1 = Display2.
	
res3(Name, Procs, Ids) :-

    Procs ? Proc,
    string(Name) |
	Ids ! Name#Proc,
	self;

    Procs =\= [], Procs =\= [_|_] |
	Procs' = [Procs],
	self;

    Procs = [] |
	Name = _,
	Ids = [] .


res4(Proc, Goals, SubSet) :-

    Goals ? Goal,
    tuple(Goal),
    listener(Goal) |
	arg(1, Goal, Functor),
	add_goal;

    Goals ? _Goal,
    otherwise |
	self;

    Goals = [] |
	Proc = _,
	SubSet = [].

add_goal(Proc, Goals, SubSet, Functor, Goal) :-

    Proc = Functor |
	SubSet ! Goal,
	res4;

    Proc =\= Functor |
	Goal = _,
	res4.
    
res5(Name, Goals, Display1, Display2) :-

  Goals ? Goal,
  string(Name) |
	Display1 ! (Name#Goal),
	self;

  Goals = [] |
	Name = _,
	Display1 = Display2 .
