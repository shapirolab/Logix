/* $Header: /home/qiana/Repository/Logix/system/ndg/phase2/ctlopt/ctlopt1.cp,v 1.1 1999/07/09 07:03:05 bill Exp $ */
-language(compound).
-export([instructions/3]).
-mode(trust).

procedure instructions([Any], [Any], Any).

instructions(In, Passed, SC) :-
	pass1([label(_) | In], [_ | Blocks], open, continue, Pass2),
	combine(Pass2, Blocks, Passed, SC).

pass1(In, Blocks, Started, Last, Pass2) :-

    In ? label(Definition1),
    In' = [label(Definition2) | _] :
      Definition1 = Definition2 |
	pass1;

    In ? label(Definition),
    Last = continue,
    In' = [goto(label(Target)) | _] :
      Definition = Target |
	pass1;

    In ? label(Definition),
    Last = stop,
    In' ? goto(label(Target)) :
      Definition = Target |
	pass1;

    In ? Label, Label = label(_),
    In' =\= [label(_) | _],
    In' =\= [goto(_) | _],
    Last = continue : Started = _,
      Started' = open,
      Blocks = [embedded(End, End), Status(Head, Tail) | Blocks'],
      Head ! Label,
      Blocks'' = [Status(Head', Tail) | Blocks'] |
	pass1;

    In ? Label, Label = label(_),
    In' =\= [label(_) | _],
    In' =\= [goto(_) | _],
    Last = stop : Started = _,
      Last' = continue,
      Started' = closed,
      Blocks ! Status(Head, Tail),
      Head ! Label,
      Blocks'' = [Status(Head', Tail) | Blocks'] |
	pass1;

    In ? Goto, Goto = goto(_),
    Started = open : Last = _,
      Last' = stop,
      Out ! Goto,
      Blocks ! Started(Out, Out') |
	pass1;

    In ? Goto, Goto = goto(_),
    Started = closed : Last = _,
      Blocks ! _(Out, Out'),
      Out ! Goto,
      Last' = stop |
	pass1;

    In ? Switch, Switch = switch_on_tag(_, _),
    Started = open : Last = _,
      Last' = stop,
      Blocks ! Started(Out, Out'),
      Out ! Switch |
	pass1;

    In ? Switch, Switch = switch_on_tag(_, _),
    Started = closed : Last = _,
      Last' = stop,
      Blocks ! _(Out, Out'),
      Out ! Switch |
	pass1;

    In ? Branch, Branch = branch_integer(_, _, _, _),
    Started = open : Last = _,
      Last' = stop,
      Blocks ! Started(Out, Out'),
      Out ! Branch |
	pass1;

    In ? Branch, Branch = branch_integer(_, _, _, _),
    Started = closed : Last = _,
      Last' = stop,
      Blocks ! _(Out, Out'),
      Out ! Branch |
	pass1;

    In ? Branch, Branch = branch_real(_, _, _, _),
    Started = open : Last = _,
      Last' = stop,
      Blocks ! Started(Out, Out'),
      Out ! Branch |
	pass1;

    In ? Branch, Branch = branch_real(_, _, _, _),
    Started = closed : Last = _,
      Last' = stop,
      Blocks ! _(Out, Out'),
      Out ! Branch |
	pass1;

    In ? Branch, Branch = branch_tuple(_, _, _, _),
    Started = open : Last = _,
      Last' = stop,
      Blocks ! Started(Out, Out'),
      Out ! Branch |
	pass1;

    In ? Branch, Branch = branch_tuple(_, _, _, _),
    Started = closed : Last = _,
      Last' = stop,
      Blocks ! _(Out, Out'),
      Out ! Branch |
	pass1;

    In ? Case, Case = case_hash_integer(_, _),
    Started = open : Last = _,
      Last' = stop,
      Blocks ! Started(Out, Out'),
      Out ! Case |
	pass1;

    In ? Case, Case = case_hash_integer(_, _),
    Started = closed : Last = _,
      Last' = stop,
      Blocks ! _(Out, Out'),
      Out ! Case |
	pass1;

    In ? Case, Case = case_hash_string(_, _),
    Started = open : Last = _,
      Last' = stop,
      Blocks ! Started(Out, Out'),
      Out ! Case |
	pass1;

    In ? Case, Case = case_hash_string(_, _),
    Started = closed : Last = _,
      Last' = stop,
      Blocks ! _(Out, Out'),
      Out ! Case |
	pass1;

    In ? Suspend, Suspend = suspend,
    Started = open : Last = _,
      Started' = closed,
      Last' = stop,
      Blocks ! Started(Out, Out'),
      Out ! Suspend |
	pass1;

    In ? Suspend, Suspend = suspend,
    Started = closed : Last = _,
      Last' = stop,
      Blocks ! _(Out, Out'),
      Out ! Suspend |
	pass1;

    In ? Suspend, Suspend = suspend(_),
    Started = open : Last = _,
      Started' = closed,
      Last' = stop,
      Blocks ! Started(Out, Out'),
      Out ! Suspend |
	pass1;

    In ? Suspend, Suspend = suspend(_),
    Started = closed : Last = _,
      Last' = stop,
      Blocks ! _(Out, Out'),
      Out ! Suspend |
	pass1;

    In ? Suspend, Suspend = suspend(_, _),
    Started = open : Last = _,
      Started' = closed,
      Last' = stop,
      Blocks ! Started(Out, Out'),
      Out ! Suspend |
	pass1;

    In ? Suspend, Suspend = suspend(_, _),
    Started = closed : Last = _,
      Last' = stop,
      Blocks ! _(Out, Out'),
      Out ! Suspend |
	pass1;

    In ? Halt, Halt = halt,
    Started = open : Last = _,
      Started' = closed,
      Last' = stop,
      Blocks ! Started(Out, Out'),
      Out ! Halt |
	pass1;

    In ? Halt, Halt = halt,
    Started = closed : Last = _,
      Last' = stop,
      Blocks ! _(Out, Out'),
      Out ! Halt |
	pass1;

    In ? Deschedule, Deschedule = deschedule,
    Started = open : Last = _,
      Started' = closed,
      Last' = stop,
      Blocks ! Started(Out, Out'),
      Out ! Deschedule |
	pass1;

    In ? Deschedule, Deschedule = deschedule,
    Started = closed : Last = _,
      Last' = stop,
      Blocks ! _(Out, Out'),
      Out ! Deschedule |
	pass1;

    In ? Instruction,
    otherwise : Last = _,
      Last' = continue,
      Blocks ! Block(Out, Tail),
      Out ! Instruction,
      Blocks'' = [Block(Out', Tail) | Blocks'] |
	pass1;

    In = [] : Started = _, Last = _/*stop*/,
      Blocks = [],
      Pass2 = iterate .


combine(Done, Blocks, Passed, SC) :-

    Done = iterate |
	freeze(Blocks, Frozen, _),
	melt(Frozen, Melted, Variables),
	pass2(Blocks, Melted, [], Blocks', First, done, Done'),
	instantiate_labels(First, Variables, Done', Done''),
	combine;

    Done = done,
    Blocks ? Status([_Label | Head], Tail) :
      Blocks' = Blocked',
      Blocked ! Status(Head, Tail) |
	collect(Blocked, Passed, SC).

collect(Blocks, Passed, SC) :-

    Blocks ? elided(_, _) |
	collect;

    Blocks ? State(Head, Passed'),
    State =\= elided :
      Passed = Head |
	collect;

    Blocks ? State(Head, Passed') :
      State = closed,
      Passed = Head |
	collect;

    Blocks = [] , SC = {L, R} :
      Passed = [], L = R .

instantiate_labels(Index, Variables, Done1, Done2) :-

    Variables ? X,
    known(X) |
	instantiate_labels;

    Variables ? L,
    unknown(L),
    Index' := Index + 1 :
      L = Index |
	instantiate_labels;

    Variables = [] : Index = _,
      Done1 = Done2 .


pass2(Blocks1, Blocks2, Kept, Blocks, First, Done1, Done2) :-

    Blocks1 ? Embedded, Embedded = embedded(_, _) :
      Blocks2 ! _(_, []),
      Blocks ! Embedded |
	pass2;

    Blocks1 ? Elided, Elided = elided(_, _) :
      Blocks2 ! _(_, []) |
	pass2;

    Blocks1 ? Open, Open = open(_, _) :
      Blocks2 ! _([_ | Block], []),
      Blocks ! Open |
	pass2,
	unify_blocks(open, Block, Open, Kept, Kept', Done1, Done1');

    Blocks1 ? Closed, Closed = Status(_, _),
    unknown(Status) :
      Blocks2 ! _([_ | Block], []) |
	pass2,
	unify_blocks(closed, Block, Closed, Kept, Kept',
			Done1(Blocks), Done1'(Blocks')
	);

    Blocks1 = [] : Kept = _, Blocks2 = _,
      Blocks = [],
      First = 1,
      Done1 = Done2 .


unify_blocks(Kind, Block, Blocked, Kept1, Kept2, Done1, Done2) :-

    Kept1 ? K, K = _(KBlock, _, _),
    Block =\= KBlock :
      Kept2 ! K |
	unify_blocks;

    Kept1 ? closed(Block, Label, BStatus),
    Kind = open :
      BStatus = elided,
      Blocked = Status([Label | _], _),
      Kept2 = [Kind(Block, Label, Status) | Kept1'] |
	iterate(Done1, Done2) ;

    Kept1 ? closed(Block, Label, _),
    Kind = closed : Kept1' = _,
      Blocked = elided([Label | _], _),
      Kept2 = Kept1 |
	iterate(Done1, Done2) ;

    Kept1 ? open(Block, Label, _),
    Kind = closed : Kept1' = _,
      Blocked = elided([Label | _], _),
      Kept1 = Kept2 |
	iterate(Done1, Done2) ;

    Kept1 ? open(Block, _, _),
    Kind = open : Kept1' = _, Blocked = _,
      Kept2 = Kept1,
      Done1 = Done2 ;

    Kept1 = [],
    Kind = open :
      Blocked = Status([Label | _], _),
      Kept2 = [Kind(Block, Label, Status)],
      Done1 = Done2 ;

    Kept1 = [],
    Kind = closed :
      Blocked = Status([Label | _], _),
      Kept2 = [Kind(Block, Label, Status)],
      Done1 = Done(Blocks),
      Blocks ! Blocked,
      Done2 = Done(Blocks') .

iterate(Done1, Done2) :-

    Done1 = Done(X),
    string(Done) :
      Done2 = iterate(X) ;

    string(Done1) :
      Done2 = iterate .
