/* $Header: /home/qiana/Repository/Logix/system/ndg/pretty_dg.cp,v 1.1 1999/07/09 07:02:57 bill Exp $ */
-export([pretty/4]).
-language(compound).
-mode(trust).

Ave ::= Integer.

Labels ::= [LabEnt].
LabEnt ::= {Label,Integer} ; {Label,Dg}.
Label ::= Integer.

Summary ::= [SumLine].
SumLine ::= {0,{Proc1,size(Integer),depth(Integer),average(Integer)}}.

/*
Ch ::= Any.


Ws ::= Integer.
*/

procedure pretty(Context, File, IxAlg, PrettyOpt).

pretty(Context, File, IxAlg, PrettyOpt) :-
	utils # append_strings([File,'.dg.',IxAlg], DgFile),
	file # execute_in_context(Context,get_file(DgFile,DgF,[string], Ok)),
	pretty1(Ok, Context, File, DgFile, DgF, PrettyOpt).
	

procedure pretty1(Ok, Context, File, DgFile, DgF, PrettyOpt).

pretty1(Ok, Context, File, DgFile, DgF, PrettyOpt) :-

	Ok = true |
	melt(DgF,Dg,Vars),
	check_vars(Vars, DgFile, DispOpt), 
	pretty2(Dg, DispOpt, Context, File, DgFile, PrettyOpt).

pretty1(not_found, _Context, _, DgFile, _, _) :-
	utils # append_strings(['ERROR - 997 - Error in reading file ', 
							DgFile], Msg),
	computation # display(term,Msg,[type(ground)]).


procedure check_vars(Vars, DgFile, DispOpt).

check_vars(Vars, DgFile, DispOpt) :- 

    Vars = [] : DispOpt = ground, DgFile = _ ;

    Vars =\= [] : DispOpt = namevars |
	computation # display(term, pretty_dg(DgFile,Vars)).


procedure pretty2(Dg, DispOpt, Context, File, DgFile, PrettyOpt).

pretty2(Dg, DispOpt, Context, File, DgFile, PrettyOpt) :-

    PrettyOpt = all |
	compile#context(Context,File,[mode(trust),intermediate(FI)],_),
	widgets#pretty#module(FI?,FO),
	widgets#pretty#context(Context,File,Src),
	pretty3(Dg, PrettyDgs\Summary, Summary),
	utils#append_strings([DgFile,'.pretty'], PrettyFile),
	write_pretty(Src, Context, PrettyFile, put, done, Done1),
	write_pretty(FO, Context, PrettyFile, append, Done1, Done2),
	write_pretty1(Done2, DispOpt, Context, PrettyDgs, PrettyFile);

    PrettyOpt = dg : File = _ |
	pretty3(Dg, PrettyDgs\Summary, Summary),
	utils#append_strings([DgFile,'.pretty'], PrettyFile),
	processor # interface(date_time(_,D,T)),
	computation # display(term, {' ',D,T},
		[put(Context#PrettyFile),type(ground),close(done,Done)]),
	write_pretty1(Done, DispOpt, Context, PrettyDgs, PrettyFile);

    PrettyOpt = src : Dg = _, DispOpt = _, DgFile = _ |
	compile#context(Context,File,[mode(trust),intermediate(FI)],_),
	widgets#pretty#module(FI?,FO),
	widgets#pretty#context(Context,File,Src),
	utils#append_strings([File,'.src'], PrettyFile),
	write_pretty(Src, Context, PrettyFile, done, Done1, put),
	write_pretty(FO, Context, PrettyFile, Done1, Done2, append),
	write_date(Context,Done2,PrettyFile);

    PrettyOpt = sum : File = _ |
	pretty_sum(Dg,PrettyDgs),
	utils#append_strings([DgFile,'.sum'], PrettyFile),
	processor # interface(date_time(_,D,T)),
	computation # display(term, {' ',D,T},
		[put(Context#PrettyFile),type(ground),close(done,Done)]),
	write_pretty1(Done, DispOpt, Context, PrettyDgs, PrettyFile).


procedure write_date(Context, Done, File).

write_date(Context, Done, File) :-
    Done = done |
	processor # interface(date_time(_,D,T)),
	file#execute_in_context(Context,put_file(File,({'
',' ',D,T},[append,type(ground)],Ok))),
	utils#append_strings(
		['MESSAGE 000 - File ', File,' has been written.'], Msg),
	check_date(Ok, Msg).


procedure check_date(Ok, Msg).

check_date(Ok, Msg) :-
    Ok = true |
	computation # display(term,Msg,[type(ground)]).


procedure write_pretty(FO, Context, File, Append, Done, Done1).

write_pretty(FO, Context, File, Append, Done, Done1) :-

    Done = done,
    Append = append |
	file#execute_in_context(Context,put_file(File,FO,
						[append,list,type(ground)],Ok)),
	check_write(Ok, Done1);

    Done = done,
    Append = put |
	file#execute_in_context(Context,put_file(File,FO,
						[list,type(ground)],Ok)),
	check_write(Ok, Done1).


procedure check_write(Ok, Done1).

check_write(Ok, Done1) :-
    Ok = true :
      Done1 = done .
	
procedure write_pretty1(Rt, DispOpt, Context, Ls, File).

write_pretty1(Rt, DispOpt, Context, Ls, File) :-

    Rt = done,
    Ls ? {I,L},
    I < 78,
    W := 78 - I |
	computation # display(term, L,
		[append(Context#File),indent(I),type(DispOpt),width(W),
		 close(Rt,Rt')]),
	write_pretty1;

    Rt = done,
    Ls ? {I,L},
    I >= 78 |
	computation # display(term, L, [append(Context#File),type(DispOpt),
					indent(60),width(18),close(Rt,Rt')]),
	write_pretty1;


    Rt = done,
    Ls = [] |
	processor # interface(date_time(_,D,T)),
	computation # display(term, {' ',D,T},
			      [append(Context#File),type(DispOpt)]),
	utils#append_strings(
		['MESSAGE 000 - File ', File,' has been written.'], Msg),
	computation # display(term, Msg).

procedure pretty3(Dgs, PrL, Summary).

pretty3(Dgs, PrL, Summary) :- 

	Dgs ? {Proc1,_,Dg},
	PrL = PrH\PrT :
	PrH ! {0,' '},
	PrH' ! {0,{Proc1,size(Size?),depth(Depth?),average(Ave?)}},
	Summary ! {0,{Proc1,size(Size?),depth(Depth?),average(Ave?)}},
	PrL' = PrT'\PrT |
	print_dg(Dg, 0, PrH''\PrT', Size),
	get_depth(Dg, Depth),
	get_ave(Dg, Ave, Proc1),	
	pretty3;

    Dgs = [],
    PrL = H\T :
      H = T,
      Summary = [] .


procedure pretty_sum(Dgs, Summary).

pretty_sum(Dgs, Summary) :- 

    Dgs ? {Proc1,Dg} :
      Summary ! {0,{Proc1,size(Size?),depth(Depth?),average(Ave?)}} |
	get_size(Dg, Size),
	get_depth(Dg, Depth),
	get_ave(Dg, Ave, Proc1),	
	pretty_sum;

    Dgs = [] :
      Summary = [] .


procedure print_dg(Dg, Indent, PrL, Size).

print_dg(Dg, Indent, PrL, Size) :-

    Dg = case(T,Sts),
    PrL = PrH \ PrT,
    Indent < 60 :
      PrH ! {Indent,{case,T}} |
	Indent' := Indent + 1,
	print_dgs(Sts, Indent', PrH'\PrT, Size'),
	Size := Size' + 1;

    Dg = case(T,Sts),
    PrL = PrH \ PrT,
    Indent >= 60 :
      PrH ! {Indent,{case,T}} |
	print_dgs(Sts, Indent, PrH'\PrT, Size'),
	Size := Size' + 1;

    Dg = {label(L),Dg'},
    PrL = PrH \ PrT :
      PrH ! {Indent,label(L)},
      PrL' = PrH'\PrT |
	print_dg;

    otherwise |
	print_leaf(Dg, Indent, Cs, PrL),
	Size := Cs.


procedure print_dgs(Sts, Indent, PrL, Size).
procedure print_dgs(Sts, Indent, PrL, Size, ISize).

print_dgs(Sts, Indent, PrL, Size)+(ISize=0) :-

    Sts ? {Edge,St}, 
    St =\= [suspend], St =\= [suspend(_)], St =\= [suspend(_,_)],
    PrL = PrH \ PrT,
    Indent < 60 :
      PrH ! {Indent,Edge},
      PrL' = PrT' \ PrT |
	Indent1 := Indent + 1,
	print_dg(St, Indent1, PrH'\PrT', ISize'),
	ISize'' := ISize + ISize',
	print_dgs;

    Sts ? {Edge,St}, 
    St =\= [suspend], St =\= [suspend(_)], St =\= [suspend(_,_)],
    PrL = PrH \ PrT,
    Indent >= 60 :
      PrH ! {Indent,Edge},
      PrL' = PrT' \ PrT |
	print_dg(St, Indent, PrH'\PrT', ISize'),
	ISize'' := ISize + ISize',
	print_dgs;

    Sts ? {Edge,[suspend]},
    PrL = PrH \ PrT  :
    PrH  ! {Indent,{Edge,[suspend]}},
    PrL' = PrH' \ PrT |
	ISize' := ISize + 1,
	print_dgs;

    Sts ? {Edge,[suspend(F)]},
    PrL = PrH \ PrT  :
      PrH  ! {Indent,{Edge,[suspend(F)]}},
      PrL' = PrH' \ PrT |
	ISize' := ISize + 1,
	print_dgs;

    Sts ? {Edge,[suspend(F,S)]},
    PrL = PrH \ PrT  :
      PrH  ! {Indent,{Edge,[suspend(F,S)]}},
      PrL' = PrH' \ PrT |
	ISize' := ISize + 1,
	print_dgs;

    Sts = [],
    PrL = PrH \ PrT  : Indent = _,
      Size = ISize,
      PrH = PrT .


procedure print_leaf(Dg, Indent, Cs, PrL).
procedure print_leaf(Dg, Indent, Cs, PrL, ICs).

print_leaf(Dg, Indent, Cs, PrL) + (ICs = 0) :-

    Dg = [], 
    PrL = PrH \ PrT :
      Cs = ICs,
      PrH = [{Indent,'----------'}|PrT] ;

    Dg ? Dg1,
    Dg1 = {DgTells,DgBodies},
    DgTells =\= suspend, DgTells =\= [], DgTells =\= goto,
    PrL = PrH \ PrT :
      PrH ! {Indent,'----------'},
      PrL' = PrT' \ PrT |
	print_tb(DgTells,Indent,PrH'\PrH'',ICs,ICs'),
	print_b(DgBodies,Indent,PrH''\PrT'),
	print_leaf;

    Dg ? {[],DgBodies},
    PrL = PrH \ PrT :
      PrH ! {Indent,'----------'},
      PrL' = PrT' \ PrT |
	ICs' := ICs + 1,
	print_b(DgBodies,Indent,PrH'\PrT'),
	print_leaf;

    Dg ? suspend,
    PrL = PrH \ PrT :
      PrH ! {Indent,'----------'},
      PrH' ! {Indent,suspend},
      PrL' = PrH'' \ PrT |
	ICs' := ICs + 1,
	print_leaf;

    Dg ? suspend(F,S),
    PrL = PrH \ PrT :
      PrH ! {Indent,'----------'},
      PrH' ! {Indent,suspend(F,S)},
      PrL' = PrH'' \ PrT |
	ICs' := ICs + 1,
	print_leaf;

    Dg ? suspend(F),
    PrL = PrH \ PrT :
      PrH ! {Indent,'----------'},
      PrH' ! {Indent,suspend(F)},
      PrL' = PrH'' \ PrT |
	ICs' := ICs + 1,
	print_leaf;

    Dg ? goto(L),
    PrL = PrH \ PrT :
      PrH ! {Indent,'----------'},
      PrH' ! {Indent,goto(L)},
      PrL' = PrH'' \ PrT |
	ICs' := ICs + 1,
	print_leaf.


procedure print_tb(DgTells, I, PrL, Count, CountOut).

print_tb(DgTells, I, PrL, Count, CountOut) :-

    DgTells ? T ,
    PrL = PrH \ PrT :
      PrH ! {I,T},
      PrL' = PrH' \ PrT |
	Count' := Count + 1,
	print_tb;

    DgTells = [],
    PrL = H\T : I = _,
      H = T,
      CountOut = Count .


procedure print_b(DgBodies, I, PrL).

print_b(DgBodies, I, PrL) :-

    DgBodies ? T ,
    PrL = PrH \ PrT :
      PrH ! {I,T},
      PrL' = PrH' \ PrT |
	print_b;

    DgBodies = [],
    PrL = PrH \ PrT : I = _,
       PrH = PrT .
	

procedure get_size(Dg, Size).

get_size(Dg, Size) :-

    Dg = case(_T,Sts) |
	get_sizes(Sts, Size'),
	Size := Size' + 1;

    Dg = {label(_),Dg'} |
	get_size;

    otherwise : Dg = _,
      Size = 1 .
%	get_size_leaf(Dg, Size).


procedure get_sizes(Sts, Size).
procedure get_sizes(Sts, Size, ISize).

get_sizes(Sts, Size) + (ISize=0) :-

    Sts ? {_Edge,St}, St =\= [suspend], St =\= [suspend(_,_)] |
	get_size(St, ISize'),
	ISize'' := ISize + ISize',
	get_sizes;

    Sts ? {_Edge,[suspend]} |
	ISize' := ISize + 1,
	get_sizes;

    Sts ? {_Edge,[suspend(_F,_S)]} |
	ISize' := ISize + 1,
	get_sizes;

    Sts = [] :
      Size = ISize .

/*
procedure get_size_leaf(Dg, Cs).
procedure get_size_leaf(Dg, Cs, ICs).

get_size_leaf(Dg, Cs) + (ICs=0) :-

    Dg = [] :
      Cs = ICs ;

    Dg ? {DgTells,_},
    DgTells =\= suspend, DgTells =\= [], DgTells =\= goto |
	count_tb(DgTells,ICs,ICs'),
	get_size_leaf;

    Dg ? {[],_} |
	ICs' := ICs + 1,
	get_size_leaf;

    Dg ? suspend |
	ICs' := ICs + 1,
	get_size_leaf;

    Dg ? suspend(_) |
	ICs' := ICs + 1,
	get_size_leaf;

    Dg ? suspend(_,_) |
	ICs' := ICs + 1,
	get_size_leaf;

    Dg ? goto(_) |
	ICs' := ICs + 1,
	get_size_leaf.
*/

procedure count_tb(DgTells, Count, CountOut).

count_tb(DgTells, Count, CountOut) :-

    DgTells ? _T |
	Count' := Count + 1,
	count_tb;

    DgTells = [] :
       CountOut = Count .	


procedure get_depth(Dg, Depth).
procedure get_depth(Dg, Depth, Labels).

get_depth(Dg, Depth) + (Labels = _Labels) :-

    Dg = case(_T,Sts) |
	get_depths(Sts, Depth', Labels),
	Depth := Depth' + 1;

    Dg = {label(L),Dg'} |
	label_depth_ave({L,Depth},Labels),
	get_depth;

    otherwise |
	get_depth_leaf(Dg, Depth, Labels).


procedure get_depths(Sts, Depth, Labels).
procedure get_depths(Sts, Depth, Labels, IDepth).

get_depths(Sts, Depth, Labels) + (IDepth=0) :-

    Sts ? {_Edge,St}, St =\= [suspend], St =\= [suspend(_,_)] |
	get_depth(St, IDepth', Labels),
	max(IDepth, IDepth', IDepth''),
	get_depths;

    Sts ? {_Edge,[suspend]} |
	max(IDepth, 1, IDepth'),
	get_depths;

    Sts ? {_Edge,[suspend(_F,_S)]} |
	max(IDepth, 1, IDepth'),
	get_depths;

    Sts = [] : Labels = _ ,
       Depth = IDepth .


procedure get_depth_leaf(Dg, Depth, Labels).
procedure get_depth_leaf(Dg, Depth, Labels, IDepth).

get_depth_leaf(Dg, Depth, Labels) + (IDepth=0) :-

    Dg = [] : Labels = _,
       Depth = IDepth ;

    Dg ? {DgTells,_},
    DgTells =\= suspend, DgTells =\= [], DgTells =\= goto |
	count_tb(DgTells,IDepth,IDepth'),
	get_depth_leaf;

    Dg ? {[],_} |
	IDepth' := IDepth + 1,
	get_depth_leaf;

    Dg ? suspend |
	IDepth' := IDepth + 1,
	get_depth_leaf;

    Dg ? suspend(_) |
	IDepth' := IDepth + 1,
	get_depth_leaf;

    Dg ? suspend(_,_) |
	IDepth' := IDepth + 1,
	get_depth_leaf;

    Dg ? goto(L) |
	label_depth_ave({L,IDepth1}, Labels),
	IDepth' := IDepth1 + IDepth + 1,
	get_depth_leaf.


procedure label_depth_ave(LabEnt,Labels).

label_depth_ave(LabEnt,Labels) :-

    Labels = [{L, D1}|_],
    LabEnt = {L,D} :
      D = D1 ;

    Labels ? {L1,_D1},
    LabEnt = {L,_D},
    L =\= L1 |
	label_depth_ave;

    true :
      Labels = LabelsTail?,
      LabelsTail = [LabEnt|_] .


procedure get_ave(Dg, Ave, Proc1).

get_ave(Dg, Ave, Proc1) :-
    true :
      make_channel(Ch, AveM) |
	get_ave1(Dg, Ch, {done(Ch),Done}),
	average(Done, AveM, Ave, Proc1, Dg).


procedure get_ave1(Dg, Ch, SC).
procedure get_ave1(Dg, Ch, SC, Labels, Depth).

get_ave1(Dg, Ch, SC) + (Labels = _Labels, Depth = 0) :-

    Dg = case(_T,Sts) |
	Depth' := Depth + 1,
	get_aves(Sts, Labels, Depth', Ch, SC);

    Dg = {label(L),Dg'} |
	label_depth_ave({L,Dg'},Labels),
	get_ave1;

    otherwise |
	get_ave_leaf(Dg, Labels, Depth, Ch, SC).


procedure get_aves(Sts, Labels, Depth, Ch, SC).

get_aves(Sts, Labels, Depth, Ch, SC) :-

    Sts ? {_Edge,St}, St =\= [suspend], St =\= [suspend(_,_)],
    SC = {L,R} :
      SC' = {M,R} |
	get_ave1(St, Ch, {L,M}, Labels, Depth),
	get_aves;

    Sts ? {_Edge,[suspend]} :
      write_channel(Depth1,Ch) |
	Depth1 := Depth + 1,
	get_aves;

    Sts ? {_Edge,[suspend(_F,_S)]} :
      write_channel(Depth1,Ch) |
	Depth1 := Depth + 1,
	get_aves;

    Sts = [],
    SC = {L,R} : Labels = _, Depth = _, Ch = _,
       L = R .


procedure get_ave_leaf(Dg, Labels, Depth, Ch, SC).

get_ave_leaf(Dg, Labels, Depth, Ch, SC) :-

    Dg = [],
    SC = {L,R} : Labels = _,
      write_channel(Depth,Ch),
      L = R ;

    Dg ? {DgTells,_},
    DgTells =\= suspend, DgTells =\= [], DgTells =\= goto,
    SC = {L,R} :
      SC' = {M,R} |
	count_tb_sc(DgTells, Depth, Depth', {L,M}),
	get_ave_leaf;

    Dg ? {[],_} |
	Depth' := Depth + 1,
	get_ave_leaf;

    Dg = [suspend],
    SC = {L,R} : Labels = _,
      write_channel(Depth1,Ch),
      L = R | 
	Depth1 := Depth + 1;

    Dg = [suspend(_)],
    SC = {L,R} : Labels = _,
      write_channel(Depth1,Ch),
      L = R | 	
	Depth1 := Depth + 1;

    Dg = [suspend(_,_)],
    SC = {L,R} : Labels = _,
      write_channel(Depth1,Ch),
      L = R | 
	Depth1 := Depth + 1;

    Dg = [goto(L)] |
	Depth1 := Depth + 1,
	label_depth_ave({L,Dg1}, Labels),
	get_ave1(Dg1, Ch, SC, Labels, Depth1).


procedure count_tb_sc(DgTells, Count, CountOut, SC).

count_tb_sc(DgTells, Count, CountOut, SC) :-

    DgTells ? _T  |
	Count' := Count + 1,
	count_tb_sc;

    DgTells = [],
    SC = {L,R} :
      CountOut = Count,
      L = R .


procedure max(X,Y,Z).

max(X,Y,Z) :-

    X =< Y :
      Z = Y ;

    X > Y :
      Z = X .


procedure average(Done, AveM, Ave, Proc1, Dg).

average(Done, AveM, Ave, Proc1, Dg) :-
	wait_done(Done),
	average1(AveM, Ave, Proc1, Dg).

procedure average1(AveM, Ave, Proc1, Dg).
procedure average1(AveM, Ave, Proc1, Dg, Sum, Cs).

average1(AveM, Ave, Proc1, Dg) + (Sum=0,Cs=0) :-

    AveM ? Depth, integer(Depth) |
    Cs' := Cs + 1,
%	computation # display(term, {Depth,Sum,Cs},[type(ground)]),
	Sum' := Sum + Depth,
	average1;

    AveM = [] : Proc1 = _, Dg = _ |
	Ave := Sum*1000/Cs.


procedure wait_done(Done).

wait_done(Done) :-
    Done = done(Ch) :
       close_channel(Ch) .
