/* $Header: /home/qiana/Repository/Logix/system/ndg/phase2/ctl_pe/phase2_dic.cp,v 1.1 1999/07/09 07:03:02 bill Exp $ */
-language(compound).
-export([dic/4,add_i/5,add_ch_i/7,add_child/4,look_child/5,look/3]).
-mode(trust).

procedure dic(Req, Reg, IterLabel, SC).

dic(Req, Reg, IterLabel, SC) :-

    Req ? Req1, Req1 = iterative_label : IterLabel = Req1 |
%	screen#display({2,Req1,Reg},type(ground)),
	dic(Req', Reg, _, SC) ;

    Req ? update_max_reg(New),
    SC = {Left, Right} :
      SC' = {Left', Right} |
	update_reg(New, Reg, Reg', {Left, Left'}),
	self;

    Req ? Rq, Rq =\= iterative_label, Rq =\= update_max_reg(_),
    SC = {L,R} : SC' = {M,R} |
        dic1(Rq, Reg, Reg', {L,M}),
	dic ;

    Req = [], Reg = {Start,High}, SC = {L,R} : 
      IterLabel = body_label, High = Start, L = R.

dic1(Req, Reg, RegO, SC)
:-
    Req = get_reg(R, {{Dic, Regs}, DicOut}),
    Regs' := Regs + 1 :
      DicOut = {Dic, Regs'},
      R = a(Regs') |
	update_reg(Regs', Reg, RegO, SC) ;

    Req = get_2_regs(R1, R2, {Dic, Regs}, DicOut),
    Regs' := Regs + 1,
    Regs'' := Regs' + 1 :
      DicOut = {Dic, Regs''},
      R1 = a(Regs'),
      R2 = a(Regs'') |
	update_reg(Regs'', Reg, RegO, SC) ;

    Req = get_4_regs(R1, R2, R3, R4, {Dic, Regs}, DicOut),
    Regs1 := Regs + 1,
    Regs2 := Regs1 + 1,
    Regs3 := Regs2 + 1,
    Regs' := Regs3 + 1 :
      DicOut = {Dic, Regs'},
      R1 = a(Regs1),
      R2 = a(Regs2),
      R3 = a(Regs3),
      R4 = a(Regs') |
	update_reg(Regs', Reg, RegO, SC) ;

    Req = look(Key, Entry, {Dic, _Regs}),
    Key ? K,
    integer(K),
    Dic = {Int_list, _Tuple_list} : RegO = Reg, Key' = _ |
        look_i(Key, Entry, Int_list, _, SC) ;

    Req = look(Key, Entry, {Dic, _Regs}),
    Key ? K,
    tuple(K),
    Dic = {_Int_list, Tuple_list} : RegO = Reg, Key' = _ |
        look_t(Key, Entry, Tuple_list, SC) ;

    Req = look(Key, Entry, {Dic, _Regs}),
    tuple(Key),
    Dic = {_Int_list, Tuple_list} : RegO = Reg |
        look_t(Key, Entry, Tuple_list, SC) ;

    Req = update(Key, Entry, {{DicIn, RegsIn}, Dic}),
    Key ? K,
    integer(K),
    DicIn = {Int_list, Tuple_list} : RegO = Reg, Key' = _,
      Dic = {{Int_list_out, Tuple_list}, RegsIn} |
	update_i_e(Key, Entry, Int_list, Int_list_out, SC) ;

    Req = update(Key, Entry, {{DicIn, RegsIn}, Dic}),
    Key ? K,
    tuple(K),
    DicIn = {Int_list, Tuple_list} : RegO = Reg, Key' = _,
      Dic = {{Int_list, Tuple_list_out}, RegsIn} |
	update_t_e(Key, Entry, Tuple_list, Tuple_list_out, SC);

    Req = update(Key, Entry, {{DicIn, RegsIn}, Dic}),
    tuple(Key),
    DicIn = {Int_list, Tuple_list} : RegO = Reg, 
      Dic = {{Int_list, Tuple_list_out}, RegsIn} |
	update_t_e(Key, Entry, Tuple_list, Tuple_list_out, SC);

    Req = add(Key, Entry, {{DicIn, RegsIn}, Dic}),
    Key ? K,
    integer(K),
    DicIn = {Int_list, Tuple_list} : RegO = Reg, Key' = _,
      Dic = {{Int_list_out, Tuple_list}, RegsIn} |
	add_i(Key, Entry, Int_list, Int_list_out, SC);

    Req = add(Key, Entry, {{DicIn, RegsIn}, Dic}),
    Key ? K,
    tuple(K),
    DicIn = {Int_list, Tuple_list} : RegO = Reg, Key' = _,
      Dic = {{Int_list, Tuple_list_out}, RegsIn} |
	add_t(Key, Entry, Tuple_list, Tuple_list_out, SC) ;

    Req = add(Key, Entry, {{DicIn, RegsIn}, Dic}),
    tuple(Key),
    DicIn = {Int_list, Tuple_list} : RegO = Reg, 
      Dic = {{Int_list, Tuple_list_out}, RegsIn} |
	add_t(Key, Entry, Tuple_list, Tuple_list_out, SC) ;

    Req = update_type(Key, Type, {{DicIn, RegsIn}, Dic}),
    Key ? K,
    integer(K),
    DicIn = {Int_list, Tuple_list} : RegO = Reg, Key' = _,
      Dic = {{Int_list_out, Tuple_list}, RegsIn} |
	update_i_t(Key, Type, Int_list, Int_list_out, SC);

    Req = update_type(Key, Type, {{DicIn, RegsIn}, Dic}),
    Key ? K,
    tuple(K),
    DicIn = {Int_list, Tuple_list} : RegO = Reg, Key' = _,
      Dic = {{Int_list, Tuple_list_out}, RegsIn} |
	update_t_t(Key, Type, Tuple_list, Tuple_list_out, SC) ;

    Req = update_type(Key, Type, {{DicIn, RegsIn}, Dic}),
    tuple(Key),
    DicIn = {Int_list, Tuple_list} : RegO = Reg, 
      Dic = {{Int_list, Tuple_list_out}, RegsIn} |
	update_t_t(Key, Type, Tuple_list, Tuple_list_out, SC).

look(Key, Entry, Dic)
:-
	Dic = {DicI, _}, Key ? K,	integer(K),
	DicI = {Int_list, _} : Key' = _ |
        look_i(Key, Entry, Int_list, _, {_,_}) ;

	Dic = {DicI, _}, Key ? K,	tuple(K),
	DicI = {_, Tuple_list} : Key' = _ |
        look_t(Key, Entry, Tuple_list, {_,_}) ;

	Dic = {DicI, _}, tuple(Key),	DicI = {_, Tuple_list} |
        look_t(Key, Entry, Tuple_list, {_,_}).


update_reg(New, Reg, RegO, SC)
:-
	SC = {Left, Right}, Reg = {High,Out},	New > High :
        RegO = {New,Out}, Right = Left;

	SC = {Left, Right}, Reg = {High,_},	New =< High :
        RegO = Reg, Right = Left.

look_child(Key, Child, Dic, Entry, ChKey) 
:-
    Key ? K, Dic = {Dic1,_},
    integer(K),
    Dic1 = {Int_list, _Tuple_list} : Key' = _ |
        look_ch_i(Key, Child, Int_list, Entry, ChKey) ;

    Key ? K, Dic = {Dic1,_},
    tuple(K),
    Dic1 = {_Int_list, Tuple_list} : Key' = _ |
        append(Key, [Child], ChKey),
        look_ch_t(ChKey, Tuple_list, Entry).

add_child(Key, Child, Entry, Dic) 
:-
    Key ? K, Dic = {{Dic1,Reg},DicOut},
    integer(K),     Dic1 = {Int_list, Tuple_list} : 
    DicOut = {{Int_listO,Tuple_list},Reg}, Key' = _ |
        add_ch_i(Key, Child, Entry, _, Int_list, Int_listO, {_,_}) ;

    Key ? K, Dic = {{Dic1,Reg},DicOut},
    tuple(K),     Dic1 = {Int_list, Tuple_list} : 
    DicOut = {{Int_list,Tuple_listO},Reg}, Key' = _ |
        append(Key, [Child], ChKey),
        add_ch_t(ChKey, Entry, Tuple_list, Tuple_listO).

add_ch_t(Key, Entry, Dic, Dicout)
:-
   Dic ? {K,E},   Key @< K      :      Dicout ! {K,E} |         add_ch_t ;  
   Dic = [{K,_E}|_],   K @< Key   :     Dicout = [{Key,Entry}|Dic] ;
   Dic = [] :      Dicout = [{Key,Entry}].
     
look_ch_i(Key, Child, Dic, Entry, ChKey) 
:-
   Key ? X,   Dic ? {K,_,_},   X < K         :
   Key' = _ |
         look_ch_i(Key, Child, Dic', Entry, ChKey) ;

   Key ? X,   Dic ? {K,_,_},   X > K :
   ChKey ! X, Entry = {_,_,new,_}, Dic' = _ |
         append(Key', [Child], ChKey') ;

   Key ? K,   Dic = [{K,_,Dic'}|_] :
   ChKey ! K |
         look_ch_i;

   Key = [] : ChKey = [Child]|
         look_ch(Child, Dic, Entry) .


look_ch(Key, Dic, Entry) 
:-
	Dic = [{Key,E,_}|_] : Entry = E;
	Dic ? {K,_,_}, Key < K | look_ch;
	Dic = [{K,_,_}|_], Key > K : Entry = {_,_,new,_};
	Dic = [] : Entry = {_,_,new,_}, Key = _.

look_ch_t(Key, Dic, Entry) 
:-
	Dic = [{Key,E}|_] : Entry = E;
	Dic ? {K,_}, Key @< K | look_ch_t;
	Dic = [{K,_}|_], K @< Key : Entry = {_,_,new,_};
	Dic = [] : Entry = {_,_,new,_}, Key = _.

append(Xs, Ys, Zs) 
:- 
	Xs ? X : Zs ! X | append ;
	Xs = [] : Zs = Ys.


add_ch_i(Key, Child, Entry, ChKey, Dic, DicOut, SC) 
:-
	Key ? X, Dic ? D, D = {K,_,_}, X < K : 
        DicOut ! D, Key' = _ |
        add_ch_i(Key, Child, Entry, ChKey, Dic', DicOut', SC) ;

	
	Key ? K, Dic ? {K,E,Ch} :
        DicOut = [{K,E,DicOut'}|Dic'], Dic'' = Ch, ChKey ! K |
        add_ch_i ;

	Key = [] : ChKey = [Child] |
        add_ch(Child, Entry, Dic, DicOut, SC).

add_ch(Key, Entry, Dic, DicOut, SC)
:-
	Dic ? {K,_,_}, Key < K | add_ch ;

	Dic = [{K,_,_}|_], Key > K, SC = {L,R} : 
        DicOut = [{Key,Entry,[]}|Dic], L = R ;

	Dic = [], SC = {L,R} :
        DicOut = [{Key,Entry,[]}], L = R.


%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

procedure add_i(Key, Entry, Dic, Dicout, SC).

add_i(Key, Entry, Dic, Dicout, SC) :-

   Key ? X,
   Dic ? D,
   D = {K,_E,_Ch},
   X < K           :
      Dicout ! D,
      Key' = _       |

         add_i(Key, Entry, Dic', Dicout', SC) ;  


   Key = [X],
   Dic ? D,
   D = {K,_E,_Ch},
   X > K,
   SC = {L,R}      :
      Dicout = [{X,Entry,[]}|Dic],
      L = R,
      Dic' = _ ;


   Key ? K,
   Dic ? {K,E,Ch} :
      Dicout = [{K,E,Dicout'}|Dic'],
      Dic'' = Ch                     |

         add_i ;


   Dic = [],
   Key = [K],
   SC = {L,R} :
      Dicout = [{K,Entry,[]}],
      L = R .
     
%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

procedure add_t(Key, Entry, Dic, Dicout, SC).

add_t(Key, Entry, Dic, Dicout, SC) :-

   Dic ? {K,E},
   Key @< K      :
      Dicout ! {K,E} |

         add_t ;  


   Dic ? {K,_E},
   K @< Key,
   SC = {L,R}    :
      Dicout = [{Key,Entry}|Dic],
      L = R,
      Dic' = _ ;


   Dic = [],
   SC = {L,R} :
      Dicout = [{Key,Entry}],
      L = R .
     

%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

procedure look_i(Key, Entry, Dic, Ent, SC).

look_i(Key, Entry, Dic, Ent, SC) :-

   Key ? X,
   Dic ? {K,_E,_Ch},
   X < K         :
      Key' = _ |

         look_i(Key, Entry, Dic', Ent, SC) ;


   Key ? X,
   Dic ? {K,_E,_Ch},
   X > K,
   SC = {L,R}    :
      Entry = {_A,_Val,new,_T},
      L = R,
      Key' = _,
      Dic' = _,
      Ent = _ ;


   Key ? K,
   Dic ? {K,E,Ch} :
      Dic' = _,
      Ent = _       |
 
%         screen#display(Dic,prefix('Dic is')),
%         screen#ask(Entry,_,prefix('Entry is ')),
         look_i(Key', Entry, Ch, E, SC) ;


   Key = [],
   SC = {L,R} :
      Entry = Ent,
      L = R,
      Dic = _ ;


   Dic = [],
   Key =\= [],
   SC = {L,R}  :
      Entry = {_A,_Val,new,_T},
      L = R,
      Ent = _ .
 
%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

procedure look_t(Key, Entry, Dic, SC).

look_t(Key, Entry, Dic, SC) :-

   Dic ? {K,_E},
   Key @< K      |

         look_t ;


   Dic ? {K,_E},
   K @< Key,
   SC = {L,R}    :
      Entry = {_A,_Val,new,_T},
      L = R,
      Dic' = _ ;


   Dic ? {Key,E},
   SC = {L,R}     :
      Entry = E,
      L = R,
      Dic' = _ ;


   Dic = [],
   SC = {L,R}    :
      Entry = {_A,_Val,new,_T},
      L = R,
      Key = _ .

%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

update_i_e(Key, Entry, Dic, Dicout, SC)
:-
   Key = [X|_],
   Dic ? {K,E,Ch},
   X < K                 :
      Dicout ! {K,E,Ch} |
         update_i_e ;


   Key ? K, Key' = [],
   Dic ? {K,_,Ch}, SC = {L,R} :            
      Dicout = [{K,Entry,Ch}|Dic'], L = R ;

   Key ? K, Key' =\= [],
   Dic ? {K,E,Dic''} :
      Dicout = [{K,E,Dicout'}|Dic'] |
      update_i_e.

%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

procedure update_i_t(Key, Type, Dic, Dicout, SC).

update_i_t(Key, Type, Dic, Dicout, SC) :-

   Key = [X|_],
   Dic ? {K,E,Ch},
   X < K                : 
      Dicout ! {K,E,Ch}   |

         update_i_t ;


   Key ? K,
   Key' = [],
   Dic ? {K,E,Ch},
   E = {Areg,Vreg,_Type,Val},
   SC = {L,R}                 :
      Dicout = [{K,{Areg,Vreg,Type,Val},Ch}|Dic'],
      L = R ;


   Key ? K,
   Key' =\= [],
   Dic ? {K,E,Dic''} :
      Dicout = [{K,E,Dicout'}|Dic'] |

         update_i_t .

%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

procedure update_t_e(Key, Entry, Dic, Dicout, SC).

update_t_e(Key, Entry, Dic, Dicout, SC) :-

   Dic ? {K,E},
   Key @< K      :
      Dicout ! {K,E} |

         update_t_e ;


   Dic ? {Key,_E},
   SC = {L,R}      :
      Dicout = [{Key,Entry}|Dic'],
      L = R .

%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

procedure update_t_t(Key, Type, Dic, Dicout, SC).

update_t_t(Key, Type, Dic, Dicout, SC) :-

   Dic ? {K,E},
   Key @< K      :
      Dicout ! {K,E} |

         update_t_t ;


   Dic ? {Key,{Areg,Vreg,_Type,Val}},
   SC = {L,R}      :
      Dicout = [{Key,{Areg,Vreg,Type,Val}}|Dic'],
      L = R .

%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      

