/* $Header: /home/qiana/Repository/Logix/system/ndg/phase2/ctl_pe/ask_guard.cp,v 1.1 1999/07/09 07:03:03 bill Exp $ */
-language(compound).
-export([guard/9]).
-mode(trust).

procedure guard(Test, Labels, BH, Dic, Ctl, Ch, Name, SC, Done).

guard(Test, Labels, BH, Dic, Ctl, Ch, Name, SC, Done)
:-
	Test = {Tp,psi(Psi)}, Ch = {_,Ch1} :
	write_channel(look(Psi,Entry,Dic),Ch1) |
	   ask_type_check#type_check(Tp, Psi, Entry, 
				Labels, BH, Dic, Ctl, Ch, Name, SC, Done) ;

	Test = {Pred,Psi1,Psi2}, Pred = '=:=', Ch = {_,Ch1} |
	   get_entry(Psi1, Entry1, Dic, Ch1),
	   get_entry(Psi2, Entry2, Dic, Ch1),
           ask_binary#binary_arith(Pred, Psi1, Entry1, Psi2, Entry2, 
			Labels, BH, Dic, Ctl, Ch, Name, SC, Done) ;

	Test = {Pred,Psi1,Psi2}, Pred = '=<', Ch = {_,Ch1} |
	   get_entry(Psi1, Entry1, Dic, Ch1),
	   get_entry(Psi2, Entry2, Dic, Ch1),
           ask_binary#binary_arith(Pred, Psi1, Entry1, Psi2, Entry2, 
			Labels, BH, Dic, Ctl, Ch, Name, SC, Done) ;

	Test = {Pred,Psi1,Psi2}, Pred = '>=', Ch = {_,Ch1} |
	   get_entry(Psi1, Entry1, Dic, Ch1),
	   get_entry(Psi2, Entry2, Dic, Ch1),
           ask_binary#binary_arith(Pred, Psi1, Entry1, Psi2, Entry2, 
			Labels, BH, Dic, Ctl, Ch, Name, SC, Done) ;

	Test = {Pred,Psi1,Psi2}, Pred = '<', Ch = {_,Ch1} |
	   get_entry(Psi1, Entry1, Dic, Ch1),
	   get_entry(Psi2, Entry2, Dic, Ch1),
           ask_binary#binary_arith(Pred, Psi1, Entry1, Psi2, Entry2, 
			Labels, BH, Dic, Ctl, Ch, Name, SC, Done) ;

	Test = {Pred,Psi1,Psi2}, Pred = '>', Ch = {_,Ch1} |
	   get_entry(Psi1, Entry1, Dic, Ch1),
	   get_entry(Psi2, Entry2, Dic, Ch1),
           ask_binary#binary_arith(Pred, Psi1, Entry1, Psi2, Entry2, 
			Labels, BH, Dic, Ctl, Ch, Name, SC, Done) ;

	Test = {Pred,Psi1,Psi2,Psi3}, Pred = diff, Ch = {_,Ch1} |
	   get_entry(Psi1, Entry1, Dic, Ch1),	   	
	   get_entry(Psi2, Entry2, Dic, Ch1),
	   get_entry(Psi3, Entry3, Dic, Ch1),
	   get_types3(Entry1, Entry2, Entry3, Types),
	   ask_arithmetic#arithmetic(Types, Pred, 
			Psi1, Entry1, Psi2, Entry2, Psi3, Entry3,
			Labels, BH, Dic, Ctl, Ch, Name, SC, Done) ;

	Test = {Pred,Psi1,Psi2,Psi3}, Pred = plus, Ch = {_,Ch1} |
	   get_entry(Psi1, Entry1, Dic, Ch1),	   	
	   get_entry(Psi2, Entry2, Dic, Ch1),
	   get_entry(Psi3, Entry3, Dic, Ch1),
	   get_types3(Entry1, Entry2, Entry3, Types),
	   ask_arithmetic#arithmetic(Types, Pred, 
			Psi1, Entry1, Psi2, Entry2, Psi3, Entry3,
			Labels, BH, Dic, Ctl, Ch, Name, SC, Done) ;

/*
	Test = arity(Psi1,Psi2), Ch = {_,Ch1} |
	   get_entry(Psi1, Entry1, Dic, Ch1),
	   get_entry(Psi2, Entry2, Dic, Ch1),
	   ask_arity#arity(Psi1, Entry1, Psi2, Entry2, 
			Labels, BH, Dic, Ctl, Ch, Name, SC, Done) ;
*/

	otherwise |
	   other_guards(Test, Labels, BH, Dic, Ctl, Ch, Name, SC, Done).

get_entry(Psi, Entry, Dic, Ch)
:-
	Psi = integer(I) : Entry = {I,I,integer,I}, Dic = _, Ch = _;
	Psi = real(I) : Entry = {I,I,real,I}, Dic = _, Ch = _;
	Psi = psi(P) : write_channel(look(P,Entry,Dic),Ch).

get_types3(Entry1, Entry2, Entry3, Types)
:-
	Entry3 = {_,_,new,_} | get_types2(Entry1, Entry2, Types) ;

	Entry3 =\= {_,_,new,_} : Types = guard, Entry1 = _, Entry2 = _|
        screen#display({
               'WARNING','phase2#ask_guard','a connected third argument in
                        an arithmetic guard'}).

get_types2(Entry1, Entry2, Types)
:-
	Entry1 = {_,_,T1,_}, Entry2 = {_,_,T2,_} |
	member_types([T1,T2], {ok,F1}, false,	
		[list,nil,string,tuple,compound,
		car(list),car(nil),car(string),car(tuple),car(compound)]),
	member_types([T1,T2], {F1,F2}, ow, [var,ref(var),car(var)]), 
	member_types([T1,T2], {F2,Types}, guard, 
		[ref,car,cdr,sub_arg,deref,real,number,constant,known,
		 car(real),car(number),car(constant),car(known)]).

member_types(Ts, SC, Token, Ms)
:-
	SC = {L,R}, L =\= ok : R = L, Ms = _, Ts = _, Token = _ ;
	Ts ? T, SC = {L,R}, L = ok : SC' = {M,R} | 
		member_type(T, Ms, Token, {L,M}), 
		member_types ;
	Ts = [], SC = {L,R} : L = R, Token = _, Ms = _.

member_type(T, Ms, Token, SC)
:-
	Ms = [T|_], SC = {_,L} : L = Token;
	Ms ? T1, T =\= T1 | member_type;
	Ms = [], SC = {L,R} : L = R, T = _, Token = _.

other_guards(Test, Labels, BH, Dic, Ctl, Ch, Name, SC, Done)
:-
	Labels = [true(True),false(False),ow(Ow)], 
	Ctl = CtlH\CtlT, SC = {L,R}, Done = {DL,DR}, Ch = {_,Ch1} |
	   arity(Test,N),
	   arg(1,Test,Pred),
%	screen#display({Pred,Args,Dic},type(ground)),
	   create_args(Test, N, {Dic,Dic1}, Ch1, Args,
	       CtlH\[call(Pred,Args,[label(Fl),label(Sus)])|CtlM], {L,M}),
	   get_cont(Ow, BH, BH'),						
	   ctl#ctl1(True, BH', Dic1, CtlM\[label(Fl)|CtlM1], Ch, Name, 
							{M,M1}, {DL,DM1}),
	   ctl#ctl1(False, BH', Dic1, CtlM1\[label(Sus)|CtlM2], 
						Ch, Name, {M1,M2}, {DM1,DM2}),
	   ctl#ctl1(Ow, BH, Dic1, CtlM2\CtlT, Ch, Name, {M2,R}, {DM2,DR}).

get_cont(Ow, BH, BHO)
:-
	Ow = {Lab,_}, Lab = label(_), BH = {B,H,_} : BHO = {B,H,goto(Lab)} ;
	Ow =\= {label(_),_} : BHO = BH.

create_args(Test, Ar, Dic, Ch, Args, Ctl, SC)+(N=2)
:-
	Ar >= N, arg(N,Test,Arg), 
	Dic = {DicIn,DicOut}, Ctl =CtlH\CtlT, SC = {L,R} :
	Args ! CtlArg, Dic' = {DicIn',DicOut}, 
	Ctl' = CtlM\CtlT, SC' = {M,R} |
	N' := N + 1,
	ask_term_creation#term_creation(Arg, {DicIn,DicIn'}, 
					           CtlArg, CtlH\CtlM, Ch, {L,M}),
	create_args ;

	N  > Ar, Dic = {DicIn,DicOut}, 	Ctl = CtlH\CtlT, SC = {L,R} :
	Args = [], DicOut = DicIn, CtlH = CtlT,	L = R, 	Test = _, Ch = _.

