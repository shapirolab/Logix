/* $Header: /home/qiana/Repository/Logix/system/ndg/phase2/ctl_pe/build_dic.cp,v 1.1.1.1 1999/07/09 07:03:02 bill Exp $ */
-language(compound).
-export([build_call_dic/7]).
-mode(trust).

procedure build_call_dic(Body, N, Iterate, Dic, CallDic, Ch, SC).

build_call_dic(Body, N, Iterate, Dic, CallDic, Ch, SC)
:-
	Body = {'=',_,_}, SC = {L,R} : CallDic = {[],[]}, 
	L = R, N = _, Iterate = _, Dic = _, Ch = _ ;

	Body =\= {'=',_,_}, Ch = {Ch1,_} : 
        write_channel(opt(Opt),Ch1) |
        build_call_dic1(Body, N, Iterate, Dic, CallDic, Ch, SC, Opt).
 
build_call_dic1(Body, N, Iterate, Dic, CallDic, Ch, SC, Opt)
:-
	Opt = 100, SC = {L,R} : CallDic = {[],[]}, L = R,
	Body = _, N = _, Iterate = _, Dic = _, Ch = _ ;

	Opt < 100,  arg(1,Body,P), N1 := N - 1, Ch = {Ch1,_} :
	write_channel(entries({P,N1,0},DicEnts),Ch1) |
        build_call_dic2(Body, N, Iterate, Dic, CallDic, {DicEnts,Opt}, Ch1, SC).
	
build_call_dic2(Body, N, Iterate, Dic, CallDic, Opt, Ch, SC)+
                                             (DicAcc = [],I=1)
:-
	I < N, SC = {L,R}, I' := I + 1,	arg(I',Body,ArgN)  : SC' = {M,R} |
	get_ent([I], Opt, St),
	build_arg1(St, ArgN, [I], Iterate, a(I), DicAcc, DicAcc', Dic, Opt, 
							Ch, {L,M}),
	I' := I + 1,
	build_call_dic2 ;

	I = N, SC = {L,R} : 
        CallDic = {DicAcc,[]}, L = R, 
	Body = _, Dic = _, Ch = _, Iterate = _, Opt = _.

build_arg1(St, Arg, Psi, Iterate, Base, DicAcc, DicAccO, Dic, 
							Opt, Ch, SC)
:-
	St = in |
	build_arg(Arg, Psi, Iterate, Base, DicAcc, DicAccO, Dic, Opt,
								Ch, SC) ;

	St = out, Base = a(_) : Arg = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Psi,{Base,'*',ref,'*'},DicAcc,DicAccO, SC) ;

	St = out, Base = '*', SC = {L,R} :
	DicAccO = DicAcc, L = R,
	Arg = _, Psi = _, Iterate = _, Base = _, 
	Dic = _, Opt = _, Ch = _, Opt = _.

build_arg(Arg, Psi, Iterate, Base, CallDic, CallDicO, Dic, Opt, Ch, SC)
:-
	Arg = integer(I) : Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Psi,{Base,'*',integer,I},CallDic,CallDicO, SC) ;

	Arg = string(I) : Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Psi,{Base,'*',string,I},CallDic,CallDicO, SC) ;

	Arg = nil(_) : Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Psi,{Base,'*',nil,'*'},CallDic,CallDicO, SC) ;

	Arg = real(_) : Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Psi,{Base,'*',real,'*'},CallDic,CallDicO, SC) ;

	Arg = variable('_') : Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Psi,{Base,'*',var,'*'},CallDic,CallDicO, SC) ;

	Arg = psi(Call) |
	phase2_dic#look(Call,Entry,Dic),
	build_entry(Call, Entry, Iterate, Base, Psi, 
				CallDic, CallDicO, Dic, Opt, Ch, SC) ;

	Arg = ro(_), Base = a(_) : Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Psi,{Base,'*',ref,'*'},CallDic,CallDicO, SC) ;

	Arg = ro(_), Base =\= a(_), SC = {L,R} :
	CallDic = CallDicO, L = R, 
	Psi = _, Iterate = _, Dic = _, Ch = _, Opt = _ ;

	Arg = tuple(Tup), SC = {L,R} |
        phase2_dic#add_i(Psi,{Base,'*',tuple,Ar},CallDic,CallDic', {L,M}),
	arity(Tup,Ar),
	build_call_tuple(Tup, Ar, Psi, Iterate, 
				CallDic', CallDicO, Dic, Opt, Ch, {M,R}) ;

	Arg = list(List), List = [Car|Cdr], SC = {L,R} |
        phase2_dic#add_i(Psi,{Base,'*',list,'*'},CallDic,CallDic', {L,M}),
	append(Psi, [1], KeyCar),
	append(Psi, [2], KeyCdr),
	get_ent(KeyCar,Opt,St1),
	get_ent(KeyCdr,Opt,St2),
	build_car_arg1(St1, Car, Psi, Iterate, CallDic', CallDic'', Dic, 
							Opt, Ch, {M,M1}),
	build_arg1(St2, Cdr, KeyCdr, Iterate, '*', CallDic'', CallDicO, Dic, 
							Opt, Ch, {M1,R}).

build_car_arg1(St, Car, Psi, Iterate, CallDic, CallDicO, Dic, Opt, Ch, SC)
:-
	St = in |
	build_car_arg(Car, Psi, Iterate, CallDic, CallDicO, Dic, Opt, 
									Ch, SC);

	St = out, SC = {L,R} :
	CallDicO = CallDic, L = R,
	Car = _, Psi = _, Iterate = _, Dic = _, Opt = _, Ch = _.


build_car_arg(Arg, Psi, Iterate, CallDic, CallDicO, Dic, Opt, Ch, SC)
:-
	Arg = integer(I) : Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_ch_i(Psi,1,
	                    {'*','*',car(integer),I},_,CallDic,CallDicO,SC);

	Arg = string(I) : Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_ch_i(Psi,1,
	                    {'*','*',car(string),I},_,CallDic,CallDicO,SC);

	Arg = nil(_) : Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_ch_i(Psi,1,{'*','*',car(nil),'*'},_,CallDic,CallDicO,SC);

	Arg = real(_) : Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_ch_i(Psi,1,
	                    {'*','*',car(real),'*'},_,CallDic,CallDicO,SC);

	Arg = variable('_') : Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_ch_i(Psi,1,{'*','*',car(var),'*'},_,CallDic,CallDicO,SC);
	
	Arg = psi(Call) |
	phase2_dic#look(Call,Entry,Dic),
	append(Psi,[1],PsiCar),
	build_car_entry(Call, Entry, Iterate, PsiCar, 
				CallDic, CallDicO, Dic, Opt, Ch, SC) ;

	Arg = ro(Ro), Ro = psi(Call) |
	phase2_dic#look(Call,Entry,Dic),
	append(Psi,[1],PsiCar),
	build_car_entry(Call, Entry, Iterate, PsiCar, 
				CallDic, CallDicO, Dic, Opt, Ch, SC) ;

	Arg = tuple(Tup), SC = {L,R} |
        phase2_dic#add_ch_i(Psi,1,
	                   {'*','*',car(tuple),Ar},Key,CallDic,CallDic',{L,M}),
	arity(Tup,Ar),
	build_call_tuple(Tup, Ar, Key, Iterate,
				CallDic', CallDicO, Dic, Opt, Ch, {M,R}) ;

	Arg = list(List), List = [Car|Cdr], SC = {L,R} |
        phase2_dic#add_ch_i(Psi,1,
	                   {'*','*',car(list),'*'},Key,CallDic,CallDic',{L,M}),
	append(Key,[1],KeyCar),
	append(Key,[2],KeyCdr),
	get_ent(KeyCar,Opt,St1),
	get_ent(KeyCdr,Opt,St2),
	build_car_arg1(St1, Car, Key, Iterate, CallDic', CallDic'', Dic, 
							Opt, Ch, {M,M1}),
	build_arg1(St2, Cdr, KeyCdr, Iterate, '*', CallDic'', CallDicO, Dic, 
							Opt, Ch, {M1,R}).

build_entry1(St, KeyCdr, EntryCdr, Iterate, Base, Callee, 
				CallDic, CallDicO, Dic, Opt, Ch, SC) 
:-
	St = in |
	build_entry(KeyCdr, EntryCdr, Iterate, Base, Callee, 
				CallDic, CallDicO, Dic, Opt, Ch, SC) ;

	St = out, SC = {L,R} :
	CallDicO = CallDic, L = R,
	KeyCdr = _, EntryCdr = _, Iterate = _, Base = _, 
	Callee = _, Dic = _, Opt = _, Ch = _.

build_entry(Call, Entry, Iterate, Base, Callee, 
					CallDic, CallDicO, Dic, Opt, Ch, SC)
:-
	Entry = {_,_,new,_}, Base = a(_) :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{Base,'*',ref,'*'},CallDic,CallDicO, SC) ;

	Entry = {_,_,new,_}, Base =\= a(_), SC = {L,R} :
	CallDic = CallDicO,
	Call = _, Callee = _, Iterate = _, Dic = _, Ch = _, Opt = _,L = R ;

	Entry = {_,_,ref,_}, Base = a(_) :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{Base,'*',ref,'*'},CallDic,CallDicO, SC) ;

	Entry = {_,_,ref,_}, Base =\= a(_), SC = {L,R} :
	CallDic = CallDicO,
	Call = _, Callee = _, Iterate = _, Dic = _, Ch = _, Opt = _,L = R ;

	Entry = {_,_,sub_arg,_}, Base = a(_) :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{Base,'*',ref,'*'},CallDic,CallDicO, SC) ;

	Entry = {_,_,sub_arg,_}, Base =\= a(_), SC = {L,R} :
	CallDic = CallDicO,
	Call = _, Callee = _, Iterate = _, Dic = _, Ch = _, Opt = _,L = R ;

	Entry = {_,_,cdr,_}, Base = a(_) :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{Base,'*',ref,'*'},CallDic,CallDicO, SC) ;

	Entry = {_,_,cdr,_}, Base =\= a(_), SC = {L,R} :
	CallDic = CallDicO,
	Call = _, Callee = _, Iterate = _, Dic = _, Ch = _, Opt = _,L = R ;

	Entry = {_,_,deref,_}, Base = a(_) :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{Base,'*',ref,'*'},CallDic,CallDicO, SC) ;

	Entry = {_,_,deref,_}, Base =\= a(_), SC = {L,R} :
	CallDic = CallDicO,
	Call = _, Callee = _, Iterate = _, Dic = _, Ch = _, Opt = _,L = R ;

	Entry = {_,_,nil,_} :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{Base,'*',nil,'*'},CallDic,CallDicO, SC) ;

	Entry = {_,_,string,S} :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{Base,'*',string,S},CallDic,CallDicO, SC) ;

	Entry = {_,_,number,_} :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{Base,'*',number,'*'},CallDic,CallDicO, SC) ;

	Entry = {_,'*',integer,V} :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{Base,'*',integer,V},CallDic,CallDicO, SC) ;

	Entry = {_,a(_),integer,V}, Base = a(_) :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{Base,Base,integer,V},CallDic,CallDicO, SC) ;

	Entry = {_,_,integer,V}, Base =\= a(_) :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{Base,'*',integer,V},CallDic,CallDicO, SC) ;

	Entry = {_,_,real,_} :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{Base,'*',real,'*'},CallDic,CallDicO, SC) ;

	Entry = {_,_,constant,_} :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{Base,'*',constant,'*'},CallDic,CallDicO, SC) ;

	Entry = {_,_,known,_} :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{Base,'*',known,'*'},CallDic,CallDicO, SC) ;

	Entry = {_,_,module,_} :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{Base,'*',known,'*'},CallDic,CallDicO, SC) ;

	Entry = {_,_,var,_}, Iterate = iterate :
	Call = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{Base,'*',var,'*'},CallDic,CallDicO, SC) ;

	Entry = {_,_,ref(var),_}, Iterate = iterate :
	Call = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{Base,'*',ref(var),'*'},CallDic,CallDicO, SC) ;

	Entry = {_,_,var,_}, Iterate = spawn, Base = a(_) :
	Call = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{Base,'*',ref,'*'},CallDic,CallDicO, SC) ;

	Entry = {_,_,ref(var),_}, Iterate = spawn, Base = a(_) :
	Call = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{Base,'*',ref,'*'},CallDic,CallDicO, SC) ;

	Entry = {_,_,var,_}, Iterate = spawn, Base =\= a(_), SC = {L,R} :
	CallDic = CallDicO,
	Call = _, Callee = _, Dic = _, Ch = _, Opt = _, L = R ;

	Entry = {_,_,ref(var),_}, Iterate = spawn, Base =\= a(_), SC = {L,R} :
	CallDic = CallDicO,
	Call = _, Callee = _, Dic = _, Ch = _, Opt = _, L = R ;

	Entry = {_,_,list,_}, Base = a(_), Callee = [I], SC = {L,R} |
        phase2_dic#add_i(Callee,{Base,'*',list,'*'},CallDic,CallDic',{L,M}),
	get_ent([I,1],Opt,St1),
	get_ent([I,2],Opt,St2),
	phase2_dic#look_child(Call,1,Dic,EntryCar,KeyCar),
	phase2_dic#look_child(Call,2,Dic,EntryCdr,KeyCdr),
	build_car_entry1(St1, KeyCar, EntryCar, Iterate, [I,1], 
				CallDic', CallDic'', Dic, Opt, Ch, {M,M1}),
	build_entry1(St2, KeyCdr, EntryCdr, Iterate, '*', [I,2], 
				CallDic'', CallDicO, Dic, Opt, Ch, {M1,R}) ;

	Entry = {_,_,list,_}, Base =\= a(_) :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{Base,'*',list,'*'},CallDic,CallDicO, SC) ;

	Entry = {_,_,tuple,'*'} :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{Base,'*',tuple,'*'},CallDic,CallDicO, SC) ;

	Entry = {_,_,tuple,Ar}, Base =\= a(_) :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{Base,'*',tuple,Ar},CallDic,CallDicO, SC) ;

	Entry = {_,_,tuple,Ar}, Base = a(_), integer(Ar), SC = {L,R} |
        phase2_dic#add_i(Callee,{Base,'*',tuple,Ar},CallDic,CallDic',{L,M}),
	build_entry_tuple(Ar, Call, Iterate, Callee, 
				CallDic', CallDicO, Dic, Opt, Ch, {M,R}) ;

	otherwise, Base = a(_) :
	Entry = _, Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{Base,'*',ref,'*'},CallDic,CallDicO, SC) ;

	otherwise, Base =\= a(_), SC = {L,R} :
	CallDic = CallDicO, L = R,
	Entry = _, Call = _, Callee = _, Iterate = _, Dic = _, Ch = _, Opt = _.

build_car_entry1(St, KeyCar, EntryCar, Iterate, Callee,	CallDic, CallDicO, Dic,
						 Opt, Ch, SC)
:-
	St = in |
	build_car_entry(KeyCar, EntryCar, Iterate, Callee, CallDic, CallDicO, 
						Dic, Opt, Ch, SC) ;

	St = out, SC = {L,R} :
	CallDicO = CallDic, L = R,
	KeyCar = _, EntryCar = _, Iterate = _, Callee = _,
	Dic = _, Opt = _, Ch = _, Opt = _.

build_car_entry(Call, Entry, Iterate, Callee, 
				CallDic, CallDicO, Dic, Opt, Ch, SC)
:-
	Entry = {_,_,new,_}, SC = {L,R} :
	CallDic = CallDicO,
	Call = _, Callee = _, Iterate = _, Dic = _, Ch = _, Opt = _,L = R ;

	Entry = {_,_,ref,_}, SC = {L,R} :
	CallDic = CallDicO,
	Call = _, Callee = _, Iterate = _, Dic = _, Ch = _, Opt = _,L = R ;

	Entry = {_,_,sub_arg,_}, SC = {L,R} :
	CallDic = CallDicO,
	Call = _, Callee = _, Iterate = _, Dic = _, Ch = _, Opt = _,L = R ;

	Entry = {_,_,cdr,_}, SC = {L,R} :
	CallDic = CallDicO,
	Call = _, Callee = _, Iterate = _, Dic = _, Ch = _, Opt = _,L = R ;

	Entry = {_,_,deref,_}, SC = {L,R} :
	CallDic = CallDicO,
	Call = _, Callee = _, Iterate = _, Dic = _, Ch = _, Opt = _,L = R ;

	Entry = {_,_,nil,_} :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{'*','*',car(nil),'*'},CallDic,CallDicO, SC) ;

	Entry = {_,_,string,_} :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{'*','*',car(string),'*'},CallDic,CallDicO,SC) ;

	Entry = {_,_,number,_} :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
%	screen#display({Callee,CallDic},type(ground)),
        phase2_dic#add_i(Callee,{'*','*',car(number),'*'},CallDic,CallDicO,SC) ;

	Entry = {_,_,integer,I} :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{'*','*',car(integer),I},CallDic,CallDicO,SC) ;

	Entry = {_,_,real,_} :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{'*','*',car(real),'*'},CallDic,CallDicO,SC) ;

	Entry = {_,_,constant,_} :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{'*','*',car(constant),'*'},
	                        CallDic,CallDicO,SC);

	Entry = {_,_,known,_} :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{'*','*',car(known),'*'},CallDic,CallDicO,SC) ;

	Entry = {_,_,module,_} :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{'*','*',car(module),'*'},CallDic,CallDicO,SC) ;

	Entry = {_,_,var,_}, Iterate = spawn, SC = {L,R} :
	CallDic = CallDicO,
	Call = _, Callee = _, Dic = _, Ch = _, Opt = _, L = R ;

	Entry = {_,_,var,_}, Iterate = iterate :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{'*','*',car(var),'*'},CallDic,CallDicO,SC) ;

	Entry = {_,_,ref(var),_}, Iterate = spawn, SC = {L,R} :
	CallDic = CallDicO,
	Call = _, Callee = _, Dic = _, Ch = _, Opt = _, L = R ;

	Entry = {_,_,ref(var),_}, Iterate = iterate :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{'*','*',car(var),'*'},CallDic,CallDicO,SC) ;

	Entry = {_,_,tuple,'*'} :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{'*','*',car(tuple),'*'},CallDic,CallDicO,SC) ;

	Entry = {_,_,tuple,Ar}, integer(Ar), SC ={L,R} |
        phase2_dic#add_i(Callee,{'*','*',car(tuple),Ar},CallDic,CallDic',{L,M}),
	build_entry_tuple(Ar, Call, Iterate, Callee, 
				CallDic', CallDicO, Dic, Opt, Ch, {M,R}) ;

	Entry = {_,_,list,'*'} :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{'*','*',car(list),'*'},CallDic,CallDicO,SC) ;

	Entry = {_,_,car(var),_}, Iterate = spawn, SC = {L,R} :
	CallDic = CallDicO,
	Call = _, Callee = _, Dic = _, Ch = _, Opt = _, L = R ;

	Entry = {_,_,car(var),_}, Iterate = iterate :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{'*','*',car(var),'*'},CallDic,CallDicO,SC) ;

	Entry = {_,_,car(Type),_}, Type =\= var :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{'*','*',car(Type),'*'},CallDic,CallDicO,SC) ;

	Entry = {_,_,car,_} :
	Call = _, Iterate = _, Dic = _, Ch = _, Opt = _ |
        phase2_dic#add_i(Callee,{'*','*',car,'*'},CallDic,CallDicO,SC) ;

	otherwise, SC = {L,R} :
	CallDic = CallDicO, L = R, 
	Entry = _, Call = _, Callee = _, Iterate = _, Dic = _, Ch = _, Opt = _.

build_entry_tuple(Ar, Call, Iterate, Callee, 
				CallDic, CallDicO, Dic, Opt, Ch, SC)
:-
	Ar > 0,  SC = {L,R} :
	SC' = {M,R} |
	append(Callee, [Ar], CalleeAr),
	get_ent(CalleeAr, Opt, St),
	phase2_dic#look_child(Call,Ar,Dic,Entry,KeyEnt),
	build_entry1(St, KeyEnt, Entry, Iterate, '*', CalleeAr, 
				CallDic, CallDic', Dic, Opt, Ch, {L,M}),
	Ar' := Ar - 1,
	build_entry_tuple ;

	Ar = 0, SC = {L,R} : CallDicO = CallDic, L = R,
	Call = _, Iterate = _, Callee = _, Dic = _, Ch = _, Opt = _.


build_call_tuple(Tup, Ar, Psi, Iterate, CallDic, CallDicO, Dic, Opt, Ch, SC)
								+(N=1)
:-
	N =< Ar, arg(N,Tup,ArgN), SC = {L,R} : SC' = {M,R} |
	N' := N + 1, 
	append(Psi,[N],Key),
	get_ent(Key, Opt, St), 
	build_arg1(St, ArgN, Key, Iterate, '*', 
				CallDic, CallDic', Dic, Opt, Ch, {L,M}),
	build_call_tuple ;

	N > Ar, SC = {L,R} :
	CallDicO = CallDic, L = R,
	Tup = _, Psi = _, Dic = _, Iterate = _, Ch = _, Opt = _.

append(Xs,Ys,Zs)
:-
	Xs ? X : Zs ! X | append ;
        Xs = [] : Zs = Ys.

get_ent(Psi, Opt, St)
:-
	Opt = {{Case,Ask,_,_},_}, Case = [], Ask = [] : St = in, Psi = _ ;

	Opt = {{Case,Ask,Tell,Body},Prec}, Case =\= []  |
	get_entry(Psi, Case, CaseP),
	get_entry(Psi, Ask, AskP),
	get_entry(Psi, Tell, TellP),
	get_entry(Psi, Body, BodyP),
	comp_st(Prec, CaseP, AskP, TellP, BodyP, St) ;

	Opt = {{Case,Ask,Tell,Body},Prec}, Ask =\= []  |
	get_entry(Psi, Case, CaseP),
	get_entry(Psi, Ask, AskP),
	get_entry(Psi, Tell, TellP),
	get_entry(Psi, Body, BodyP),
	comp_st(Prec, CaseP, AskP, TellP, BodyP, St).

get_entry(Psi, List, Entry)
:-
	List = [{psi(Psi),E}|_] : Entry = E ;
	List ? {psi(Psi1),_}, Psi =\= Psi1 | get_entry ;
	List = [] : Entry = 0, Psi = _.

comp_st(Prec, CaseP, AskP, TellP, BodyP, St)
:-
	CaseP + AskP + TellP + BodyP >= Prec : St = in ; 
	CaseP + AskP + TellP + BodyP < Prec : St = out.

