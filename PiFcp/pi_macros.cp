-language(compound).
-export([transform/2, transform_and_wait/3,
	 pi2cmp/3, pi2fcp/3]).
-mode(failsafe).

pi2cmp(Name, Options, Results) :-
    string_to_dlist(Name,Fp,[46, 99, 109, 112]),
    list_to_string(Fp,FN) |
	get_source#file(Name,[language([evaluate,pifcp])|Options],R,_),
	transform2.

pi2fcp(Name, Options, Results) :-
    string_to_dlist(Name,Fp,[46, 102, 99, 112]),
    list_to_string(Fp,FN) |
	get_source#file(Name,[language([evaluate,pifcp,compound,colon]) |
				Options],
			R,_),
	transform2.

transform2(FN,Results,R) :-
    R = module(O,A,S) |
	transform#languages(O,dg,A,_AO,S,SO,Results,Done),
	widgets#pretty#module(SO,SP),
	file#put_file(FN, SP, put, Done);
    otherwise :
      FN = _,
      Results = R.


transform(Name, Results) :-
    string_to_dlist(Name, Fp, [46, 112, 105]),
    list_to_string(Fp,PN),
    string_to_dlist(Name, Fc, [46, 99, 112]),
    list_to_string(Fc,FN) |
	file#get_file(PN, S, [], FGR),
	report_pifcp.

transform_and_wait(Name, Reply, Results) :-
	transform(Name, Results),
	transformed.

transformed(Name, Results, Reply) :-

    Results =?= (_FileName : written) |
      Reply = Name;

    otherwise :
      Name = _,
      Results = _,
      Reply = "_".

/* Manage output for pifcp to fcp transformation. */

report_pifcp(PN, FN, FGR, S, Results) :-

    FGR =?= true :
      PN = _ |
	parse#string(S, TI, PSE),
	report_pifcp_parsed;

    otherwise :
      S = _,
      FN = _,
      Results = (PN : FGR).

report_pifcp_parsed(FN, PSE, TI, Results) :-

    PSE = [] |
	pifcp#transform([],TI,EX,TO,PTE),
	report_pifcp_to_compound;

    otherwise :
      TI = _,
      Results = [FN - "parsing errors:" | PSE].

report_pifcp_to_compound(FN, PTE, EX, TO, Results) :-

    PTE = [] |
        transform#languages([], dg, [language(compound)],_AT,TO,IO,TLE,[]),
	report_pifcp_transformed;

    otherwise :
      EX = _,
      TO = _,
      Results = [FN - "pifcp transformation errors:" | PTE].


report_pifcp_transformed(FN, TLE, EX, IO, Results) :-

    TLE = [] |
	handle_exports,
	widgets#pretty#module(EXIO,SP),
	file#put_file(FN, SP, put, FPR),
	report_pifcp_written;

    otherwise :
      EX = _,
      IO = _,
      Results = [FN - "compound transformation errors:" | TLE].

  handle_exports(EX, IO, EXIO) :-

    EX ? export(Es), Es =\= [] :
      EX' = [],
      EXIO = [-export(Es) | IO];

    EX ? exports(_),
    otherwise |
	self;

    EX = [] :
      EXIO = IO.


report_pifcp_written(FN, FPR, Results) :-

    FPR =?= true :
      Results = (FN : written);

    otherwise :
      FN = _,
      Results = FPR.


