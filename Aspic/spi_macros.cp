-language(compound).
-export([transform/2, transform_and_wait/3,
	 spi2cmp/3, spi2fcp/3]).
-mode(failsafe).

spi2cmp(Name, Attributes, Results) :-
    string_to_dlist(Name,Fp,[46, 99, 109, 112]),
    list_to_string(Fp, FN) |
	get_source#file(Name, [], R, _),
	transform2 + (Target = compound).

spi2fcp(Name, Attributes, Results) :-
    string_to_dlist(Name,Fp,[46, 102, 99, 112]),
    list_to_string(Fp, FN) |
	get_source#file(Name, [], R, _),
	transform2 + (Target = dg).

transform2(FN, Attributes, R, Target, Results) :-
    R = module(O, A, S) |
	concatenate(Attributes, A, AI),
	transform#languages(O, Target, AI, _AO, S, SO, Results, Done),
	widgets#pretty#module(SO,SP),
	file#put_file(FN, SP, put, Done);
    otherwise :
      FN = _,
      Attributes = _,
      Target = _,
      Results = R.

  concatenate(L1, L2, L3) :-

    L1 ? I :
      L3 ! I |
	self;

    L1 =?= [] :
      L3 = L2;

    L1 =\= [_|_], L1 =\= [] :
      L1' = [L1] |
	self.


transform(Name, Results) :-
    string_to_dlist(Name, Fp, [46, 112, 105]),
    list_to_string(Fp,PN),
    string_to_dlist(Name, Fc, [46, 99, 112]),
    list_to_string(Fc,FN) |
	file#get_file(PN, S, [], FGR),
	report_spifcp.

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

/* Manage output for spifcp to fcp transformation. */

report_spifcp(PN, FN, FGR, S, Results) :-

    FGR =?= true :
      PN = _ |
	parse#string(S, TI, PSE),
	report_spifcp_parsed;

    otherwise :
      S = _,
      FN = _,
      Results = (PN : FGR).

report_spifcp_parsed(FN, PSE, TI, Results) :-

    PSE = [] |
	spifcp#transform([],TI,EX,TO,PTE),
	report_spifcp_to_compound;

    otherwise :
      TI = _,
      Results = [FN - "parsing errors:" | PSE].

report_spifcp_to_compound(FN, PTE, EX, TO, Results) :-

    PTE = [] |
        transform#languages([], dg, [language(compound)],_AT,TO,IO,TLE,[]),
	report_spifcp_transformed;

    otherwise :
      EX = _,
      TO = _,
      Results = [FN - "spifcp transformation errors:" | PTE].


report_spifcp_transformed(FN, TLE, EX, IO, Results) :-

    TLE = [] |
	handle_exports,
	widgets#pretty#module(EXIO,SP),
	file#put_file(FN, SP, put, FPR),
	report_spifcp_written;

    otherwise :
      EX = _,
      IO = _,
      Results = [FN - "compound transformation errors:" | TLE].

  handle_exports(EX, IO, EXIO) :-

    EX ? export(Es), Es =\= [] :
      EX' = _,
      EXIO = [-export(Es) | IO];

    EX ? _Other,
    otherwise |
	self;

    EX = [] :
      EXIO = IO.


report_spifcp_written(FN, FPR, Results) :-

    FPR =?= true :
      Results = (FN : written);

    otherwise :
      FN = _,
      Results = FPR.


