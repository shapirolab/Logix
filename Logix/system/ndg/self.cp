/* $Header: /home/qiana/Repository/Logix/system/ndg/self.cp,v 1.1.1.1 1999/07/09 07:02:58 bill Exp $ */
-export([context/4, encode/2, file/3, characters/3, string/3, parsed/3]).
-language(compound).
-mode(trust).

procedure context(Path, String, Options, Out).

context(Path, Name, Options, Out) :-
	fcpcolon(context(Path, Name, COptions, Out), Options, COptions).

procedure context(Path, String, Options, Out).

encode(Intermediate, Options) :-
	fcpcolon(encode(Intermediate, COptions), Options, COptions).

procedure file(Path, Options, Out).

file(Path, Options, Out) :-
	fcpcolon(context(computation, Path, COptions, Out), Options, COptions).

procedure characters(Characters, Options, Out).

characters(Characters, Options, Out) :-
	fcpcolon(characters(Characters, COptions, Out), Options, COptions).

procedure string(String, Options, Out).

string(String, Options, Out) :-
	fcpcolon(string(String, COptions, Out), Options, COptions).

procedure parsed(Parsed, Options, Out).

parsed(Parsed, Options, Out) :-
	fcpcolon(parsed(Parsed, COptions, Out), Options, COptions).

fcpcolon(Call, Options, COptions) :-
	options(Options, DtDgOpt, Assembler, Iterate, Partial, Imm, OutPut, 
							CtlOpt,	SId, Residue),
	COptions =  [intermediate(Intermediate),target(dg) | Residue],
	in_file_id(Call, InId, Reply),
	out_file_id(SId, InId, Reply, OutId),
	compile(Call, InId),
	dtdg_suffixes(DtDgOpt, DtDgS, DtDgOptS),
	out_files(OutId, OutPut, DtDgS, DtDgOptS, Files),
	fcpcolon1(Intermediate, DtDgOpt, Assembler, Iterate, ms(Partial,Imm), 
	                                                CtlOpt, Files).

dtdg_suffixes(dg(IxAlg), [".dg.",IxAlg]^, ".dgopt"^).
dtdg_suffixes(dt(IxAlg), [".dt.",IxAlg]^, ".dtopt"^).


procedure options(Options, DtDgOpts, Path, Iterate, Partial, Immediate,
	                                          OutPut, CtlOpt, SId, Residue).

options(Options, DtDgOpts, Assembler, Iterate, Partial, Immediate, OutPut, 
						CtlOpt,	SId, Residue) :-

    Options = [] :
      DtDgOpts = dg(minvariability),
      Assembler = assemble,
      Iterate = first,
      CtlOpt = no,
      OutPut = {no,no,no,no,no,no,yes},
      Partial = 1,
      Immediate = 0,
      SId = [],
      Residue = [];

    Options ? dt(Opts) : DtDgOpts' = _,
      DtDgOpts = dt(IxAlg) |
	dtdg_options(dt, Opts, IxAlg),
	options;

    Options ? dg(Opts) : DtDgOpts' = _,
      DtDgOpts = dg(IxAlg) |
	dtdg_options(dg, Opts, IxAlg),
	options;
    Options ? assembler(Path) : Assembler' = _,
      Assembler = Path |
	options;

    Options ? ctl([Opt]) : Iterate' = _,
      Iterate = Opt |
	options;

    Options ? opt(Opt) : Partial' = _ |
      Partial := 100 - Opt,
	options;

    Options ? immediate(Opt) : Immediate' = _,
      Immediate = Opt | 
	options;

    Options ? output(Out) : OutPut' = _ |
	out_options(Out, OutPut),
	options;

    Options ? ctlopt : CtlOpt' = _ , CtlOpt = yes | options ;
 
    Options ? service_id(SI), SI = [S | _], string(S) : SId' = _,
      SId = SI |
	options;

    Options ? module(M) :
      OutPut = {O1,O2,O3,O4,O5,O6,yes(M)},
      OutPut' = {O1,O2,O3,O4,O5,O6,_} |
	options;

    Options ? Other,
    otherwise :
      Residue ! Other |
	options;

    Options =\= [_|_], Options =\= [] :
      Options' = [Options] |
	options.

procedure dtdg_options(DtDg, DtDgOpts, IxAlg).

dtdg_options(DtDg, DtDgOpts, IxAlg) :- 

    DtDgOpts = [],
    DtDg = dg :
      IxAlg = minvariability;

    DtDgOpts = [],
    DtDg = dt :
      IxAlg = minvariability;

    DtDgOpts ? ix(IxAlg^) : IxAlg' = _ |
	dtdg_options;

    DtDgOpts ? Other,
    otherwise |
	computation # event(unknown_dtdg_option(Other)),
	dtdg_options;

    DtDgOpts =\= [_|_], DtDgOpts =\= [] :
      DtDgOpts' = [DtDgOpts] |
	dtdg_options.


procedure out_options(OutPutOpts, OutPut).

out_options(OutPutOpts, OutPut) :-

    OutPutOpts = [] : OutPut = {no,no,no,no,no,no,no};

    OutPutOpts ? S, string(S) |
	out_option(S, yes, OutPut, OutPut'),
	out_options;

    OutPutOpts ? S(Value), string(S) |
	out_option(S, yes(Value), OutPut, OutPut'),
	out_options;

    OutPutOpts ? Other,
    otherwise |
	computation#event(unknown_output_option(Other)),
	out_options;

    OutPutOpts =\= [_|_], OutPutOpts =\= [] :
      OutPutOpts' = [OutPutOpts] |
	out_options.


out_option(dt,     Yes, {Yes,O1,O2,O3,O4,O5,O6}^, {_,O1,O2,O3,O4,O5,O6}^).
out_option(dg,     Yes, {Yes,O1,O2,O3,O4,O5,O6}^, {_,O1,O2,O3,O4,O5,O6}^).
out_option(dtopt,  Yes, {O1,Yes,O2,O3,O4,O5,O6}^, {O1,_,O2,O3,O4,O5,O6}^).
out_option(dgopt,  Yes, {O1,Yes,O2,O3,O4,O5,O6}^, {O1,_,O2,O3,O4,O5,O6}^).
out_option(ctl,    Yes, {O1,O2,Yes,O3,O4,O5,O6}^, {O1,O2,_,O3,O4,O5,O6}^).
out_option(ctlopt, Yes, {O1,O2,O3,Yes,O4,O5,O6}^, {O1,O2,O3,_,O4,O5,O6}^).
out_option(asm ,   Yes, {O1,O2,O3,O4,O5,Yes,O6}^, {O1,O2,O3,O4,O5,_,O6}^).
out_option(module, Yes, {O1,O2,O3,O4,O5,O6,Yes}^, {O1,O2,O3,O4,O5,O6,_}^).
out_option(Other, _, OutOpts, OutOpts^) :-
	otherwise |
	computation#event(unknown_output_option(Other)).


in_file_id(Call, InId, Reply) :-

    Call = encode(_(Name, _), _) :
      InId ! Name,
      Reply = true |
	computation # (self # service_id(InId'));

    Call = context(InId', Source, _, _), string(Source), list(InId') :
      InId ! Source,
      Reply = true ;

    Call = context(Path, Source, _, _),
    otherwise :
      InId ! InName |
	computation_utils # call_id_goal(Path#Source, InId', InName, Reply);

    Call =\= context(_,_,_,_) :
      InId ! '_',
      Reply = true |
	computation # (self # service_id(InId')).

out_file_id(SId, InId, Reply, OutId) :-

    Reply = true,
    SId = [],
    InId = [InName | _], string(InName) :
      OutId = InId ;

    list(SId) : InId = _, Reply = _,
      InId' = SId,
      SId' = [],
      Reply' = true |
	out_file_id;

    otherwise : InId = _, Reply = _, SId = _,
      OutId = ['_' | Id] |
	computation # service_id(Id).

compile(Call, Id) :-

    Call = context(_, _, Options, Out),
    Id ? Name |
	compile # context(Id', Name, Options, Out);

    Call = encode(Intermediate, [intermediate(Intermediate^) | _]) : Id = _;

    otherwise : Id = _ |
	compile # Call.


out_files(OutId, OutPut, DtDgS, DtDgOptS, Files) :-
    OutPut = {DtDg, DtDgOpt, Ctl, CtlOpt, Target, Asm, Module} :
      Files = {DF, DOptF, CtlF, COptF, TF, AF, MF} |
	file_output(DtDg, OutId, DtDgS, DF),
	file_output(DtDgOpt, OutId, DtDgOptS, DOptF),
	file_output(Ctl, OutId, [".ctl"], CtlF),
	file_output(CtlOpt, OutId, [".ctlopt"], COptF),
	file_output(Target, OutId, [".target"], TF),
	file_output(Asm, OutId, [".asm"], AF),
	file_output(Module, OutId, [], MF).


file_output(no, _, _, no^).
file_output(yes(Value),  _, _, output(Value)^).
file_output(yes, [Name | OutDir], Suffix, file([FN | OutDir])^) :-
	utils # append_strings([Name | Suffix], FN).


procedure fcpcolon1(Intermediate, DtDgOpt, Path, Iterate, CtlOpt, Files).

fcpcolon1(Intermediate, DtDgOpt, Assembler, Iterate, Partial, CtlOpt, Files) :-
    Intermediate = module(Name, Procedures), CtlOpt = yes,
    Files = {D,DO,C,CO,_T,A,M} |
	phase0 # preproc(Procedures, Procs),
%	screen#display(Procs,[depth(20),length(2000),type(ground)]),
	phase0 # flow_analysis(Procs, Procs1),
	phase1(Procs1, DtDgOpt, Ds, {done,DsDone}),
	phase1opt(Ds, DsOpt, {DsDone,DsOptDone}),
	phase2(DsOpt, Ctls, Iterate, Partial, {DsOptDone,CtlsDone}),
	phase2opt(Ctls, CtlsOpt, {CtlsDone,CtlsOptDone}),
%	phase3(CtlsOpt, Targets, {CtlsOptDone,TargetsDone}),
	assemble(A, M, Name, Assembler, CtlsOpt, CtlsF, Asm, AsmDone),
	dispose(D, Ds, DsDone),
	dispose(DO, DsOpt, DsOptDone),
	dispose(C, Ctls, CtlsDone),
	dispose(CO, CtlsOpt, CtlsOptDone, CtlsF),
%	dispose(T, Targets, TargetsDone),
	dispose(A, Asm, AsmDone) ;

    Intermediate = module(Name, Procedures), CtlOpt = no,
    Files = {D,DO,C,CO,_T,A,M} |
	phase0 # preproc(Procedures, Procs),
%	screen#display(Procs,[depth(20),length(2000),type(ground)]),
	phase0 # flow_analysis(Procs, Procs1),
	phase1(Procs1, DtDgOpt, Ds, {done,DsDone}),
	phase1opt(Ds, DsOpt, {DsDone,DsOptDone}),
	phase2(DsOpt, Ctls, Iterate, Partial, {DsOptDone,CtlsDone}),
%	phase2opt(Ctls, CtlsOpt, {CtlsDone,CtlsOptDone}),
%	phase3(CtlsOpt, Targets, {CtlsOptDone,TargetsDone}),
	assemble(A, M, Name, Assembler, Ctls, CtlsF, Asm, AsmDone),
	dispose(D, Ds, DsDone),
	dispose(DO, DsOpt, DsOptDone),
	dispose(C, Ctls, CtlsDone),
	dispose(CO, Ctls, CtlsOptDone, CtlsF),
%	dispose(T, Targets, TargetsDone),
	dispose(A, Asm, AsmDone).


assemble(no, no, _, _, _, _, []^, done^).
assemble(_, M, Name, Assembler, Ctls, CtlsF, Asm, Done) :-
    otherwise, known(CtlsF) |
	melted_ctls(CtlsF, Ctls, Melted),
	Assembler # ctl(Name(Melted), Module, Asm, _),
	module(M, Module),
	when(Module, done, Done).

melted_ctls(Frozen, Ctls, Melted) :-

    Frozen = [] :
      Melted = Ctls ;

    Frozen =\= [] : Ctls = _,
      melt(Frozen, Melted, _) .

module(output(Module^), Module).
module(file(OutId), Module) :-
	file # put_module(OutId, Module).
module(no, _).

when(A, B, C) :-
    known(A) :
      C = B .

procedure phase1(Procs, DtDgOpt, Dgs, SC).

phase1(Procs, DtDgOpt, Dgs, SC) :-
	DtDgOpt = dt(IxAlg), SC = {done,_SCR} |
	phase1#dts(Procs, IxAlg, Dgs, SC).
phase1(Procs, DtDgOpt, Dgs, SC) :-
	DtDgOpt = dg(IxAlg), SC = {done,_SCR} |
	phase1#dgs(Procs, IxAlg, Dgs, SC).

procedure phase1opt(Dgs, DgsOpt, SC).

% future optimizations
phase1opt(Dgs, DgsOpt, SC) :-
	SC = {L,R} : DgsOpt = Dgs, L = R.
	
procedure phase2(DgsOpt, Ctls, Iterate, Partial, SC).

phase2(DgsOpt, Ctls, Iterate, Partial, SC) :-
	SC = {done,_SCR} |
	phase2#ctls(DgsOpt, Ctls, Iterate, Partial, SC).

procedure phase2opt(Ctls, CtlsOpt, SC).

phase2opt(Ctls, CtlsOpt, SC) :-
	SC = {done,_SCR} |
	phase2#ctlopts(Ctls, CtlsOpt, SC).

procedure dispose(Destination, Code, Done).
procedure dispose(Destination, Code, Done, (String ; [])).

dispose(Dest, Code, Done) + (Frozen = _) :-

    Done = done,
    Dest = file(Id), known(Code),
    Id = [Name | _] |
        freeze(Code, Frozen, _),
	file # put_file(Id, Frozen, [string], Ok),
	check_st(Ok, Name);

    Done = done,
    Dest = output(Code^) :
      Frozen = [] .

dispose(no, _, _, []^).


procedure check_st(Ok, Name).

check_st(Ok, Name) :-

    Ok = true |
	utils#append_strings(['MESSAGE 000 - ',
					Name, ' has been written.'], Msg),
	screen#display(Msg, known(Msg));

    Ok = write_error |
	utils#append_strings(['ERROR - 999 - Error writing ', Name], Msg),
	screen#display(Msg, known(Msg)).
