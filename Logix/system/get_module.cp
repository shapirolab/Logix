/*

Module server
William Silverman - 3/89

Last update by		$Author: bill $
			$Date: 1999/07/09 07:02:53 $
Currently locked by     $Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/system/get_module.cp,v $

Copyright (C) 1989, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([binary/3, cdg/4, ndg/4, ccdg/4, nccdg/4, compile/4, 
		compile_module/4, update/5]).
-mode(interrupt).
-language(compound).

/*
** binary/3 - Find latest binary version of module.
**
*/

procedure binary(Context, Name, ModuleResult).

binary(Context, Name, ModuleResult) :-
	update(Context, Name, binary, _, ModuleResult).

/*
** update/5 - Find latest version of module.
**
**	    Optionally compile if source most recent.
**
*/

Default ::= query ; auto ; binary.

procedure update(Context, Name, Options, Output, ModuleResult).

update(Context, Name, Options, Output, ModuleResult) :-
    string_to_dlist(Name, ListBin, Bin),
    string_to_dlist(".bin", Bin, []),
    string_to_dlist(Name, ListDot_o, Dot_o),
    string_to_dlist(".o", Dot_o, []),
    string_to_dlist("Bin/", BinListBin, ListBin),
    string_to_dlist("Bin/", BinListDot_o, ListDot_o) |
	list_to_string(ListBin, BinName),
	list_to_string(BinListBin, BinBinName),
	list_to_string(ListDot_o, Dot_oName),
	list_to_string(BinListDot_o, BinDot_oName),
	options(Options, Name, Name', Default, Encoder, Others),
	cp_info(Default, Context, Name, DateCp, CpInfo),
	file # [execute_in_context(Context, fileinfo(BinName, BinDate)),
		execute_in_context(Context, fileinfo(BinBinName, BinBinDate)),
		execute_in_context(Context, fileinfo(Dot_oName, Dot_oDate)),
		execute_in_context(Context,fileinfo(BinDot_oName, BinDot_oDate))
	       | CpInfo],
	compare_dates([BinBinDate, BinDate, BinDot_oDate, Dot_oDate],
			[BinBinName, BinName, BinDot_oName, Dot_oName],
			DateBin, NameBin
	),
	update_source(DateCp, DateBin, Context, NameBin, Update),
	update_compile([Name | Context], Name'(Encoder, Others),
			Output, Result, Default, DateBin, Update
	),
	module_result(Result, ModuleResult).


cp_info(Default, Scope, Name, DateCp, CpInfo) :-

    Default = binary : Scope = _, Name = _,
      DateCp = 0,
      CpInfo = [] ;

    otherwise,
    string_to_dlist(Name, ListCp, Cp),
    string_to_dlist(".cp", Cp, []) : Default = _,
      CpInfo = [execute_in_context(Scope, fileinfo(NameCp, DateCp))] |
	list_to_string(ListCp, NameCp).


compare_dates(Dates, Names, DateBin, NameBin)
 + (Newest_Date = -1, Newest_Name = _) :-

    Newest_Date = -1, Dates ? Date, Names ? Name :
      Newest_Name = _,
      Newest_Date' = Date, Newest_Name' = Name |
	self;

    Dates = [], Names = [],
    "930530090000" @< Newest_Date :
      DateBin = Newest_Date,
      NameBin = Newest_Name;

    Dates = [], Names = [], otherwise : Newest_Date = _,
      DateBin = 0,
      NameBin = Newest_Name ;

    Newest_Date =\= -1, Dates ? Date, Names ? Name,
    Newest_Date @< Date :
      Newest_Name = _,
      Newest_Date' = Date,
      Newest_Name' = Name |
	self;

    Newest_Date =\= -1, Dates ? Date, Names ? Name,
    otherwise: Date = _, Name = _ |
	self.


update_source(DateCp, DateBin, Context, NameBin, Update) :-

    DateBin @< DateCp :
      Update = update(DateCp, Context, NameBin) ;

    DateCp = 0, DateBin = 0 : Context = _, NameBin = _,
      Update = false(no_module) ;

    otherwise,			% Second time around ?
    NameBin = '.' : DateCp = _, DateBin = _, Context = _, NameBin = _,
      Update = false(no_module) ;

    otherwise,
    NameBin =\= '.' |
	name_to_base_and_suffix(NameBin, BaseName, NameSuffix),
	load_file(DateCp, DateBin, Context, NameBin, Update,
						BaseName, NameSuffix).

name_to_base_and_suffix(FullName, BaseName, NameSuffix) :-

  string_to_dlist(FullName, L, []) |
	get_suffix_index(L, Index),
	get_name_pieces(L, Index, BaseName, NameSuffix).

get_suffix_index(L, Index)
  + (N = 0, BeforeLastDot = 0) :-

  L ? Character, Character =:= ascii("."),
  N++ :
    BeforeLastDot = _,
    BeforeLastDot' = N |
	get_suffix_index;

  L ? _Character,
  otherwise,
  N++ |
	get_suffix_index;

  L = [] : N = _,
    Index = BeforeLastDot.

get_name_pieces(L, Index, BaseName, NameSuffix)
  + (N = 0, B = BaseList, BaseList) :-

  L ? Character, N < Index, N' := N + 1 :
    BaseList ! Character |
	get_name_pieces;

  L ? _, N = Index, N' := N + 1 :
    BaseList = [] |
	get_name_pieces;

  N > Index : BaseList = _ |
	list_to_string(L, NameSuffix),
	list_to_string(B, BaseName).


load_file(DateCp, DateBin, Context, NameBin, Update, BaseName, NameSuffix) :-

    NameSuffix = "bin" : BaseName = _ |
	file # execute_in_context(Context,
				  get_file(NameBin, Module, [string], Reply)
		),
	update_module(DateCp, Module, NameBin, DateBin, bin, Reply, Update);

    NameSuffix = "o",
    string_to_dlist(BaseName, BNL, []) : NameBin = _ |
	Context#"_unique_id"(Path),
	string_to_dlist(Path, PL, BNL),
	list_to_string(PL, UNIXpath),
	processor # link(get(UNIXpath, Module), Reply),
	update_module(DateCp, Module, NameBin, DateBin, native,
			Reply, Update
	).

update_module(DateCp, Module, NameBin, DateBin, Kind, Reply, Update) :-

    Reply = true, Kind = bin,
    module(Module) : DateCp = _,
      Update = module(Module, NameBin, DateBin) ;

    Reply = true, Kind = native,
    integer(Module) : DateCp = _,	% native coded
      Update = module(Module, NameBin, DateBin) ;

    Reply =\= true, Kind = native,
    Module = unbound : Module = _, NameBin = _, DateBin = _, DateCp = _ |
					% native coded
      Update = false('error in dynamic linking');

    otherwise : Module = _, NameBin = _, DateBin = _, Kind = _, Reply = _ |
	update_source(DateCp, 0, [], '.', Update).


update_compile(_, _, []^, ModuleResult^, binary, _, ModuleResult) :-
    arg(1, ModuleResult, module) |
	true.
update_compile(_, _, []^, False^, _, _, False) :-
    arg(1, False, false) |
	true.
update_compile(ID, NCO, Output, Result, Default, DateBin, Update) :-

    Update = module(Module, _, _),
    Default =\= binary :
      activate(Module, Arg1, Arg2),
      deschedule |
	module_attributes(Arg1, Arg2, Depends),
	depends(ID, NCO, Output, Result, Default, DateBin, Update, Depends);

    arg(1, Update, update),
    DateBin =\= 0,
    Default = query |
	module_path(ID, PathName),
	ask(done, PathName-binary(DateBin)-'compile newer source?',
		Default, load, Do
	),
	update_compile1(ID, NCO, Output, Result, Do, DateBin, Update);

    otherwise |
% No need to ask - (DateBin = 0 or Default = auto) and Update = update(...).
	update_compile1(ID, NCO, Output, Result, Default, DateBin, Update).


depends(ID, NCO, Output, Result, Default, DateBin, Module, Depends) :-

    Depends = [] : ID = _, NCO = _, Default = _, DateBin = _,
      Output = [],
      Result = Module ;

    Depends =\= [] |
	depends_warnings(Depends, ID, DateBin, Default, Warnings),
	warnings(ID, NCO, Output, Result, Default, DateBin, Module,
			Warnings
	).


depends_warnings(Depends, ID, DateBin, Default, Warnings) :-

    Depends ? Depend |
	depend_warning(Depend, ID, DateBin, Default, Warnings, Warnings'),
	depends_warnings;

    Depends =\= [_|_]: ID = _, DateBin = _, Default = _,
      Warnings = [] .

depend_warning(Depend, ID, DateBin, Default, Warnings1, Warnings2)
		+ (DateCp = 0) :-

    DateCp =\= 0,
    DateBin @< DateCp : ID = _, Default = _, Depend = _,
      Warnings1 = [Depend(DateCp) | Warnings2];

    DateCp = 0,
    ID ? _ |
	file # [isdirectory([Depend | ID'], IsDirectory) | CpInfo],
	depend_director(IsDirectory, Depend, ID', Scope, Name),
	cp_info(Default, Scope, Name, DateCp', CpInfo),
	depend_warning;

    DateCp = 0,
    ID =\= [_|_], ID =\= [] |	% hierarchy_server calls with
				% [Name | context(SuperServer)]
	ID # service_id(ID'),
	depend_warning;

    otherwise : ID = _, Depend = _, DateBin = _, Default = _, DateCp = _,
      Warnings2 = Warnings1 .

depend_director(IsDirectory, Depend, ID, Scope, Name) :-

    IsDirectory = true :
      Scope = [Depend | ID],
      Name = self ;

    IsDirectory =\= true :
      Scope = ID,
      Name = Depend .
	
module_attributes(Arg1, Arg2, Depends) :-

    true :
      Arg1 = export(attributes(Attributes)) |
	activate_controls(Arg2),
	module_depends(Attributes, Depends);

    otherwise : Arg1 = _, Arg2 = _,
      Depends = [] .

activate_controls(Controls) :-

    Controls = Monitor(Input, _L, _R, _Channel),
    Monitor =\= procedures :
      Input = [] ;

    otherwise : Controls = _ .

module_depends(Attributes, Depends) :-

    Attributes ? depends(Depends^) : Attributes' = _ ;

    Attributes ? Other, Other =\= depends(_) |
	module_depends;

    Attributes =\= [_|_] :
      Depends = [] .


warnings(ID, NCO, Output, Result, Default, DateBin, Module, Warnings) :-

    Warnings = [] : ID = _, NCO = _, Default = _, DateBin = _,
      Output = [],
      Result = Module ;

    Warnings =\= [],
    Default = query |
	module_path(ID, PathName),
	update_warning(PathName, DateBin, Warnings, NameDate, Warning),
	ask(done, NameDate-'depends on newer'-Warning-recompile, 
		Default, load, Do),
	update_compile1(ID, NCO, Output, Result, Do, DateBin, Module);
    
    Warnings =\= [],
    Default =\= query |
	update_compile1(ID, NCO, Output, Result, Default, DateBin, Module).


update_warning(PathName, DateBin, Warnings, NameDate, Warning) :-

    DateBin = 0,
    Warnings = [Warning^] :
      NameDate = PathName ;

    DateBin =\= 0,
    Warnings = [Warning^] :
      NameDate = PathName(DateBin);

    DateBin = 0,
    Warnings =\= [_] :
      NameDate = PathName,
      Warning = Warnings ;

    DateBin =\= 0,
    Warnings =\= [_] :
      NameDate = PathName(DateBin),
      Warning = Warnings .


update_compile1(ID, NCO, Output, Result, Do, DateBin, Update) :-

    Do =\= load,
    NCO = Name(Encoder, Others) |
	get_source(Do, ID, Name, Encoder, Others, Controls, Found),
	compile_source(Found, Controls, Output, _, ModuleResult, Encoder),
	update_compile2(ModuleResult, Update, Result, Do, DateBin);

    Do = load,
    Update = update(0, _, NameBin),
    ID ? _  : NCO = _,
      Output = [] |
	update_source(0, DateBin, ID', NameBin, Result);

    Do = load,
    Update = module(_Module, _NameBin, _DateBin) :
	ID = _, NCO = _, DateBin = _,
      Output = [] |
	Result = Update ;

    otherwise,			% Do = load, DateCp =\= 0
    ID ? _,
    Update = update(_DateCp, _, NameBin) : Do = _ |
	update_source(DateBin, DateBin, ID', NameBin, Update'),
	update_compile(ID, NCO, Output, Result, query, 0, Update').


update_compile2(ModuleResult, Update, Result, Default, DateBin) :-

    ModuleResult = import, DateBin =\= 0,
    Update = update(_, Context, NameBin),
    NameBin =\= '.' |
	update_compile3(Default, Context, NameBin, DateBin, DateBin'),
	update_source(0, DateBin', Context, NameBin, Result);

    otherwise : Update = _, Default = _, DateBin = _,
      ModuleResult = Result .


update_compile3(Default, Context, NameBin, DateBin1, DateBin2) :-

    Default = query |
	module_path(Context, NameBin, BinPath),
	ask(done, use(BinPath), DateBin1, 0, DateBin2);

    otherwise : Default = _, Context = _, NameBin = _,
      DateBin1 = DateBin2 .


module_path(ID, PathName) :-
    ID ? Name |
	module_path(ID', Name, PathName).

module_path(Context, NameBin, NamedBin) + (CharList = []) :-

    Context ? NameBin',
    string_to_dlist(NameBin, CharList', CharList),
    string_to_dlist(' # ', CharList'', CharList') |
	module_path;

    Context =\= [_|_], Context =\= [] |	% hierarchy_server calls with
					% [Name | context(SuperServer)]
	Context # service_id(Context'),
	module_path;

    Context = [],
    string_to_dlist(NameBin, CL, CharList) |
	list_to_string(CL, NamedBin).


ask(Done, Query, Yes, No, Which) :-
    Done = done |
	computation # display(ask, - 'y/n', Answer, [prefix(Query),
						     type(ground), read(char)
						    ]
			),
	select_answer(Answer, Yes, No, Which).


select_answer(y, Yes, _, Yes^).
select_answer(_, _, No, No^) :-
    otherwise |
	true.


module_result(Result, ModuleResult) :-

    Result = {_,_,_,_} :
      Result = ModuleResult ;

    otherwise : Result = _,
      ModuleResult = import.

/*
** compile/4 - Compile module or sub-hierarchy.
**
*/

procedure compile(ID, Options, Output, Reply).

compile(ID, Options, Output, Reply) :-
	file # isdirectory(ID, IsDirectory),
	compile1(ID, Options, Output, Reply, IsDirectory).


/*
** compile_module/4 - Compile module only
**
*/

procedure compile_module(ID, Options, Output, Reply).

compile_module(ID, Options, Output, Reply) :-
	compile1(ID, Options, Output, Reply, false).

/*
** cdg/4 - Remove soon
**
*/

procedure cdg(ID, Options, Output, Reply).

cdg(ID, Options, Output, Reply) :-
	file # isdirectory(ID, IsDirectory),
	compile1(ID, [compiler(cdg) | Options], Output, Reply, IsDirectory).


ccdg(ID, Options, Output, Reply) :-
	file # isdirectory(ID, IsDirectory),
	compile1(ID, [compiler(ccdg) | Options], Output, Reply, IsDirectory).

ndg(ID, Options, Output, Reply) :-
	file # isdirectory(ID, IsDirectory),
	compile1(ID, [compiler(ndg) | Options], Output, Reply, IsDirectory).

nccdg(ID, Options, Output, Reply) :-
	file # isdirectory(ID, IsDirectory),
	compile1(ID, [compiler(nccdg) | Options], Output, Reply, IsDirectory).

compile1(ID, Options, Output, Reply, IsDirectory) :-

    IsDirectory = true,
    ID = [DirName | _] :
      COptions = [wait(_Action, _Module), name(Name), service_id(SelfID),
		  target(Encoder), blocked(UID), date(DateTime)
		 | Others],
      SelfID = [self | ID] |
	ID # '_unique_id'(UID),
	processor # interface(date_time(DateTime, _, _)),
	options(Options, DirName, Name, Default, Encoder, Others),
	block # compose(ID, [source(Terms), report(Report)]),
	block_typed(Terms, Language),
	compile # parsed([-language(Language) | Terms], COptions, Diagnostics),
	computation # Comments?,
	compilation_output(Report, Diagnostics, Output, Comments, Continue),
	compilation_done(Continue, SelfID, Default, COptions, Result),
	compiled(SelfID, Result, Reply, _);

    IsDirectory =\= true,
    ID ? Name : ID' = _ |
	options(Options, Name, Name', Default, Encoder, Others),
	get_source(Default, ID, Name', Encoder, Others, Controls, Found),
	compile_source(Found, Controls, Output, Reply, _, Encoder).

/********************** block_compile sub-hierarchy **************************/

block_typed(Terms, Language) :-

    Terms ? procedure(_) : Terms' = _,
      Language = typed ;

    Terms ? (_ ::= _) : Terms' = _,
      Language = typed ;

    Terms ? _,
    otherwise |
	block_typed;

    Terms = [] :
      Language = fcp .

/***************************** compile module ********************************/


options(Options, Name1, Name2, Default, Encoder, Others) :-

    Options ? auto :
      Default = auto, Default' = _ |
	options;

    Options ? query :
      Default = query, Default' = _ |
	options;

    Options ? binary :
      Default = binary, Default' = _ |
	options;

    Options ? name(Name) :
      Name2 = Name, Name2' = _ |
	options;

    Options ? compiler(compile) :
      Encoder = fcp, Encoder' = _ |		% Default
	options;

    Options ? compiler(wam) :
      Encoder = wam, Encoder' = _ |
	options;

    Options ? compiler(OtherEncoder),		% cdg, ccdg, ndg, nccdg
    otherwise :
      Encoder = OtherEncoder, Encoder' = _ |
	options;

    Options ? Other,
    otherwise :
      Others ! Other |
	options;

    Options = [] :
      Name2 = Name1,
      Default = query,
      Encoder = fcp,
      Others = [] ;

    Options =\= [], Options =\= [_|_] :
      Options' = [Options] |
	options.

get_source(Default, ID, Name, Encoder, Others, Controls, Found) :-
    true :
      Controls = Default(ID, (source : FileName - Date), Characters, COptions),
      COptions = [wait(_Action, _Module), name(Name), target(Encoder),
		  service_id(ID), source(FileName), date(Date)
		 | Others] |
	file # get_source(ID, Characters, Found, FileName, Date).


compile_source(Status, Controls, Output, Reply, ModuleResult, Encoder):-

    Status = found,
    Controls = Default(SId, Mss, Characters, Options),
    Characters =\= [] |
	computation # [comment(Mss) | Comments?],
	compile # characters(Characters, Options, Results),
	compilation_output(Results, [], Output, Comments, Continue),
	compilation_done(Continue, SId, Default, Options, Result),
	compiled(Encoder, SId, Result, Reply, ModuleResult);

    Status = found,
    otherwise : Controls = _, Encoder = _,		% Characters = []
      Output = [],
      Reply = false(empty),
      ModuleResult = import ;

    Status =\= found : Controls = _, Encoder = _,
      Output = [],
      Reply = false(Status),
      ModuleResult = import .

/*************************** compilation control *****************************/

compilation_done(Continue, SId, Default, Options, Result) :-

    Continue = continue,
    Options ? wait(save^, Module) :
      Options'' = [module(Module) | Options'] |
	compilation_done;

    Continue = continue,
    Options ? module(Module),
    module(Module) : SId = _, Default = _, Options' = _,
      Result = Module ;

    Continue = query,
    Default = query,
    Options ? wait(Action, Module) :
      Continue' = continue,
      Options'' = [module(Module') | Options'] |
	module_path(SId, PathName),
	processor # machine(idle_wait(Done)),
	ask(Done, compile(errors)-save(PathName),
	    save(Module), none([]), Action(Module')
	),
	compilation_done;

% Continue = continue and Module = [] 
% or Continue =\= query or Default =\= query.
    otherwise,
    Options ? First : Continue = _, SId = _, Default = _, Options' = _,
      Result = false(errors) |
	unify_without_failure(First, wait(none, _)).


compilation_output(Results, More, Output, Comments, Continue)
		+ (State = continue) :-

    Results ? Comment, Comment = (_ : _) :
      Comments ! comment(Comment) |
	self;

    Results ? Comment, Comment = comment(_) :
      Comments ! Comment |
	self;

    Results ? Diagnostic, Diagnostic = diagnostic(_) : State = _,
      Output ! Diagnostic,
      State' = query |
	self;

    Results ? Diagnostic,
    otherwise : State = _,
      Output ! diagnostic(Diagnostic),
      State' = query |
	self;

    Results =\= [], Results =\= [_|_] :
      Results' = [Results] |
	self;

    Results = [], More =\= [] :
      Results' = More,
      More' = [] |
	self;

    Results = [], More = [] :
      Output = [],
      Comments = [],
      Continue = State .


compiled(Encoder, SId, Result, Reply, ModuleResult) :-

    Encoder = ccdg :
      SId = _, Result = _, Reply = true, ModuleResult = true;

    Encoder = nccdg :
      SId = _, Result = _, Reply = true, ModuleResult = true;

    otherwise : Encoder = _ |
	compiled(SId, Result, Reply, ModuleResult).

compiled(SId, Result, Reply, ModuleResult) :-

    module(Result) |
	file # put_module(SId, Result, ModuleResult),
	when_module_result(ModuleResult, Reply);

    otherwise : SId = _,
      Result = Reply,
      ModuleResult = import .


when_module_result(module(_, _, _), true^).
when_module_result(Result, Result^) :-
    otherwise |
	true.

