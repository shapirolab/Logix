/* $Header: /home/qiana/Repository/Logix/system/ndg/assemble/offsets.cp,v 1.1.1.1 1999/07/09 07:02:59 bill Exp $ */
 -language([evaluate,compound,colon]).
-include(ctl_values).
-include(registers).
-export([dictionary/6]).
-mode(trust).

Request ::= increment(Integer) ;
	    instruction(String, Any, Integer) ;
	    declare_label(Integer(Integer), Integer) ;
	    define_label(Integer(Integer)) ;
	    label_reference(LabelRef, {FCP_ctl_LabelOffsetEscape, Integer},
			    Integer) ;
	    iterative_reference({FCP_ctl_LabelOffsetEscape, Integer},
				Address) ;
	    string_reference(String, {FCP_ctl_StringOffsetEscape, Integer}) ;
	    procedure_reference(ProcedureId,
	    			{FCP_ctl_IterativeEscape, Integer}, Address) ;
	    iterative_reference(ProcedureId,
				{FCP_ctl_ProceduralEscape, Integer}, Address) ;
	    start_procedure(ProcedureId, Integer, Tuple) ;
	    data_escape(Tuple, Tuple) ; info_escape(Tuple, Tuple) .
In ::= [Request].

Address ::= Integer.

Contents ::= [String | Procedures].
Procedures ::= [(Address * ProcedureId @ Address) | ProcedureBody].
ProcedureBody ::= Address * ProcedureId ;
		  [Address - Any | ProcedureBody].


procedure dictionary(In, [String], Contents, String, Tuple, Ngs).

dictionary(In, Strings, Contents, Name, Info, Ngs) :-

	serve_dictionary([module_name(Name, Info) | In],
			 Contents, Lookups, EndOffset, Ngs
	),
	strings(Lookups, Strings, EndOffset).

procedure serve_dictionary(In, Procedures, Lookups, Integer, Ngs).
procedure serve_dictionary(In, Procedures, Lookups, Integer, Ngs,
			   Tree, Integer, Integer, ProcedureIdentifier
).

serve_dictionary(In, Instructions, Lookups, EndOffset, Ngs) +
(	IdTree = _,
	IC = FCP_ctl_StringHdrSize,
	IterativeIC = _,
	LabelId = 0,
        ProcedureIdentifier = "?"
) :-

    In ? increment(N),
    IC' := IC + FCP_ctl_MinorItemSize*N |
	serve_dictionary;

    In ? instruction(Mnemonic, Arguments, N),
    IC' := IC + FCP_ctl_MinorItemSize*N :
      Instructions ! (IC - Mnemonic : Arguments) |
	serve_dictionary;

    In ? declare_label(label(Definition), Identifier),
    Definition = Id(_BaseOffset) :
      Id = Identifier |				% return label identifier
	serve_dictionary;

    In ? declare_label(label(Definition), Identifier),
    LabelId' := LabelId + 1 :			% initial declaration
      Definition = LabelId(_BaseOffset),	% identify label
      LabelId = Identifier |			% return label identifier
	serve_dictionary;

    In ? define_label(Definition),
    Definition = _Id(BaseOffset) :		% label already declared
      IC = BaseOffset |		% assign label offset
	serve_dictionary;

    In ? define_label(Definition),
    LabelId' := LabelId + 1 :
      Definition = LabelId(IC) |	% identify label, assign offset
	serve_dictionary;

    In ? label_reference(label(Definition), LabelReference, BaseOffset^),
    Definition = _Id(BaseOffset),		% label previously declared
    IC' := IC + FCP_ctl_BranchAddressSize :
      LabelReference = {FCP_ctl_LabelOffsetEscape, RelativeOffset} |
	serve_dictionary,
%	computation#display(term,
%			    [label_reference, base, BaseOffset, relative,
%			     RelativeOffset, ic, IC],
%			    known(RelativeOffset)),
	relative_branch(BaseOffset, IC, RelativeOffset, ProcedureIdentifier,
		Ngs, Ngs'
        );

    In ? label_reference(label(Definition), LabelReference, BaseOffset),
    IC' := IC + FCP_ctl_BranchAddressSize,
    LabelId' := LabelId + 1 :
      Definition = LabelId(BaseOffset),		% identify label
      LabelReference = {FCP_ctl_LabelOffsetEscape, RelativeOffset} |
	serve_dictionary,
%	computation#display(term,
%			    ["label ic", IC, baseoffset, BaseOffset, relative,
%			     RelativeOffset, def, Definition],
%			    known(RelativeOffset)),
	relative_branch(BaseOffset, IC, RelativeOffset, ProcedureIdentifier,
                Ngs, Ngs'
        );

    In ? string_reference(String, StringReference),
    IC' := IC + FCP_ctl_StringOffsetSize :
      StringReference = {FCP_ctl_StringOffsetEscape, RelativeOffset},
      Lookups ! lookup(String, BaseOffset) |
	serve_dictionary,
%	computation#display(term,
%			    [baseoffset, BaseOffset, relative,
%			     RelativeOffset, ic, IC],
%			    known(RelativeOffset)),
	RelativeOffset := BaseOffset - IC;
        
    In ? data_escape(Escape1, Escape2) |
	serve_dictionary,
	data_escape(Escape1, Escape2, IC, IC');

    In ? start_procedure(Id, EndProcedureOffset, Info),
    Id = {Name, Arity, I1, I2, Registers},
    BaseOffset := FCP_ctl_MajorAllignment*
		((IC + FCP_ctl_RoundUp + FCP_ctl_StringHdrSize)
			/FCP_ctl_MajorAllignment),
    IC' := BaseOffset + FCP_ctl_MajorItemSize :
      IterativeIC = _, ProcedureIdentifier = _, ProcedureIdentifier' = _,
      Info = {FCP_ctl_InfoOffsetEscape, RelativeOffset},
      Id1 = {Name, Arity, I1, I2},
      Instructions ! (BaseOffset * Id @ EndProcedureOffset) |
	check_registers(Registers, Name, Ngs, Ngs'?),
	serve_dictionary,
	lookup(Id1?(IC', IterativeIC'), IdTree, _),
	RelativeOffset := EndProcedureOffset - BaseOffset;

    In ? iterative_label :
      IC = IterativeIC |
	serve_dictionary;

    In ? procedure_reference(Id, ProcedureReference, ProcedureBaseOffset),
    IC' := IC + FCP_ctl_MajorItemSize :
      ProcedureReference = {FCP_ctl_ProceduralEscape, RelativeOffset} |
	serve_dictionary,
	lookup(Id?(ProcedureBaseOffset, _), IdTree, _),
%	computation#display(term,
%			    ['Id, base offset, IC, relative: ', Id,
%			     ProcedureBaseOffset, IC, RelativeOffset],
%			    known(RelativeOffset)),
	RelativeOffset := ProcedureBaseOffset - IC;

    In ? iterative_reference(Id, IterativeReference, IterativeBaseOffset),
    IC' := IC + FCP_ctl_MajorItemSize :
      IterativeReference = {FCP_ctl_IterativeEscape, RelativeOffset} |
	serve_dictionary,
	lookup(Id?(_, IterativeBaseOffset), IdTree, _),
%	computation#display(term,
%			    [iterative_reference, iterativebaseoffset,
%			     IterativeBaseOffset, relative, RelativeOffset],
%			    known(RelativeOffset)),
	RelativeOffset := IterativeBaseOffset - IC;

    In ? iterative_reference(IterativeReference, IterativeIC^),
    IC' := IC + FCP_ctl_MinorItemSize :
      IterativeReference = {FCP_ctl_LabelOffsetEscape, RelativeOffset} |
	serve_dictionary,
%	computation#display(term,
%			    [iterative_reference, iterativeic, IterativeIC,
%			     relative, RelativeOffset],
%			    known(RelativeOffset)),
	relative_branch(IterativeIC, IC, RelativeOffset, ProcedureIdentifier,
                        Ngs, Ngs'
        );

    In ? procedure_info(ProcedureId, ProcedureInfoOffset^, Info),
    ProcedureId = {Name, Arity, Index, _, _},
    ProcedureInfoOffset := FCP_ctl_MajorAllignment*
			((IC + FCP_ctl_RoundUp)/FCP_ctl_MajorAllignment),
    IC' := ProcedureInfoOffset + 4*FCP_ctl_MajorItemSize :
      ProcedureIdentifier = ProcedureId,
      ProcedureIdentifier' = "??",
      Info = {FCP_ctl_PrcdrInfoEscape, RelativeOffset, Arity, Index},
      Lookups ! lookup(Name, BaseOffset),
      Instructions ! (ProcedureInfoOffset * ProcedureId) |
	serve_dictionary,
%	computation#display(term,
%			    [procedure_info2, baseoffset, BaseOffset, procinfo,
%			     ProcedureInfoOffset, relative, RelativeOffset],
%			    known(RelativeOffset)),
	RelativeOffset := BaseOffset - ProcedureInfoOffset;

    In ? module_name(Name, ModuleNameReference),
    IC' := IC + FCP_ctl_MajorItemSize :
      ModuleNameReference = {FCP_ctl_ModuleNameOffsetEscape, RelativeOffset},
      Lookups ! lookup(Name, BaseOffset),
      Instructions ! Name |
	serve_dictionary,
%	computation#display(term,
%			    [module_name, base, BaseOffset, relative,
%			     RelativeOffset],
%			    known(RelativeOffset )),
	RelativeOffset := BaseOffset - IC;

    In = [] : IdTree = _, IterativeIC = _, LabelId = _,
                ProcedureIdentifier = _,
      Lookups = [],
      Instructions = [],
      IC = EndOffset,
      Ngs = [] .


check_registers(Registers, Name, Ngs1, Ngs2) :-

    Registers < MAX_arg_reg : Name = _,
      Ngs1 = Ngs2 ;

    Registers >= MAX_arg_reg :
      Ngs1 = [Name - register_maximum_exceeded(Registers) | Ngs2] .


data_escape(Escape1, Escape2, IC1, IC2) :-

    arg(1, Escape1, null) :
      Escape2 = {FCP_ctl_AnyEscape, IC1},
      IC1 = IC2 ;

    Escape1 = integer(Value),
    IC2^ := IC1 + FCP_ctl_IntegerSize :
      Escape2 = {FCP_ctl_IntegerWordValueEscape, Value};

    Escape1 = tuple(Value),
    IC2^ := IC1 + FCP_ctl_TupleAritySize :
      Escape2 = {FCP_ctl_TupleWordArityEscape, Value};

    Escape1 = real(Value),
    IC2^ := IC1 + FCP_ctl_RealSize :
      Escape2 = {FCP_ctl_RealValueEscape, Value} .


Lookups ::= [Lookup].
Lookup ::= lookup(String, Integer).

procedure strings(Lookups).

strings(Lookups, Strings, InitialStringOffset) :-

    true :
      priority([], high) |
	make_vector(256, V, T),
	serve_lookups(Lookups, Strings, InitialStringOffset, _Stop, V, T, 0).

procedure serve_lookups(Lookups, [String], Integer,
			stop, Vector, Tuple, Index
).

serve_lookups(Ls, Strings, StringOffset, Stop, V, T, N) :-

    Ls ? lookup(String, BaseOffset),
    Index := string_hash(String) + 1,
    arg(Index, T, S), unknown(S),
    N' := N + 1 :
      write_vector(Index, String(BaseOffset, Reply), V) |
	serve_stream(S, Stop, _Tree),
	serve_lookups,
	new_string(Reply, String, BaseOffset, StringOffset, StringOffset',
			Strings, Strings'
	);

    Ls ? lookup(String, BaseOffset),
    Index := string_hash(String) + 1,
    arg(Index, T, S), known(S) :
      write_vector(Index, String(BaseOffset, Reply), V) |
	serve_lookups,
	new_string(Reply, String, BaseOffset, StringOffset, StringOffset',
			Strings, Strings'
	);

    Ls = [] : StringOffset = _, V = _, T = _, N = _,
      Strings = [],
      Stop = stop ;

    N = 256 : T = _ |
	serve_lookups(Ls, Strings, StringOffset, Stop, V).

procedure serve_lookups(Lookups, [String], Integer, stop, Vector).

serve_lookups(Ls, Strings, StringOffset, Stop, V) :-

    Ls ? lookup(String, BaseOffset),
    Index := string_hash(String) + 1 :
      write_vector(Index, String(BaseOffset, Reply), V) |
	serve_lookups,
	new_string(Reply, String, BaseOffset, StringOffset, StringOffset',
			Strings, Strings'
	);

    Ls = [] : StringOffset = _, V = _,
      Strings = [],
      Stop = stop .

Tree ::= {String, Tree, Tree}.

procedure serve_stream([String(Integer, Reply)], stop, Tree).

serve_stream(Ls, Stop, Tree) :-

   Ls ? String(Value, Reply) |
	lookup(String(Value), Tree, Reply),
	serve_stream;

    Stop = stop,
    unknown(Ls) : Tree = _ .

Reply ::= new ; old.

procedure lookup(Any, Tree, Reply).

lookup(Term, Tree, Reply) :-

    Tree = {Greater, Tree', _},
    Term @< Greater |
	lookup;

    Tree = {Less, _, Tree'},
    Less @< Term |
	lookup;

    Tree = {Entry, _, _} :
      Entry = Term,
      Reply = old ;

    writable(Tree) :
	Tree = {Term, _, _},
	Reply = new.


new_string(Reply, String, Offset, StringOffset1, StringOffset2,
		Strings1, Strings2
) :-

    Reply = old : String = _, Offset = _,
      StringOffset1 = StringOffset2,
      Strings1 = Strings2 ;

    Reply = new,
    Size := FCP_ctl_MajorAllignment*
	(string_length(String)/FCP_ctl_MajorAllignment) +
		FCP_ctl_StringHdrSize + FCP_ctl_MajorAllignment :
      Offset = StringOffset1,
      Strings1 = [String | Strings2] |
	StringOffset2 := StringOffset1 + Size .

procedure relative_branch(BaseOffset, IC, RelativeOffset, ProcedureIdentifier,
                          Ngs1, Ngs2
).

relative_branch(BaseOffset, IC, RelativeOffset, ProcedureIdentifier,
                Ngs1, Ngs2
) :-

    Span := (BaseOffset - IC)/FCP_ctl_RescaleOffset,
    FCP_ctl_MinBranchAddress =< Span,
    Span =< FCP_ctl_MaxBranchAddress : ProcedureIdentifier = _,
      RelativeOffset = Span,
      Ngs1 = Ngs2 ;

    otherwise,
    RelativeOffset^ := BaseOffset - IC : BaseOffset = _, IC = _, Ngs2 = _,
      Ngs1 = [ProcedureIdentifier@BaseOffset - maximum_span_exceeded(IC) | Ngs2] .
