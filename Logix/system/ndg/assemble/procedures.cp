/* $Header: /home/qiana/Repository/Logix/system/ndg/assemble/procedures.cp,v 1.1 1999/07/09 07:02:59 bill Exp $ */
-export([assemble/4]).
-mode(trust).
-include(dg_opcodes).
-include(registers).
-language([evaluate,compound,colon]).

IfIntegerBaseOp => FCP_dg_if_int_lt.
IfRealBaseOp => FCP_dg_if_real_lt.
IfStringBaseOp => FCP_dg_if_str_lt.
IfTupleBaseOp => FCP_dg_if_tuple_lt.
CompareIntegerBaseOp => FCP_dg_cmp_int_lt.
CmtPlus_BaseOp => FCP_dg_cmt_plus_reg_reg_reg.
Plus_BaseOp => FCP_dg_plus_reg_reg_reg.
Plus_number_BaseOp => FCP_dg_plusnum_reg_reg.
CmtPlus_number_BaseOp => FCP_dg_cmt_plusnum_reg_reg.
Diff_BaseOp => FCP_dg_diff_reg_reg_reg.
CmtDiff_BaseOp => FCP_dg_cmt_diff_reg_reg_reg.
Diff_number_BaseOp => FCP_dg_diffnum_reg_reg.
CmtDiff_number_BaseOp => FCP_dg_cmt_diffnum_reg_reg.

Requests ::= offsets # In .

procedure assemble(CTLs, Contents, [Strings], Requests).

assemble(CTLs, Contents, Strings, Requests) :-
	guardtable # dictionary(Guards),
	assemble(CTLs, Contents, Strings, Requests, Guards).

Guards ::= [guard_index(String/Integer, Integer)].

procedure assemble(CTLs, Contents, [Strings], Requests, Guards).

%assemble no longer calls prepass#instructions since it was moved to ctlopt in phase2
assemble(CTLs, Contents, Strings, Requests, Guards) :-

    CTLs ? {ProcId, Instructions} :
      Requests ! start_procedure(ProcId, InfoOffset, InfoReference),
      Contents ! InfoReference |
	assemble,
	first_instruction(Instructions, Instructions'),
	instructions(Instructions', Requests',
		     [procedure_info(ProcId, InfoOffset, Info) | Requests''],
		     Contents', [Info | Contents''],
		     Guards, Guards'
	);

    CTLs = [] :
      Contents = Strings,
      Requests = [],
      Guards = [] .

procedure first_instruction(Instructions, Instructions).

first_instruction(Instructions1, Instructions2) :-

    Instructions1 ? multiple_copy(_, Dest) |
      do_first_instruction(Dest,Instructions1, Instructions1', Instructions2);

    otherwise :
      Instructions1 = Instructions2 .

procedure do_first_instruction(Dest,Instructions, Instructions, Instructions).

do_first_instruction(Dest,Instructions1, Instructions1Prime, Instructions2) :-
    Dest ? a(_) |
	do_first_instruction;

    Dest = [] :
	Instructions1 = _,
	Instructions2 = Instructions1Prime;	% remove first multiple_copy ins
						% if the Dest field had only 
						% plain regs (w/o indexing etc).

    otherwise :
	Dest = _,
	Instructions1Prime = _,
	Instructions2 = Instructions1 .	% keep the entire instruction if
					% part of the source is not a register.

procedure instructions(Instructions, Requests, Requests, Items, Items,
			Guards, Guards
).

instructions(Instructions, Requests, EndRequests, Items, EndItems,
		Guards, EndGuards
) :-

    Instructions ? label(Definition) :
      Requests ! define_label(Definition) |
	instructions;

    Instructions ? iterative_label :
      Requests ! iterative_label |
	instructions;

    Instructions ? body_label :		% needed for code generation
      Requests ! iterative_label |
	instructions;

    Instructions ? call("=:=", Args, Labels) :	% Transitional kluge
      Instructions'' = [call("=?=", Args, Labels) | Instructions'] |
	instructions;

    Instructions ? call("=", Args, Labels) :
      Instructions'' = [call("=?=", Args, Labels) | Instructions'] |
	instructions;

    Instructions ? call(Name, Args, Label),
    Label =\= [_,_], Label =\= [_],

    Name =\= "=:=", Name =\= '=' :	% Transitional kluge
      Guards ! guard_index(Name/N, OpCode) |
	aux#call_args(Name, OpCode, Args, else(Address), N,
		  Requests, [label_reference(Label, Reference1, Address),
			     label_reference(Label, Reference2, Address)
			    | Requests'],
		  Items, [Reference1, Reference2 | Items']
	),
	instructions;

    Instructions ? call(Name, Args, [Label]),

    Name =\= "=:=" , Name =\= "=" :	% Transitional kluge
      Guards ! guard_index(Name/N, OpCode) |
	aux#call_args(Name, OpCode, Args, else(Address), N,
		  Requests, [label_reference(Label, Reference1, Address),
			     label_reference(Label, Reference2, Address)
			    | Requests'],
		  Items, [Reference1, Reference2 | Items']
	),
	instructions;

    Instructions ? call(Name, Args, [Label1, Label2]),

    Name =\= "=:=" , Name =\= "=" :	% Transitional kluge
      Guards ! guard_index(Name/N, OpCode) |
	aux#call_args(Name,OpCode, Args, (fail(Address1), suspend(Address2)), N,
		  Requests, [label_reference(Label1, Reference1, Address1),
			     label_reference(Label2, Reference2, Address2)
			    | Requests'],
		  Items, [Reference1, Reference2 | Items']
	),
	instructions;

    Instructions ? unify(Asgns, Args, Label) :
      Requests = [instruction(unify, (N, else(Address)), 2),
		label_reference(Label, Reference, Address)
		| Requests'],
      Items = [FCP_dg_unify_args, N, Reference | Items'] |
	aux # unify_args(Asgns,Args,Requests', Requests'',
			Items', Items'', N, 0),
	instructions;

    Instructions ? deref(Arg1, Arg2, Arg3) |
	instructions,
	aux # deref(Arg1, Arg2, Arg3, Requests, Requests', Items, Items',
		Instructions', Instructions'');

    Instructions ? deref_car(Arg1, Arg2, Arg3) |
	instructions,
	aux # registers([Arg1, Arg2, Arg3], 1, Requests, Requests',
			Items, [FCP_dg_deref_car_3 | Items'], deref_car, '');

    Instructions ? deref_value(a(I), a(K)) |
	instructions,
	aux # deref_value(I, K, Requests, Requests', Items, Items',
			  Instructions', Instructions'');

    Instructions ? deref({a(I), IX}, a(J)) :
      Requests ! instruction(deref, ((I, IX), J), 4),
      Items = [FCP_dg_deref_subarg_2, RegE(I), IndexE(IX), RegE(J) | Items']  |
	instructions;

    Instructions ? deref(a(I), a(J)) :
      Requests ! instruction(deref, (I, J), 3),
      Items = [FCP_dg_deref_2_addr, RegE(I), RegE(J) | Items']  |
	instructions;

    Instructions ? deref_integer(a(I), [Label1, Label2]) :
      Requests =[instruction(deref_integer, (I, else(Address1),
				variable(Address2)), 2),
		label_reference(Label1, Reference1, Address1),
		label_reference(Label2, Reference2, Address2)
		| Requests'],
      Items = [FCP_dg_deref_integer1,RegE(I),Reference1, Reference2
	      | Items']  |
	instructions;

    Instructions ? load_car(a(I), a(J)) :
      Requests ! instruction(load_car, (I, J), 3),
      Items = [FCP_dg_load_car, RegE(I), RegE(J) | Items']  |
	instructions;

    Instructions ? deref_car(a(I), a(J)) :
      Requests ! instruction(deref_car, (I, J), 3),
      Items = [FCP_dg_deref_car_2, RegE(I), RegE(J) | Items']  |
	instructions;

      Instructions ? deref_vars([a(I)], Label) :	% optimization
       Requests = [instruction(deref_var1, (I, else(Address)), 2),
		label_reference(Label, Reference, Address)
		| Requests'],
      Items = [FCP_dg_deref_var1, RegE(I), Reference | Items'] |
	instructions;

     Instructions ? deref_vars(Regs, Label), Regs =\= [a(_)] :
      Requests = [instruction(deref_vars, (K, else(Address), Arguments), Count),
		label_reference(Label, Reference, Address)
		| Requests'],
      Items = [FCP_dg_deref_vars, K | OtherItems] |
       instructions,
       aux#do_vars(Regs, Arguments, K, 0, [Reference | Items'], 
			OtherItems, Count, 2);

		/** Internally generated **/
    Instructions ? deref_integer(a(I), a(K), Integer, Label) :
      Data = integer(Integer),
      Requests = [instruction(deref_integer, (I, K =\= Data, then(Address)), 3
		  ),
                  data_escape(Data, IntegerWord),		  
		  label_reference(Label, Reference, Address)
		 | Requests'],
      Items = [FCP_dg_drf2_if_int_neq, RegE(I), RegE(K), IntegerWord, Reference
	      | Items']  |
	instructions;

		/** Internally generated **/
    Instructions ? deref_real(a(I), a(K), Real, Label) :
      Data = real(Real),
      Requests = [instruction(deref_real, (I, K =\= Data, then(Address)), 3),
                  data_escape(Data, RealWords),		  
		  label_reference(Label, Reference, Address)
		 | Requests'],
      Items = [FCP_dg_drf2_if_real_neq, RegE(I), RegE(K), RealWords, Reference
	      | Items']  |
	instructions;

		/** Internally generated **/
    Instructions ? deref_string(a(I), a(K), String, Label) :
      Data = string_reference(String),
      Requests = [instruction(deref_string, (I, K =\= Data, then(Address)), 3),
                  data_escape(Data, StringRef),		  
		  label_reference(Label, Reference, Address)
		 | Requests'],
      Items = [FCP_dg_drf2_if_str_neq, RegE(I), RegE(K), StringRef, Reference
	      | Items']  |
	instructions;

		/** Internally generated **/
    Instructions ? deref_nil(a(I), a(K), Label) :
      Requests = [instruction(deref_nil, (I, K, else(Address)), 3),
		  label_reference(Label, Reference, Address)
		 | Requests'],
      Items = [FCP_dg_drf2_if_not_nil, RegE(I), RegE(K), Reference | Items']  |
	instructions;

		/** Internally generated **/
    Instructions ? old_deref_list(a(I), a(K), Label) :
      Requests = [instruction(old_deref_list, (I, K, else(Address)), 3),
		  label_reference(Label, Reference, Address)
		 | Requests'],
      Items = [FCP_dg_drf2_if_not_list, RegE(I), RegE(K), Reference
	      | Items']  |
	instructions;

    Instructions ? deref_list(a(I), a(K), Label) :
      Requests = [instruction(deref_list, (I, K, else(Address)), 3),
		  label_reference(Label, Reference, Address)
		 | Requests'],
      Items = [FCP_dg_deref_list, RegE(I), RegE(K), Reference | Items']  |
	instructions;

    Instructions ? deref_list(a(I), a(J), a(K), Label) :
      Requests = [instruction(deref_list, (I, J, K, else(Address)), 4),
		  label_reference(Label, Reference, Address)
		 | Requests'],
      Items = [FCP_dg_deref_list3, RegE(I),RegE(J), RegE(K), Reference
	      | Items']  |
	instructions;

    Instructions ? deref_car_list(a(I), a(J), a(K), Label) :
      Requests = [instruction(deref_car_list, (I, J, K, else(Address)), 4),
		  label_reference(Label, Reference, Address)
		 | Requests'],
      Items = [FCP_dg_deref_car_list, RegE(I), RegE(J), RegE(K), Reference
	      | Items']  |
	instructions;

    Instructions ? deref_list({a(I), IX}, a(J), a(K), Label) :
      Requests = [instruction(deref_list, ((I, IX), J, K, else(Address)),5),
		  label_reference(Label, Reference, Address)
		 | Requests'],
      Items = [FCP_dg_deref_sub_list, RegE(I), IndexE(IX), RegE(J), RegE(K),
	       Reference | Items']  |
	instructions;

		/** Internally generated **/
    Instructions ? deref_tuple(a(I), a(K), Arity, Label) :
      Data = tuple(Arity),
      Requests = [instruction(deref_tuple, (I, K =\= Data, then(Address)), 3),
                  data_escape(Data, TupleWord),		  
		  label_reference(Label, Reference, Address)
		 | Requests'],
      Items = [FCP_dg_drf2_if_tuple_neq, RegE(I), RegE(K), TupleWord,
	       Reference | Items']  |
	instructions;

		/** Internally generated **/
    Instructions ? deref_on_tag(a(I), a(K), TagLabels),
    integer(I), integer(K) :
      Requests = [instruction(deref_on_tag,
			      (I, K, N, mask(TagBits), Addresses),
			      4
		  ),
                  data_escape(integer(TagBits), TagBitsWord)
		 | Requests'],
      Items = [FCP_dg_switch_on_tag, RegE(I), RegE(K), N, TagBitsWord
	      | Items'] |
	aux#tag_labels(TagLabels, N, TagBits, Requests', Requests'',
			Items', Items'', Addresses
	),
	instructions;

		/** Internally generated **/
    Instructions ? deref_branch_integer(a(I), a(K), Low, High, Labels),
    N := High - Low :
      Requests = [instruction(deref_branch_integer,
			      (I, K, min(Low), max(High), Addresses),
			      3
		  ),
		  data_escape(integer(Low), LowI),
		  data_escape(integer(High), HighI)
		 | Requests'],
      Items = [FCP_dg_drf2_branch_integer, RegE(I), RegE(K), LowI, HighI
	      | Items'] |
	aux#branch_labels(Labels, Requests', Requests'', Items', Items'',
			Addresses, -2, N	% Consistency check
	),
	instructions;

		/** Internally generated **/
    Instructions ? deref_branch_tuple(a(I), a(K), Low, High, Labels),
    N := High - Low :
      Requests = [instruction(deref_branch_tuple,
			      (I, K, min(Low), max(High), Addresses),
			      3
		  ),
		  data_escape(tuple(Low), LowI),
		  data_escape(tuple(High), HighI)
		 | Requests'],
      Items = [FCP_dg_drf2_branch_tuple, RegE(I), RegE(K), LowI, HighI
	      | Items'] |
	aux#branch_labels(Labels, Requests', Requests'', Items', Items'',
			Addresses, -2, N	% Consistency check
	),
	instructions;

		/** Internally generated **/
    Instructions ? deref_hash_integer(a(I), a(K), EntryList) :
      Requests ! instruction(deref_hash_integer, (I, K, NArguments), 3),
      Items = [FCP_dg_drf2_hash_integer, RegE(I), RegE(K) | Items'] |
	aux#case_hash(EntryList, NArguments, Requests', Requests'',
			Items', Items''
	),
	instructions;

		/** Internally generated **/
    Instructions ? deref_hash_string(a(I), a(K), EntryList) :
      Requests ! instruction(deref_hash_string, (I, K, NArguments), 3),
      Items = [FCP_dg_drf2_hash_string, RegE(I), RegE(K) | Items'] |
	aux#case_hash(EntryList, NArguments, Requests', Requests'',
			Items', Items''
	),
	instructions;

    Instructions ? allocate_var(Arg) |
	instructions,
	aux#registers([Arg], 1, Requests, Requests', Items,
			[FCP_dg_allocate_var | Items'],
			allocate_var, ''
	);


    Instructions ? allocate_vars(Regs) :
      Requests = [instruction(allocate_vars, (K, Arguments), Count)
		| Requests'],
      Items = [FCP_dg_allocate_vars, K | OtherItems] |
       instructions,
       aux#do_vars(Regs, Arguments, K, 0, Items', OtherItems, Count, 2);

    Instructions ? allocate_tuple(Args, Arg) |
	instructions,
	aux#registers([Arg], 2, Requests, Requests',
		  Items, [FCP_dg_allocate_tuple, Arity | Items'],
		  allocate_tuple, (Arity, Arguments)
	),
	arguments # encode(Args, tuple, Requests', Requests'', Items', Items'',
				Arguments, Arity
		    );

    Instructions ? allocate_list(Args, Arg) |
	instructions,
	aux#allocate_list(Args, Arg, Requests, Requests', Items, Items',
			Instructions', Instructions''
	);

    Instructions ? allocate_pr(Args, Arg) |
	instructions,
	aux#allocate_enqueue(Args, Arg, Instructions', Instructions'', Summary),
	aux#allocate_pr(Summary, Requests, Requests', Items, Items');

     Instructions ? fetch(Term, Register) |
	instructions,
	aux#fetch(Term, Register, Requests, Requests', Items, Items');

    otherwise |
	instructions2(Instructions, Requests, EndRequests, Items, EndItems,
			Guards, EndGuards
	).


procedure instructions2(Instructions, Requests, EndRequests, Items, EndItems,
			Guards, EndGuards
).

instructions2(Instructions, Requests, EndRequests, Items, EndItems,
		Guards, EndGuards
) :-

    Instructions ? multiple_copy(Source, Destination) |
	emulated_copy(Source, Destination, Requests, Requests', Items, Items',
			Instructions', Instructions''
	),
	instructions(Instructions'', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? multiple_assign(Source, Destination) |
	assign # multiple_assignment(Source, Destination, Requests, Requests',
		Items, Items', assign),
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? multiple_assign_and_commit(Source, Destination) |
	assign # multiple_assignment(Source, Destination,
		Requests, Requests',
		Items, Items', assign_and_commit),
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? multiple_assign_and_commit_trail(Source, Destination) |
	assign # multiple_assignment(Source, Destination,
		Requests, Requests',
		Items, Items',assign_and_commit_trail),
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? assign_and_increment(a(I), a(J)) :
      Requests ! instruction(assign_and_increment, (I, J), 3),
      Items = [FCP_dg_assign_and_inc, RegE(I), RegE(J) | Items']  |
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

     Instructions ? assign_increment_and_commit(a(I), a(J)) :
      Requests ! instruction(assign_increment_and_commit, (I, J), 3),
      Items = [FCP_dg_assign_inc_com, RegE(I), RegE(J) | Items']  |
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

     Instructions ? assign_increment_and_commit_trail(a(I), a(J)) :
      Requests ! instruction(assign_increment_and_commit_trail, (I, J), 3),
      Items = [FCP_dg_assign_inc_com_trail, RegE(I), RegE(J) | Items']  |
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? goto(Label) :
      Requests = [instruction(goto, Address, 1),
		  label_reference(Label, Reference, Address)
		 | Requests'],
      Items = [FCP_dg_goto_there, Reference | Items'] |
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? if_tag(a(I), "=", Tag, Label) :
      InventedLabel = label(_),
      Instructions'' = [if_tag(a(I), "=\=", Tag, InventedLabel),
			goto(Label),
			InventedLabel
		       | Instructions'] |
	instructions(Instructions'', Requests, EndRequests, Items, EndItems,
			Guards, EndGuards
	);

    Instructions ? if_tag(a(I), "=\=", Tag, Label) |
	aux#if_tag_not(I, Tag, Label, Requests, Requests', Items, Items',
		Instructions', Instructions''
	),
	instructions(Instructions'', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? if_integer(Comparee, Relation, Comparand, Label) |
	aux#special_arg(Comparee, integer(Comparee), Comparee'),
	aux#special_arg(Comparand, integer(Comparand), Comparand'),
	aux#ifs(IfIntegerBaseOp, integer, Comparee', Relation, Comparand', Label,
		   Requests, Requests', Items, Items'
	),
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? if_real(Comparee, Relation, Comparand, Label) |
	aux#ifs(IfRealBaseOp, real, Comparee, Relation, Comparand, Label,
		Requests, Requests', Items, Items'
	),
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? if_string(Comparee, Relation, Comparand, Label) |
	aux#ifs(IfStringBaseOp, string, Comparee, Relation, Comparand, Label,
		Requests, Requests', Items, Items'
	),
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? if_tuple(Comparee, Relation, Comparand, Label) |
	aux#special_arg(Comparee, tuple(Comparee), Comparee'),
	aux#special_arg(Comparand, tuple(Comparand), Comparand'),
	aux#ifs(IfTupleBaseOp, tuple, Comparee', Relation, Comparand', Label,
		   Requests, Requests', Items, Items'
	),
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? switch_on_tag(a(I), TagLabels) :
      Requests = [instruction(switch_on_tag,
			      (I, N, mask(TagBits), Addresses),
			      3
		  ),
                  data_escape(integer(TagBits), TagBitsWord)
		 | Requests'],
      Items = [FCP_dg_switch_on_tag, RegE(I), N, TagBitsWord | Items'] |
	aux#tag_labels(TagLabels, N, TagBits, Requests', Requests'',
			Items', Items'', Addresses
	),
	instructions(Instructions', Requests'', EndRequests, Items'', EndItems,
			Guards, EndGuards
	);

    Instructions ? branch_integer(a(I), Low, High, Labels),
    N := High - Low :
      Requests = [instruction(branch_integer,
			      (I, min(Low), max(High), Addresses),
			      2
		  ),
		  data_escape(integer(Low), LowI),
		  data_escape(integer(High), HighI)
		 | Requests'],
      Items = [FCP_dg_branch_integer, RegE(I), LowI, HighI | Items'] |
	aux#branch_labels(Labels, Requests', Requests'', Items', Items'',
			Addresses, -2, N	% Consistency check
	),
	instructions(Instructions', Requests'', EndRequests, Items'', EndItems,
			Guards, EndGuards
	);

    Instructions ? branch_real(a(I), Low, High, Labels),
    N := High - Low :
      Requests = [instruction(branch_real,
			      (I, min(Low), max(High), Addresses),
			      2
		  ),
		  data_escape(Low, LowR),
		  data_escape(High, HighR)
		 | Requests'],
      Items = [FCP_dg_branch_real, RegE(I), LowR, HighR | Items'] |
	aux#branch_labels(Labels, Requests', Requests'', Items', Items'',
			Addresses, -2, N	% Consistency check
	),
	instructions(Instructions', Requests'', EndRequests, Items'', EndItems,
			Guards, EndGuards
	);

    Instructions ? branch_tuple(a(I), Low, High, Labels),
    N := High - Low :
      Requests = [instruction(branch_tuple,
			      (I, min(Low), max(High), Addresses),
			      2
		  ),
		  data_escape(tuple(Low), LowT),
		  data_escape(tuple(High), HighT)
		 | Requests'],
      Items = [FCP_dg_branch_tuple, RegE(I), LowT, HighT | Items'] |
	aux#branch_labels(Labels, Requests', Requests'', Items', Items'',
			Addresses, -2, N	% Consistency check
	),
	instructions(Instructions', Requests'', EndRequests, Items'', EndItems,
			Guards, EndGuards
	);

    Instructions ? case_hash_integer(Arg, EntryList) |
	aux#registers([Arg], 1, Requests, Requests',
		  Items, [FCP_dg_case_hash_integer | Items'],
		  case_hash_integer, Arguments
	),
	aux#case_hash(EntryList, Arguments, Requests', Requests'',
			Items', Items''
	),
	instructions(Instructions', Requests'', EndRequests, Items'', EndItems,
			Guards, EndGuards
	);

    Instructions ? case_hash_string(Arg, EntryList) |
	aux#registers([Arg], 1, Requests, Requests',
		  Items, [FCP_dg_case_hash_string | Items'],
		  case_hash_string, Arguments
	),
	aux#case_hash(EntryList, Arguments, Requests', Requests'',
			Items', Items''
	),
	instructions(Instructions', Requests'', EndRequests, Items'', EndItems,
			Guards, EndGuards
	);

    Instructions ? compare_integer(Comparee, Relation, Comparand, Label) |
	aux#special_arg(Comparee, integer(Comparee), Comparee'),
	aux#special_arg(Comparand, integer(Comparand), Comparand'),
	aux#compare(CompareIntegerBaseOp, int_, Comparee', Relation, Comparand', 
		   Label, Requests, Requests', Items, Items'
	),
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? decrement(a(I), a(J)) :
      Requests !instruction(decrement_2_reg, (I, J), 3),
      Items = [FCP_dg_decrement_2_reg, RegE(I), RegE(J) | Items'] |
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? decrement_and_commit(a(I), a(J)) :
      Requests !instruction(decrement_and_commit, (I, J), 3),
      Items = [FCP_dg_decrement_and_commit, RegE(I), RegE(J) | Items'] |
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? decrement(a(I), {a(J), IX}) :
      Requests !instruction(decrement_2_xreg, (I, J, IX), 4),
      Items = [FCP_dg_decrement_2_xreg, RegE(I), RegE(J), IndexE(IX)
	      | Items'] |
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? decrement(a(I)) :
      Requests !instruction(decrement, (I), 2),
      Items = [FCP_dg_decrement, RegE(I) | Items'] |
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? decrement_pointer(a(I)) :
      Requests !instruction(decrement_pointer, (I), 2),
      Items = [FCP_dg_decrement_pointer, RegE(I) | Items'] |
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? decrement_pointer(a(I), a(J)) :
      Requests !instruction(decrement_2_pointer, (I, J), 3),
      Items = [FCP_dg_decrement_2_pointer, RegE(I), RegE(J) | Items'] |
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? increment(a(I), a(J)) :
      Requests !instruction(increment_2_reg, (I, J), 3),
      Items = [FCP_dg_increment_2_reg, RegE(I), RegE(J) | Items'] |
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? increment_and_commit(a(I), a(J)) :
      Requests !instruction(increment_and_commit, (I, J), 3),
      Items = [FCP_dg_increment_and_commit, RegE(I), RegE(J) | Items'] |
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? increment(a(I), {a(J), IX}) :
      Requests !instruction(increment_2_xreg, (I, J, IX), 4),
      Items = [FCP_dg_increment_2_xreg, RegE(I), RegE(J), IndexE(IX)
	      | Items'] |
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? increment(a(I)) :
      Requests !instruction(increment, (I), 2),
      Items = [FCP_dg_increment, RegE(I) | Items'] |
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? increment_pointer(a(I)) :
      Requests !instruction(increment_pointer, (I), 2),
      Items = [FCP_dg_increment_pointer, RegE(I) | Items'] |
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? increment_pointer(a(I), a(J)) :
      Requests !instruction(increment_2_pointer, (I, J), 3),
      Items = [FCP_dg_increment_2_pointer, RegE(I), RegE(J) | Items'] |
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? plus(IntArg1, IntArg2, RegArg) |
	aux#do_integer_arith(plus_, Plus_BaseOp, IntArg1, IntArg2, RegArg, 
				Requests, Requests', Items, Items'),
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? plus_and_commit(IntArg1, IntArg2, RegArg) |
	aux#do_integer_arith(cmt_plus_, CmtPlus_BaseOp, IntArg1, IntArg2,
			     RegArg, Requests, Requests', Items, Items'),
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? plus_number(NumArg1, NumArg2, Reg) |
	aux#do_number_arith(plusnum_, Plus_number_BaseOp, NumArg1, NumArg2,
			    Reg, Requests, Requests', Items, Items'),
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? plus_number_commit(NumArg1, NumArg2, Reg) |
	aux#do_number_arith(cmt_plusnum_, CmtPlus_number_BaseOp,
			    NumArg1, NumArg2, Reg,
			    Requests, Requests', Items, Items'),
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? diff(IntArg1, IntArg2, RegArg) |
	aux#do_integer_arith(diff_, Diff_BaseOp, IntArg1, IntArg2, RegArg, 
				Requests, Requests', Items, Items'),
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? diff_and_commit(IntArg1, IntArg2, RegArg) |
	aux#do_integer_arith(cmt_diff_, CmtDiff_BaseOp,IntArg1, IntArg2,
			     RegArg, Requests, Requests', Items, Items'),
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? diff_number(NumArg1, NumArg2, Reg) |
	aux#do_number_arith(diffnum_, Diff_number_BaseOp, NumArg1, NumArg2,
			    Reg, Requests, Requests', Items, Items'),
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? diff_number_commit(NumArg1, NumArg2, Reg) |
	aux#do_number_arith(cmt_diffnum_, CmtDiff_number_BaseOp,
			    NumArg1, NumArg2, Reg, Requests, Requests',
			    Items, Items'),
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? enqueue(PId, a(I)) :
      Requests = [instruction(enqueue, (I, PId @ Address), 2),
		  procedure_reference(PId, Reference, Address)
		 | Requests'],
      Items = [FCP_dg_enqueue, RegE(I), Reference | Items'] |
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? iterate :
      Requests = [instruction(iterate, Address, 1),
		  iterative_reference(Reference, Address)
		 | Requests'],
      Items = [FCP_dg_iterate, Reference | Items'] |
	filter_redundant_suspension(Instructions', Instructions''),
	instructions(Instructions'', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? iterate(PId) :
      Requests = [instruction(iterate, (PId @ PAddr), 1),
		  procedure_reference(PId, PReference, PAddr)
		| Requests'],
      Items = [FCP_dg_iterate1, PReference | Items'] |
	filter_redundant_suspension(Instructions', Instructions''),
	instructions(Instructions'', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? execute(PId) :
      Requests = [instruction(execute, (PId @ PAddr), 1),
		  procedure_reference(PId, PReference, PAddr)
 		 | Requests'],
      Items = [FCP_dg_execute1, PReference | Items'] |
	filter_redundant_suspension(Instructions', Instructions''),
 	instructions(Instructions'', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? execute(PId1, PId2):
      Requests = [instruction(execute, (PId1 @ PAddr1, PId2 @ PAddr2), 1),
		  procedure_reference(PId1, PReference1, PAddr1),
		  procedure_reference(PId2, PReference2, PAddr2)
		| Requests'],
      Items = [FCP_dg_execute2, PReference1, PReference2 | Items'] |
	filter_redundant_suspension(Instructions', Instructions''),
	instructions(Instructions'', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? halt :
      Requests ! instruction(halt, '', 1),
      Items ! FCP_dg_halt |
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? commit :
      Requests ! instruction(commit_nolabel, '', 1),
      Items ! FCP_dg_commit_nolabel  |
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

   
    Instructions ? set_cp_arity(Arity) :
      Requests ! instruction(set_cp_arity, Arity, 2),
      Items = [FCP_dg_set_cp_arity, Arity | Items'] |
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? suspend(PId1, PId2) :
      Requests = [instruction(suspend, (PId1 @ PAddr1, PId2 @ PAddr2), 1),
		  procedure_reference(PId1, Reference1, PAddr1),
		  procedure_reference(PId2, Reference2, PAddr2)
		 | Requests'],
      Items = [FCP_dg_suspend2, Reference1, Reference2 | Items'] |
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? suspend(ContinueId) :
      Requests = [instruction(suspend, ContinueId @ Address, 1),
		  procedure_reference(ContinueId, Reference, Address)
		 | Requests'],
      Items = [FCP_dg_suspend1, Reference | Items'] |
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? suspend :
      Requests ! instruction(suspend, '', 1),
      Items ! FCP_dg_suspend0 |
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? suspend_on(a(I)) :
      Requests ! instruction(suspend_on, I, 2),
      Items = [FCP_dg_suspend_on, RegE(I) | Items'] |
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? set_HBT :
      Requests ! instruction(set_HBT, '', 1),
      Items ! FCP_dg_set_HBT |
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? undo :
      Requests ! instruction(undo, '', 1),
      Items ! FCP_dg_undo |
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? list_assign_with_check(Arg, a(I)) |
	aux#registers([Arg], 2, Requests, Requests',
		Items, [FCP_dg_list_assign_with_check, RegE(I) | Items'],
		list_assign_with_check, I),
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);

    Instructions ? list_assign(Arg, a(I)) |
	aux # registers([Arg], 2, Requests, Requests',
			Items, [FCP_dg_list_assign, RegE(I) | Items'],
			list_assign, I),
	instructions(Instructions', Requests', EndRequests, Items', EndItems,
			Guards, EndGuards
	);
/* ... */

    Instructions = [] :
      Requests = EndRequests,
      Items = EndItems,
      Guards = EndGuards ;

    otherwise, Instructions ? Instr :
      Requests = _, EndRequests = _, Instructions' = _, 
      Items = _, EndItems = _, Guards = _, EndGuards = _ |
	computation#diagnostic(("unknown instruction" : Instr)).


procedure emulated_copy(Source, Destination, Requests, Requests, Items, Items,
			Instructions, Instructions
).

emulated_copy(Source, Destination, Requests1, Requests2, Items1, Items2,
		Instructions1, Instructions2
) :-

     true:
      Instructions1 = Instructions2 |
	assign # multiple_copy(Source, Destination, Requests1, Requests2,
				Items1, Items2
		 ).

procedure filter_redundant_suspension(Instructions, Instructions).

filter_redundant_suspension(Instructions1, Instructions2) :-

    Instructions1 = [multiple_copy(_,_), deschedule | Instructions1'] :
      Instructions1' = Instructions2 ;

    Instructions1 ? deschedule :
      Instructions1' = Instructions2 ;

    otherwise :
      Instructions1 = Instructions2 .
