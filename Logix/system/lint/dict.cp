/* $Header: /home/qiana/Repository/Logix/system/lint/dict.cp,v 1.2 2002/11/16 11:35:22 bill Exp $ */
/*
 *
 * dict serves the dictionary of the lint program.
 * Its arguments are a set of flags and an input stream of messages.
 * It calls two external modules:  cross  to handle all cross
 * referencing and checks per the input program and per each
 * goal in it, and  defs  to check the data and format of called 
 * procedures.
 *
 * It recognises the input stream messages:
 *
 * 	- ident(Proc, Id, Gi_options)
 * 	- add_proc(Type, Proc, Part, Call, Properties, Proccheck)
 * 	- add_dict(Const, Part)
 * 	- add_var(Var, var/ro/suspend, Part)
 *
 * and the options message which must be the last message:
 *
 *	- options(Options, Ci_options)
 *
 * Dict format:
 * dict(
 *	Current_proc  = P/A-C
 *	Proc_id_list  = [P1/A1-C1, P2/A2-C2, ... ]
 *	Clause_var_internal = [varref(Var, Var_at_list), ... ]
 *	Clause_const_internal = [constref(Const, At_list), ... ]
 *	Clause_proc_internal = [prc(Prc, At_list), ... ]
 *	Proc_var_internal = [varref(Var, Var_at_list), ... ]
 *	Proc_const_internal = [constref(Const, At_list), ... ]
 *	Proc_proc_internal = [prc(Prc, At_list), ... ]
 *	Vars_ref      = [varref(Var, Var_at_list), ... ]
 *	Const_ref     = [constref(Const, At_list), ... ]
 *	Proc_ref      = [prc(Prc, At_list), ... ]
 * )
 *
 * where Var_at_list = [at(Id, var/ro/suspend, Part), ...]
 *       At_list = [at(Id, Part), ...]
 *
 */


-export([dict/2]).
%-mode(trust).
-language(compound).

procedure dict(Messages, Flags).

Messages ::= [Ident | TrailingMessages].
TrailingMessages ::= [LastMessage | Nil] ; [Message | TrailingMessages].

Ident ::= ident(Proc, IdN, {Pi_options, Ci_Options}).
LastMessage ::= [options(Options, {Pi_options, Ci_options})].
Message ::= Ident ;
	    add_proc(Type, Proc, Part, Term, Properties, Proccheck) ;
	    add_dict(X, Part) ;
	    add_var(X, VarRo, Part).

Flags ::= {Flag, Flag, Flag, Flag, Flag, Flag, Flag, Flag, Flag}.
Flag ::= true ; false.

/*
 *
 * dict/2 is the initialization procedure of dict. It initiates the
 * process and calls serve/12, the message stream server.
 *
 * ident/3 must be the first message on the stream.
 *
 */

dict(Flags, Ms) :-
    Flags = {Cvi_flag, Cci_flag, Cpi_flag,
	     Pvi_flag, Pci_flag, Ppi_flag,
	     Mvr_flag, Mcr_flag, Mpr_flag},
    Ms ? ident(Proc, _, _) : Ms' = _ |
	get_term_id(Proc, Id),
	serve(Ms, Id-0, [], Cvi, Cci, Cpi, Pvi, Pci, Ppi, Mvr, Mcr, Mpr),
	tree # binary_tree([Cvi_flag | Cvi], varref),
	tree # binary_tree([Cci_flag | Cci], constref),
	tree # binary_tree([Cpi_flag | Cpi], prc),
	tree # binary_tree([Pvi_flag | Pvi], varref),
	tree # binary_tree([Pci_flag | Pci], constref),
	tree # binary_tree([Ppi_flag | Ppi], prc),
	tree # binary_tree([Mvr_flag | Mvr], varref),
	tree # binary_tree([Mcr_flag | Mcr], constref),
	tree # binary_tree([Mpr_flag | Mpr], prc);

    Ms = [] : Flags = _ .

/*
 *
 * serve/12 is the main procedure of the dict module. It translates the
 * data in the input stream of the module into internal procedure calls
 * and maintains the dictionary.
 *
 */

serve(Ms, IdN, Ref_list, Cvi, Cci, Cpi, Pvi, Pci, Ppi, Mvr, Mcr, Mpr) :-

    Ms ? ident(Proc, IdN', {Pi_options, Ci_options}) :	% ident message
      Cvi ! tree_to_list(Cvi_list), Cvi' ! reset,
      Cci ! reset,
      Cpi ! tree_to_list(Cpi_list), Cpi' ! reset |
	cross # clause_internal(IdN, Cvi_list, Cpi_list, Ci_options),
	get_term_id(Proc, PId),
	ident(PId, IdN, IdN', Ref_list, Ref_list',
	      Pvi, Pvi', Pci, Pci', Ppi, Ppi', Pi_options
	),
	serve;

    Ms ? add_proc(Type, Proc, Part, Call, Properties, Proccheck) :

      Cvi ! get_tree(Cvi_tree),				% add_proc message
      Cpi ! get_tree(Cpi_tree),
      Cpi' ! add({Folder,Proc}, at(IdN, Cmd, Part)),
      Ppi ! add({Folder,Proc}, at(IdN, Cmd, Part)),
      Mpr ! add({Folder,Proc}, at(IdN, Cmd, Part)),
      Type = {Cmd, Folder} |
	check_specific_procedures(IdN, Type, Proc, Part, Call, Properties,
				  Proccheck, g_dict(Cvi_tree, Cpi_tree)
	),
	serve;

    Ms ? add_dict(X, Part) :				% add_dict message
      Cci ! add(X, at(IdN, Part)),
      Pci ! add(X, at(IdN, Part)),
      Mcr ! add(X, at(IdN, Part)) |
	serve;

    Ms ? add_var(X, VarRo, Part) :			% add_var message
      Cvi ! add(X, at(IdN, VarRo, Part)),
      Pvi ! add(X, at(IdN, VarRo, Part)),
      Mvr ! add(X, at(IdN, VarRo, Part)) |
	serve;

    Ms = [options(Options, {Pi_options, Ci_options})],	 % options message
    IdN = Id-_ :
      Cvi = [tree_to_list(Cvi_list)],
      Cci = [],
      Cpi = [tree_to_list(Cpi_list)],
      Pvi = [tree_to_list(Pvi_list)],
      Pci = [tree_to_list(Pci_list)],
      Ppi = [tree_to_list(Ppi_list)],
      Mvr = [tree_to_list(Mvr_list)],
      Mcr = [tree_to_list(Mcr_list)],
      Mpr = [tree_to_list(Mpr_list)] |		% should be the last message
	cross # clause_internal(IdN, Cvi_list, Cpi_list, Ci_options),
	cross # proc_wide(Id , Pvi_list, Pci_list, Ppi_list, Pi_options),
	cross # modulewide([IdN | Ref_list],
				Mvr_list, Mcr_list, Mpr_list, Options).

/*
 *
 * ident/12 handles the ident message.
 * Its functions are:
 *	- Given a procedure identifier (P/A), return a clause
 *	  identification (P/A-N) where N is the index to clauses
 * 	  of the procedure.
 *	- Put the clause identification of the current goal in the dictionary.
 *	- Add the previous clause identification to the reference list of the
 *	  goal.
 *	- Check if the procedure is doubly defined;
 *	  if so, the index continues from the last previous value.
 *
 */

ident(PId, IdN1, IdN2, Ref_list1, Ref_list2,
      Pvi1, Pvi2, Pci1, Pci2, Ppi1, Ppi2, Pi_options
) :-

    IdN1 = PId-N,
    N' := N + 1 : Pi_options = _,
      IdN2 = PId-N',
      Ref_list1 = Ref_list2,
      Pvi1 = Pvi2, Pci1 = Pci2, Ppi1 = Ppi2 |
	true;

    otherwise :
      Ref_list2 = [IdN1 | Ref_list1],
      Pvi1 = [tree_to_list(Pvi_list), reset | Pvi2],
      Pci1 = [tree_to_list(Pci_list), reset | Pci2],
      Ppi1 = [tree_to_list(Ppi_list), reset | Ppi2] |
	clause_identification(PId, Ref_list1, IdN2, Reply),
	cross # proc_wide(PId, Pvi_list, Pci_list, Ppi_list, Pi_options),
	ident_repeating(Reply, IdN2).

/*
 *
 * clause_identification/4 checks if the given procedure identifier appeared
 * previously, and returns the clause identification of the next clause.
 * When this is the first appearance, clause number 1 is returned.
 *
 */

clause_identification(Id, IdNs, IdN, Reply) :-

    IdNs ? IdX,
    IdX =\= Id-_ |
	clause_identification;

    IdNs ? Id-N,
    N' := N + 1 : IdNs' = _,
      IdN = Id-N',
      Reply = true |
	true;

    IdNs = [] :
      IdN = Id-1,
      Reply = false |
	true.

/*
 *
 * ident_repeating/5 checks to see if a procedure was previously
 * defined.
 *
 */

ident_repeating(Reply, IdN) :-

    Reply = true |
	computation # diagnostic(IdN - multiply_defined_procedure);

    Reply = false : IdN = _ |
	true.

/*
 *
 * check_specific_procedures/7 check the arguments of a given call using
 * module  defs , returning the  Properties  of the procedure.  If not a
 * predefined procedure, but a definition, it returns default  Properties .
 *
 * Property is one of: calculate, suspend, suspend_subs, mode(Properties, ...).
 * For further details, refer to module  defs .
 *
 */

check_specific_procedures(Id, Type, Proc, Part, Call, Properties,
				Proccheck, G_dict
) :-

    Type = {use, Folder} |
	defs # check_call(Id, {Folder, Proc}, Part, Call, Properties,
				G_dict, Proccheck
		);

    otherwise : Id = _, Type = _, Proc = _, Part = _,
		Call = _, Proccheck = _, G_dict = _,
      Properties = calculate |
	true.

/*
 *
 * get_term_id/2 returns the procedure identifier of the given
 * Term  (Functor/Arity).
 *
 */

get_term_id(Term, Id) :-

    tuple(Term),
    arg(1, Term, Functor),
    Arity := arity(Term) - 1 :
      Id = Functor/Arity |
	true;

    string(Term) :
      Id = Term/0 |
	true;

    otherwise : Term = _,
      Id = term_error/error |
	true.
