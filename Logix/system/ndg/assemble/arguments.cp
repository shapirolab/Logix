/* $Header: /home/qiana/Repository/Logix/system/ndg/assemble/arguments.cp,v 1.1 1999/07/09 07:02:59 bill Exp $ */
-export([encode/7, encode/8]).
-mode(trust).
-include(dg_opcodes).
-include(registers).
-language([evaluate,compound,colon]).

ArgCode ::= RegisterAC ; PRAC ; IndirectAC ; RegisterXAC ; 
	    RoAC ; CarAC ; ReferenceXAC ; RoXAC ;
	    NilAC ; IntegerAC ; UnsignedAC ; RealAC ; StringAC ; VariableAC .

RegisterAC	::= 65.		% a(I) -> 65,I
PRAC		::= 66.		% PR(I) -> 66,I
IndirectAC	::= 67.		% *(a(I)) -> 67,I
RegisterXAC	::= 68.		% {a(I),IX} -> 68,I,IX
RoAC		::= 69.		% ro(a(I)) -> 69,I or 70,I
CarAC		::= 76.		% car(a(I)) -> 76,I
ReferenceXAC	::= 77.		% &({a(I),IX}) -> 77,I,IX
ReferenceRoXAC	::= 78.		% ro("&"({a(I),IX})) -> 78,I,IX or 79,I,IX
				% "&"(ro({a(I),IX})) -> 78,I,IX or 79,I,IX
NilAC		::= 80.		% [] -> 80
IntegerAC	::= 88.		% integer(Integer), Integer ->
				%			88,IntegerReference
WordAC		::= 88.		% tuple(Arity) -> 88,TupleReference
RealAC		::= 89.		% Real -> 89,RealReference or 93,RealReference
StringAC	::= 94.		% String -> 94,StringReference
VariableAC 	::= 95.		% {'_'} -> 95 or 96

Display ::= a(Number) ; "PR"(Number) ; "*a"(Number) ; a(Number, Index) ;
	    ro(Number) ; car(Number) ; "&a"(Number, Index) ;
	    "&"(ro(Number, Index)) ; Operand .
Operand ::= Nil ; integer(Integer) ; tuple(Integer) ;
	    real(Real) ; string(String) ; we .


Kind ::= list ; tuple ; process.

procedure encode(Arguments, Kind, Requests, Requests, Items, Items, [Display]).
procedure encode(Arguments, Kind, Requests, Requests, Items, Items, [Display],
			Count
).
procedure encode(Arguments, Kind, Requests, Requests, Items, Items, [Display],
			Count, Count
).

encode(Arguments, Kind, Requests1, Requests2, Items1, Items2, Args) :-
	encode(Arguments, Kind, Requests1, Requests2, Items1, Items2, Args,
		_, 0
	).

encode(Arguments, Kind, Requests1, Requests2, Items1, Items2, Args, N)
		+ (K = 0) :-

    Arguments ? a(I), K' := K + 1 :
      Requests1 ! increment(2),
      Items1 = [FCP_dg_load_reg, RegE(I) | Items1'],
      Args ! a(I) |
	encode;

    Arguments ? "PR"(I), K' := K + 1 :
      Requests1 ! increment(2),
      Items1 = [FCP_dg_load_pr_arg, PRegE(I) | Items1'],
      Args ! "PR"(I) |
	encode;

    Arguments ? "*"(a(I)), K' := K + 1 :
      Requests1 ! increment(2),
      Items1 = [FCP_dg_load_reg_indirect, RegE(I) | Items1'],
      Args ! "*a"(I) |
	encode;

    Arguments ? {a(I),IX}, K' := K + 1 :
      Requests1 ! increment(3),
      Items1 = [FCP_dg_load_subarg, RegE(I), IndexE(IX) | Items1'],
      Args ! a(I, IX) |
	encode;

    Arguments ? ro(a(I)), K' := K + 1 :
      Requests1 ! increment(2),
      Items1 = [ArgOp, RegE(I) | Items1'],
      Args ! ro(I) |
	encode,
	which_kind(Kind, Arguments', FCP_dg_load_ro_of_reg, ArgOp);

    Arguments ? car(a(I)), K' := K + 1 :
      Requests1 ! increment(2),
      Items1 = [FCP_dg_load_car_of_reg, RegE(I) | Items1'],
      Args ! car(I) |
	encode;

    Arguments ? "&"({a(I), IX}), K' := K + 1 :
      Requests1 ! increment(3),
      Items1 = [FCP_dg_load_ref_to_subarg, RegE(I), IndexE(IX) | Items1'],
      Args ! "&a"(I, IX) |
	encode;

    Arguments ? "&"(ro({a(I), IX})), K' := K + 1 :
      Requests1 ! increment(3),
      Items1 = [ArgOp, RegE(I), IndexE(IX) | Items1'],
      Args ! "&"(ro(I, IX)) |
	encode,
	which_kind(Kind, Arguments', FCP_dg_load_ro_of_subarg, ArgOp);

    Arguments ? ro("&"({a(I), IX})), K' := K + 1 :
      Requests1 ! increment(3),
      Items1 = [ArgOp, RegE(I), IndexE(IX) | Items1'],
      Args ! "&"(ro(I, IX)) |
	encode,
	which_kind(Kind, Arguments', FCP_dg_load_ro_of_subarg, ArgOp);

    Arguments ? [], K' := K + 1 :
      Requests1 ! increment(1),
      Items1 ! FCP_dg_load_nil,
      Args ! [] |
	encode;

    Arguments ? Integer, integer(Integer), K' := K + 1 :
      Argument = integer(Integer),
      Requests1 ! increment(1),
      Requests1' ! data_escape(Argument, Reference),
      Items1 = [FCP_dg_load_word, Reference |  Items1'],
      Args ! Argument |
	encode;

    Arguments ? Argument, Argument = integer(_), K' := K + 1 :
      Requests1 ! increment(1),
      Requests1' ! data_escape(Argument, Reference),
      Items1 = [FCP_dg_load_word, Reference |  Items1'],
      Args ! Argument |
	encode;

    Arguments ? Argument, Argument = tuple(_), K' := K + 1 :
      Requests1 ! increment(1),
      Requests1' ! data_escape(Argument, Reference),
      Items1 = [FCP_dg_load_word, Reference |  Items1'],
      Args ! Argument |
	encode;

    Arguments ? Real, real(Real), K' := K + 1 :
      Requests1 ! increment(1),
      Requests1' ! data_escape(Argument, Reference),
      Items1 = [RealArgOp, Reference |  Items1'],
      Argument = real(Real),
      Args ! Argument |
	encode,
	real_kind_code(Kind, Arguments', RealArgOp);

    Arguments ? String, string(String), K' := K + 1 :
      Requests1 ! increment(1),
      Requests1' ! string_reference(String, Reference),
      Items1 = [FCP_dg_load_ref_to_string, Reference |  Items1'],
      Args ! string(String) |
	encode;

    Arguments ? {_}, K' := K + 1 :
      Requests1 ! increment(1),
      Items1 ! ArgOp,
      Args ! we |
	encode,
	which_kind(Kind, Arguments', FCP_dg_load_we_var, ArgOp);

    Arguments = [] : Kind = _,
      N = K,
      Requests1 = Requests2,
      Items1 = Items2,
      Args = [] .

procedure which_kind(Kind, [Any], Integer, Integer).

which_kind(Kind, Arguments, Op, ArgOp) :-

    Kind = tuple : Arguments = _,
      ArgOp = Op ;

    Kind = list, Arguments = [] :
      ArgOp = Op ;

    Kind = list, Arguments =\= [],
    ArgOp^ := Op + 1 |
	true ;

    Kind = process,
    ArgOp^ := Op + 1  : Arguments = _ .

procedure real_kind_code(Kind, Arguments, Integer).

    
real_kind_code(Kind, Arguments, RealArgOp) :-

    Kind = tuple, Arguments = [] :
      RealArgOp = FCP_dg_load_real ;

    Kind = list, Arguments = [] :
      RealArgOp = FCP_dg_load_real ;

    Kind = list, Arguments =\= [] :
      RealArgOp = FCP_dg_load_ref_to_real ;

    Kind = process : Arguments = _,
      RealArgOp = FCP_dg_load_ref_to_real .
