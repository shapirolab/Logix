/* $Header: /home/qiana/Repository/Logix/system/decode/output.cp,v 1.1.1.1 1999/07/09 07:03:31 bill Exp $ */
-export([server/3]).
-include(dg_opcodes).
-include(escapes).
-language([evaluate,compound,colon]).
-mode(trust).

RQs ::= strings(Integer) ; [RQ | RQs].

RQ ::= increment(Integer) ; instruction(String, Any, Count) ;
       escape(Escape) ; escape(Escape, Any).

Escape ::= {Integer, Integer}.

procedure server(RQs, [Any], StringRefs).
procedure server(RQs, [Any], StringRefs, Integer).

server(RQs, Pretty, StringRefs) + (Offset = 8) :-

    RQs ? increment(I),
    Offset' := Offset + 2*I |
	server;

    RQs ? instruction(OpCode, Arguments, I),
    Offset' := Offset + 2*I :
      Pretty ! (Offset - OpCode : Arguments) |
	server;

    RQs ? escape({LabelE, LabelOffset}, Address^),	% label reference
    Address := Offset + 2*LabelOffset,
    Offset' := Offset + 2 |
	server;

    RQs ? escape({StringE, StringOffset}, String),	% string reference
    Offset' := Offset + 4,
    BaseOffset := Offset + StringOffset :
      StringRefs ! String(BaseOffset) |
	server;

    RQs ? escape({IntegerE, Value}, Value^),		% integer word
    Offset' := Offset + 4 |
	server;

    RQs ? escape({TupleE, Value}, Value^),		% tuple word
    Offset' := Offset + 4 |
	server;

    RQs ? escape({ProcE, ProcedureOffset}, Address^),	% procedure reference
    Address := Offset + ProcedureOffset,
    Offset' := Offset + 4 |
	server;

    RQs ? escape({IterE, IterativeOffset}, Address^),	% iterative reference
    Address := Offset + IterativeOffset,
    Offset' := Offset + 4 |
	server;

    RQs ? escape({RealE, Value}, Value^),	% real
    Offset' := Offset + 8 |
	server;

    RQs ? escape({SInfoE, InfoOffset}),	% start procedure - info reference
    Base := Offset + 8,			% allow for procedure string header
    Offset' := Base + 4,
    BaseOffset := Base + InfoOffset :
      Pretty ! (Base * 'procedure_info' @ BaseOffset) |
	server;

    RQs ? escape({ModuleE, NameOffset}),	% module name reference
    Offset' := Offset + 4,
    BaseOffset := Offset + NameOffset :
      StringRefs ! Name(BaseOffset),
      Pretty ! Name |
	server;

    RQs ? escape({PInfoE, NameOffset, Arity, Index}),	% procedure info
    Base := 4*((Offset + 2)/4),
    Offset' := Base + 16,				% 3 words a null-word
    BaseOffset := Base + NameOffset :
      StringRefs ! Name(BaseOffset),
      Pretty ! (Base * procedure_info(Name, Arity, Index)) |
	server;

    RQs = strings(Base^),
    Base := 4*((Offset + 2)/4) :
      Pretty = [],
      StringRefs = [] .
