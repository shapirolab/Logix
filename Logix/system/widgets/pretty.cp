/* $Header: /home/qiana/Repository/Logix/system/widgets/pretty.cp,v 1.2 2002/06/26 06:48:36 bill Exp $ */
-export([context / 3, module / 2, intermediate / 2, term / 2]).
-mode(interrupt).
-language(compound).

procedure context(Any, String, [String]).
procedure module((String ; [Any] ; Intermediate), [String]).
procedure intermediate(Intermediate, [String]).
procedure term(Any, [String]).

Intermediate ::= intermediate(String, [Procedure]).
Procedure ::= procedure(String/Integer, [Clause]).
Clause ::= {Atom, {[Atom], [Atom]}, [Atom]}.
Atom ::= String ; Tuple.


context(Context, Name, ListOfStrings) :-
  string(Name) |
      computation_utils #
                call_list([file # execute_in_context(Context,
                  			get_source(Name,Chars,Status,_,_)
				  )
			  ],
                          Result
                ),
      parse # characters(Chars?, Parsed, Errors),
      check_status(Result, Status, Errors, Output),
      computation # display(stream, Output,
			    [prefix(pretty(Name)), type(ground)]
		    ),
      clauses(Parsed, ListOfStrings).


check_status(Result, Status, Errors, Output) :-

  Result = true,
  Status = found :
    Errors = Output ;

  Result = true,
  Status =\= found :
    Output = [false(Status) | Errors] ;

  Result =\= true : Status = _,
    Output = [Result | Errors] .


module(Data, ListOfStrings) :-

  string(Data) |
      context(computation, Data, ListOfStrings);

  tuple(Data) |
      intermediate(Data, Source),
      clauses(Source, ListOfStrings);

  list(Data) |
      clauses(Data, ListOfStrings);

  Data = [] :
    ListOfStrings = [] .


intermediate(Data, Source) :-

  Data = module(Name, Procedures) |
      computation # display(term, module(Name), close(done, Done)),
      intermediate_procedures(Procedures, Source, Done).

intermediate_procedures(Procedures, Source, Done) :-

  Procedures ? procedure(Ident, Clauses) |
      computation # display(term, Ident, [known(Done), prefix("procedure"),
					  close(Done, Done')]
		    ),
      intermediate_clauses(Clauses, Source, Source'),
      self;

  Procedures = [] : Done = _,
    Source = [] .

intermediate_clauses(Clauses, Source1, Source2) :-

  Clauses ? Clause :
    Source1 ! Clause' |
      intermediate_clause(Clause, Clause'),
      self;

  Clauses = [] :
    Source1 = Source2 .


intermediate_clause(Clause1, Clause2) :-

  Clause1 = Head({[], []}, Body),
  list(Body) :
    Clause2 = (Head :- Body') |
      intermediate_list(Body, Body');

  Clause1 = Head(Ask([]), Body),
  list(Ask) :
    Clause2 = (Head :- Ask' | Body') |
      intermediate_list(Ask, Ask'),
      intermediate_list(Body, Body');

  Clause1 = Head(Ask(Tell), Body),
  list(Tell) :
    Clause2 = (Head :- Ask' : Tell' | Body') |
      intermediate_list(Ask, Ask'),
      intermediate_list(Tell, Tell'),
      intermediate_list(Body, Body');

  Clause1 = Head({[], []}, []) :
    Clause2 = Head .


intermediate_list(List, Conjunction) :-

  List ? Predicate,
  List' =\= [] :
    Conjunction = (Predicate, Conjunction') |
      self;

  List = [Predicate] :
    Conjunction = Predicate ;

  List = [] :
    Conjunction = true .


term(Term1, Term2) :-
  true :
    LF = _ |
      termlf.

termlf(Term1, Term2, LF) :-
  true :
    Left = LF,
    Right = false |
      term1.

term1(Term1, Term2, Left, Right) :-

  Term1 = [] :
    Term1 = Term2,
    Left = Right ;

  number(Term1) :
    Term1 = Term2,
    Left = Right ;

  string_length(Term1) > 1,
  string_to_dlist(Term1, Chars, []),
  Term1 =\= "procedure",
  Chars ? C, ascii(a) =< C, C =< ascii(z) |
      unquoted(Term1, Chars', Term2, Left, Right);

  string_length(Term1) =:= 1,
  nth_char(1, Term1, C),
  ascii(a) =< C, C =< ascii(z) :
    Term2 = Term1,
    Left = Right;

  Term1 = "" :
    Term2 = """""",
    Left = Right ;

  string_to_dlist(Term1, Chars, []),
  Chars ? C, C =:= ascii($) |
      unquoted(Term1, Chars', Term2, Left, Right);

  Term1 = '"' :
    Term2 = "'""'",
    Left = Right ;

  Term1 = '''' :
    Term2 = "''''",
    Left = Right ;

  string_length(Term1) =:= 1,
  nth_char(1, Term1, Char),
  otherwise :
    ascii('''', Quote),
    ascii("""", DoubleQuote) |
      single_character(Quote, DoubleQuote, Char, Term2, Left, Right);

  string(Term1),
  otherwise,
  string_to_dlist(Term1, List, []) :
    ascii('"', Q) |
      requote(List, [Q | Quoted], Q, Quoted, Term2, Left, Right);

  Term1 = `Ident |
      variable(Term1, Ident, [], Term2, Left, Right);

  Term1 = ?Ident :
    ascii('?', Ro) |
      variable(Term1, Ident, [Ro], Term2, Left, Right);

/* kluge for mosaic_doors log */
  invalid(Term1, Reason),
  string_to_dlist("INVALID[", IL, RL),
  string_to_dlist(Reason, RL, [RSB]),
  RSB := ascii(']') :
    Left = Right |
      list_to_string(IL, Term2);

  Term1 ? Car :
    Term2 ! Car' |
      term1(Car, Car', Right', Right),
      self;

  otherwise,
  arg(1, Term1, Functor),
  Arity := arity(Term1) |
      tuple(Functor, Arity, Term1, Term2, Left, Right).


single_character(Quote, DoubleQuote, Char, Term, Left, Right) :-

  Char =:= ascii(lf),
  list_to_string([Char, Quote, Char, Quote, Char], String) :
    DoubleQuote = _,
    Right = _,
    Term = String,
    Left = true ;

  otherwise,
  list_to_string([DoubleQuote, Char, DoubleQuote], String) :
    Quote = _,
    Term = String,
    Left = Right .


unquoted(String1, Chars, String2, Left, Right) :-

  Chars = [] :
    String2 = String1,
    Left = Right ;

  Chars = [Z],
  ascii(a) =< Z, Z =< ascii(z) :
    String2 = String1,
    Left = Right ;

  Chars = [Y, Z],
  ascii(a) =< Y, Y =< ascii(z),
  ascii(a) =< Z, Z =< ascii(z) :
    String2 = String1,
    Left = Right ;

  Chars = [X, Y, Z],
  ascii(a) =< X, X =< ascii(z),
  ascii(a) =< Y, Y =< ascii(z),
  ascii(a) =< Z, Z =< ascii(z) :
    String2 = String1,
    Left = Right ;

  Chars = [W, X, Y, Z],
  ascii(a) =< W, W =< ascii(z),
  ascii(a) =< X, X =< ascii(z),
  ascii(a) =< Y, Y =< ascii(z),
  ascii(a) =< Z, Z =< ascii(z) :
    String2 = String1,
    Left = Right ;

  Chars = [V, W, X, Y, Z | Chars'],
  ascii(a) =< V, V =< ascii(z),
  ascii(a) =< W, W =< ascii(z),
  ascii(a) =< X, X =< ascii(z),
  ascii(a) =< Y, Y =< ascii(z),
  ascii(a) =< Z, Z =< ascii(z) |
      self;

  Chars ? C,
  ascii('A') =< C, C =< ascii('Z') |
      self;

  Chars ? N, ascii('0') =< N, N =< ascii('9') |
      unquoted;

  Chars ? D, D =:= ascii($) |
      self;

  Chars ? UL, UL =:= ascii('_') |
      self;

  Chars ? SQ, SQ =:= ascii('''') |
      self;

  otherwise |
      unquoted1(String1, Chars, String2, 0, Left, Right).

unquoted1(String1, Chars, String2, Count, Left, Right) :-

  Chars ? C,
  ascii(a) =< C, C =< ascii(z),
  Count++ |
      self;

  otherwise,
  Count =\= 0 |
      unquoted;

  otherwise,
  Count = 0,
  string_to_dlist(String1, List, []) : Chars = _,
    ascii('"', Q) |
      requote(List, [Q | Quoted], Q, Quoted, String2, Left, Right).

requote(List, Requoted, Q, Quoted, String, Left, Right) :-

  List ? Q :
    Quoted ! Q, Quoted' ! Q |
      self;

  List ? LF,
  LF =:= ascii(lf) : Right = _,
    Quoted ! LF,
    Right' = true |
      self;

  List ? C,
  otherwise :
    Quoted ! C |
      requote;

  List = [] :
    Quoted = [Q],
    Left = Right |
      list_to_string(Requoted, String).


variable(Term, Ident, Annotation, Variable, Left, Right) :-

  Ident = '_', Annotation = [] : Term = _,
    Left = Right,
    Variable = Ident ;

  Ident = '_', Annotation = [_] : Term = _,
    Left = Right,
    Variable = "_?" ;

  Ident =\= '_',
  string_to_dlist(Ident, NL, []),
  NL ? C, C =:= ascii('_') |
      unquoted(Ident, NL', String, Right', Right),
      unquoted_variable(Term, Ident, Annotation, Variable, "", String,
			Left, Right'
      ) ;

  string_to_dlist(Ident, NL, []),
  NL ? C, ascii('A') =< C, C =< ascii('Z') : NL' = _ |
      unquoted(Ident, NL, String, Right', Right),
      unquoted_variable(Term, Ident, Annotation, Variable, "", String,
			Left, Right'
      ) ;

  Ident = Kind(Name),
  string_to_dlist(Kind, KL, IL),
  convert_to_string(Name, NS),
  string_to_dlist(NS, IL', []) : Term = _,
    ascii('_', UL),
    IL ! UL |
      list_to_string(KL, Name'),
      unquoted(Name', KL, String, Right', Right),
      unquoted_variable(Term, Name', Annotation, Variable, "_", String,
			Left, Right'
      ) ;

  Ident = Kind(Name, Add),
  string_to_dlist(Kind, KL, IL),
  convert_to_string(Name, NS),
  string_to_dlist(NS, IL', AL),
  convert_to_string(Add, AddS),
  string_to_dlist(AddS, AL', []) : Term = _,
    ascii('_', UL),
    IL ! UL,
    AL ! UL |
      list_to_string(KL, Name'),
      unquoted(Name', KL, String, Right', Right),
      unquoted_variable(Term, Name', Annotation, Variable, "_", String,
			Left, Right'
      ) ;

  otherwise,
  arg(1, Term, Functor) : Ident = _, Annotation = _ |
      tuple(Functor, 2, Term, Variable, Left, Right).

unquoted_variable(Term, Name, Annotation, Variable, Prefix, String,
		  Left, Right
) :-

  Name = String,
  string_to_dlist(Prefix, PL, NL),
  string_to_dlist(Name, NL, Annotation) : Term = _,
    Left = Right |
      list_to_string(PL, Variable);

  Name =\= String,
  arg(1, Term, Functor) : Name = _, Annotation = _, Prefix = _ |
      tuple(Functor, 2, Term, Variable, Left, Right).


tuple(Functor, Arity, Tuple, PrettyTuple, Left, Right) :-

  string(Functor),
  0 < string_length(Functor),
  string_length(Functor) < 4,
  2 =< Arity,
  Arity =< 3,
  string_to_dlist(Functor, [First | _], [0]),
  make_tuple(Arity, Tuple1) |
      reserved(Functor, First, Arity, Tuple, Tuple1, PrettyTuple, Left, Right);

  otherwise,
  make_tuple(Arity, Tuple1) : Functor = _ |
      tuple_args(0, Arity, Tuple, Tuple1, PrettyTuple, Left, Right).

reserved(Functor, First, Arity, Tuple, Tuple1, PrettyTuple, Left, Right) :-

% This is only a partial check for non-operators.

  First =:= ascii('_') : Functor = _ |
      tuple_args(0, Arity, Tuple, Tuple1, PrettyTuple, Left, Right);

  ascii('0') =< First,
  First =< ascii('9') : Functor = _ |
      tuple_args(0, Arity, Tuple, Tuple1, PrettyTuple, Left, Right);

  ascii('A') =< First,
  First =< ascii('Z') : Functor = _ |
      tuple_args(0, Arity, Tuple, Tuple1, PrettyTuple, Left, Right);

  ascii('a') =< First,
  First =< ascii('z'),
  Functor =\= "div", Functor =\= "mod" |
      tuple_args(0, Arity, Tuple, Tuple1, PrettyTuple, Left, Right);

  otherwise,
  Arity =?= 2 :
    First = _ |
      parse#syntax#fcp#serve([prefix_operator(Functor, _, _, _),
			      postfix_operator(Functor, _, _, _)], Errs),
      reserved2;

  otherwise,
  Arity =?= 3 :
    First = _ |
      parse#syntax#fcp#serve([infix_operator(Functor, _, _, _, _)], Errs),
      reserved3.

reserved2(Errs, Functor, Arity, Tuple, Tuple1, PrettyTuple, Left, Right) :-
  Errs =?= [_, _] :
    Functor = _ |
      tuple_args(0, Arity, Tuple, Tuple1, PrettyTuple, Left, Right);
  otherwise,
  arg(1, Tuple1, Functor^) :
    Errs = _ |
      tuple_args(1, Arity, Tuple, Tuple1, PrettyTuple, Left, Right).

reserved3(Errs, Functor, Arity, Tuple, Tuple1, PrettyTuple, Left, Right) :-
  Errs =?= [_] :
    Functor = _ |
      tuple_args(0, Arity, Tuple, Tuple1, PrettyTuple, Left, Right);
  otherwise,
  arg(1, Tuple1, Functor^) :
    Errs = _ |
      tuple_args(1, Arity, Tuple, Tuple1, PrettyTuple, Left, Right).

tuple_args(N, A, C, C1, PC, Left, Right) :-

  N < A,
  N' := N + 1,
  arg(N', C, X^),
  arg(N', C1, (Y?)^) |
      term1(X, Y, Right', Right),
      self;

  N = A : C = _,
    C1 = PC,
    Left = Right .


clauses(Source, Strings) :-
      end_of_line(Ends),
      clauses1(Source, _, Strings, Ends).

/* Any compound operator (see system#parse#tokenize) such as ";" should
   be prefixed by a space to prevent concatention with a preceding "?"
   or any compound postfix operator. */
end_of_line({
".
",
" :-
",
",
",
" :
",
" |
",
" ;
"
}^
).

clauses1(Source, Id, Strings, Ends) :-

  Source ? Clause :
     Strings ! Head |
      termlf(Clause, PrettyClause, Lf),
      utils # ground_stream([H, And], Terms),
      terms_to_string # acyclic_grounded_terms_to_string(Terms, 0, 78, Head),
      clause(PrettyClause, H, And, Strings', Post, Ends, Lf),
      identifier(H, Id),
      double_space(Id, Id', Post, Strings''),
      self;

  Source = [] : Ends = _,
    Id = end,
    Strings = [] .


identifier(H, Id) :-

  tuple(H),
  arg(1, H, F),
  N := arity(H) - 1 :
    Id = F / N ;

  otherwise :
    Id = H / 0 .

double_space(Id1, Id2, [
'
'
 | Next]^, Next) :-
  Id1 =\= Id2,
  Id2 =\= end |
      true.
double_space(_, _, Next^, Next) :-
  otherwise |
      true.

clause(Clause, Head, AfterHead, Strings, Post, Ends, Lf) :-

  Clause = (Head^ :- RHS),
  Ends = Stop(Imply, Comma, Colon, Commit, SemiColon) :
    AfterHead = Imply |
      compound(RHS, Strings, Post, Stop, Comma, Colon, Commit, SemiColon, Lf);

  otherwise,
  arg(1, Ends, AfterHead^) : Lf = _,	% Stop
    Clause = Head,
    Strings = Post .

compound(RHS, Strings, Tail, Suffix, Comma, Colon, Commit, SemiColon, Lf) :-

  RHS = (Part; RHS'),
  Part = (_;_) |
      compound(Part, Strings, Strings', SemiColon, Comma, Colon, Commit, SemiColon,
		Lf
      ),
      self;

  RHS = (Part; RHS'),
  Part =\= (_;_) |
      right_hand_side(Part, Strings, Strings', SemiColon,
			Comma, Colon, Commit, Lf
      ),
      self;

  otherwise : SemiColon = _ |
      right_hand_side(RHS, Strings, Tail, Suffix, Comma, Colon, Commit, Lf).

right_hand_side(RHS, Strings, Tail, Suffix, Comma, Colon, Commit, Lf) :-

  RHS = (A : T | B),
  B =\= true |
      goals(A, 2, Comma, Colon, Strings, Tell, Lf),
      goals(T, 4, Comma, Commit, Tell, Body, Lf),
      goals(B, 6, Comma, Suffix, Body, Tail, Lf);

  RHS = (A : T | true),
  string_to_dlist(Suffix, Chars, []) : Commit = _,
    ascii(' ', Space) |
      list_to_string([Space | Chars], Suffix'),
      goals(A, 2, Comma, Colon, Strings, Tell, Lf),
      goals(T, 4, Comma, Suffix', Tell, Tail, Lf);

  RHS = (G | B),
  G =\= (_ : _) : Colon = _ |
      goals(G, 2, Comma, Commit, Strings, Body, Lf),
      goals(B, 6, Comma, Suffix, Body, Tail, Lf);

  RHS = (A : T) : RHS' = (A : T | true) |
      self;

  otherwise : Commit = _, Colon = _ |
      goals(RHS, 6, Comma, Suffix, Strings, Tail, Lf).

goals(RHS, Indent, Comma, Suffix, Left, Right, Lf) :-

  RHS = (Goal, RHS') |
      goals(Goal, Indent, Comma, Comma, Left, Left', Lf),
      self;

  otherwise,
  Length := 78 - Indent : Comma = _,
    Left = [Line' | Right] |
      utils # ground_stream([RHS, Suffix], Terms),
      terms_to_string # acyclic_grounded_terms_to_string(Terms, Indent, Length,
                                                         Line
      ),
      exdent(Lf, Indent, Line, Line').

exdent(Lf, Indent, Line1, Line2) :-

  Lf = false : Indent = _,
    Line2 = Line1 ;

  Lf =\= false,
  string_to_dlist(Line1, Chars, []) |
	exdent1(Chars, Indent, Line2).

exdent1(Chars, Indent, Line) + (Cs, LCs = Cs?) :-

  Chars ? SQ, SQ =:= ascii("'") :
    Cs ! SQ |
      single_quote;

  Chars ? DQ, DQ =:= ascii('"') :
    Cs ! DQ |
      double_quote;

  Chars ? C,
  C =\= 34, C =\= 39 :	% 34 =:= ascii('"'), 39 =:= ascii("'")
    Cs ! C |
      self;

  Chars = [] : Indent = _,
    Cs = [] |
      list_to_string(LCs, Line).

single_quote(Chars, Indent, Line, Cs, LCs, SQ) :-

  Chars = [C, SQ | Chars'] :
    Cs =  [C, SQ | Cs'] |
      exdent1;

  Indent = 2,
  Chars = [LF, SP, SP, SQ | Chars'],
  LF =:= ascii(lf), SP =:= ascii(' ') :
    Cs ! LF,
    Cs' ! SQ |
      exdent1;

  Indent = 4,
  Chars = [LF, SP, SP, SP, SP, SQ | Chars'],
  LF =:= ascii(lf), SP =:= ascii(' ') :
    Cs ! LF,
    Cs' ! SQ |
      exdent1;

  Indent = 6,
  Chars = [LF, SP, SP, SP, SP, SP, SP, SQ | Chars'],
  LF =:= ascii(lf), SP =:= ascii(' ') :
    Cs ! LF,
    Cs' ! SQ |
      exdent1;

  otherwise : SQ = _ |
      exdent1.

double_quote(Chars, Indent, Line, Cs, LCs, DQ) :-

  Chars ? DQ, Chars' =\= [DQ | _] :
    Cs ! DQ |
      exdent1;

  Chars ? DQ, Chars' ? DQ :
    Cs ! DQ,
    Cs' ! DQ |
      self;

  Chars ? LF, LF =:= ascii(lf) :
    Cs ! LF |
      skip(Indent, Chars', Chars''),
      self;

  otherwise,
  Chars ? C : DQ = _,
    Cs ! C |
      self.

skip(Indent, Chars1, Chars2) :-

  Indent-- > 0,
  Chars1 ? _ |
      self;

  Indent =< 0 :
    Chars2 = Chars1 .
