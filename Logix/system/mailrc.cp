-language([inherit, dfcp]).
-mode(trust).
-export([aliases/1, find/3, name/2, tokens/2]).

/* find/3
**
** find(String, ListOfAddresses^)
**
** Tokenize String and search .mailrc for each Name in it.
** Commas in the String are treated as White Space.
** For each name which is found, the search is iterated for
** each of the equivalent names.  Each name which is not
** found is returned in ListOfAddresses.  Terms in the String
** which cannot be names are returned in the Lost list.
**
*/

find(String, List, Lost) :-
	tokens(String, Tokens),
	filter(Tokens?, Names, Lost),
	aliases,
	search(Names?, Aliases?, List).


/* tokens/2
**
** tokens(String, ListOfTokens^)
**
** Tokenize String, producing ListOfTokens.
**
** A token is a comma (",") or a list of characters, not including
** a comma nor white space (" ", <tab>, <cr>, ...).
**
*/

tokens(In, Tokens) :-

    string(In) |
	string_to_dlist(In, In', []),
	self;

    In ? C, C =:= ascii(',') |
	Tokens ! ",",
	self;

    In ? C, C =< ascii(' ') |
	self;

    In ? C,
    otherwise |
	Head = [C | Tail?],
	token;

    In = [] |
	Tokens = [].

token(In, Head, Tail, Tokens) :-

    In ? C, C =< ascii(' ') |
	Tail = [],
	list_to_string(Head, Token),
	Tokens ! Token?,
	tokens;

    In ? C, C =:= ascii(',') |
	Tail = [],
	list_to_string(Head, Token),
	Tokens ! Token?,
	tokens;

    In ? C,
    otherwise |
	Tail ! C,
	self;

    In = [] |
	Tail = [],
	list_to_string(Head, Token),
	Tokens = [Token?].

filter(Tokens, Names, Lost) :-

    Tokens ? Name,
    string(Name), Name =\= ',' |
	Names ! Name,
	self;

    Tokens ? ',' |
	self;

    Tokens ? `Name |
	Names ! Name,
	self;

    Tokens ? string(Name, _) |
	Names ! Name,
	self;

    Tokens ? Other,
    otherwise |
	Lost ! Other,
	self;

    Tokens = [] |
	Names = [],
	Lost = [] .


/* name/2
**
** name(Name, ListOfAddresses^)
**
** Search .mailrc aliases for Name.  If found, the search is
** iterated for each equivalent name.  Each name which is not
** found is returned in ListOfAddresses.
**
*/

name(Name, List) :-
	aliases,
	search([Name], Aliases?, List).

search(Names, Aliases, List) + (Residue = []) :-

    Names ? Name,
    Aliases ? Name(Equivs) |
	append(Names'?, Equivs, Names''),
	self;

    Names = [Name | _ ],
    Aliases ? Alias,
    Alias = Other(_), Name =\= Other |
	Residue' = [Alias | Residue],
	self;

    Names ? Name,
    Aliases = [] |
	List ! Name,
	search(Names'?, Residue, List');

    Names = [] | Aliases = _, Residue = _,
	List = [].

append(Xs, Ys, Zs) :-

    Xs ? X |
	Zs ! X,
	self;

    Xs = [] |
	Zs = Ys .

/* aliases/1
**
** aliases(Aliases^) returns a list of aliases of the form:
**
**	Name(ListOfAddresses)
**
** This list is extracted from the .mailrc file in the home directory
** of the used of the Logix System.
**
** Each address in ListsOfAddresses is a string of non-blank, contiguous 
** characters, including '.', '!', etc. from an alias statement in the
** users ~/.mailrc.
*/

aliases(Aliases) :-
	processor # interface(user_data({_, HomeDir})),
	utils # append_strings([HomeDir?, '/', ".mailrc"], Mailrc),
	file # get_file(Mailrc?, MR, [], _),
	string_to_dlist(MR?, MRC, []),
	utils # chars_to_lines(MRC?, MRL),	
	alias_list(Aliases, MRL?).

alias_list(Aliases, MRL) :-

    MRL ? L |
	string_to_dlist(L, CL, []),
	alias(CL?, MRL'?, MRL'', Aliases, Aliases'?),
	self;

    MRL = [] |
	Aliases = [] .

alias(CL, M1, M2, A1, A2) :-

    CL = [A, L, I, A, S, WS | Rest],
    A =:= ascii(a),
    L =:= ascii(l),
    I =:= ascii(i),
    S =:= ascii(s),
    WS =< ascii(' ') |
	scan(Rest, Rest', Id, M1, M1'),
	list(Id?, Rest'?, M1'?, M2, A1, A2);

    otherwise | CL = _,
	M2 = M1,
	A1 = A2 .


list(Id, Rest, M1, M2, A1, A2) :-

    Id = "" | Rest = _,
	M2 = M1,
	A1 = A2 ;

    Id =\= "" |
	A1 ! Id(Equivs?),
	scan(Rest, Rest', Id', M1, M1'),
	equivs(Equivs, Rest'?, M1'?, M2, A1', A2, Id'?).


equivs(Equivs, Rest, M1, M2, A1, A2, Id) :-

    Id = "" | Rest = _,
	Equivs = [],
	M2 = M1,
	A1 = A2 ;

    Id =\= "" |
	Equivs ! Id,
	scan(Rest, Rest', Id', M1, M1'),
	self.


scan(Rest, Next, Id, M1, M2) :-

    Rest = [],
    M1 ? "" |
	self;

    Rest = [],
    M1 ? L,
    string(L),
    L =\= "" |
	string_to_dlist(L, Rest', []),
	scan1;

    Rest = [],
    otherwise : Next = [],
      Id = "",
      M2 = M1;

    Rest ? C, C =< ascii(' ') |
	self;

    Rest ? C, C > ascii(' ') |
	M2 = M1,
	scanid(Rest', Next, Id, [C | Cs?], Cs).

scan1(Rest, Next, Id, M1, M2, L) :-

    Rest ? C,
    C > ascii(' ') | Rest' = _,
	Id = "",
	Next = Rest,
	M2 = [L | M1];

    otherwise | L = _,
	scan.

scanid(Rest, Next, Id, LCs, Cs) :-

    Rest ? C, C > ascii(' ') |
	Cs ! C,
	self;

    otherwise |
	Next = Rest,
	Cs = [],
	list_to_string(LCs, Id).
