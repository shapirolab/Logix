-language(dfcp).
-mode(interrupt).
-include(string_includes).

convert_to_integer(X, Y) :-
  I := integer(X) |
	Y = I;

  otherwise |
	X = _,
	Y = [].

ncompare(String1, String2, Length, Ok) :-
  string(String1), string(String2) |
	string_to_dlist(String1, Chars1, []),
	string_to_dlist(String2, Chars2, []),
	strncmp(Chars1?, Chars2?, Length, Ok);

  otherwise | Length = _, String1 = _, String2 = _,
	Ok = false.

strncmp(Chars1, Chars2, Count, Ok) + (Temp = 1) :-
  Chars1 ? C1, Chars2 ? C2, C1 =\= C2 |
	Chars1' = _, Chars2' = _, Temp = _, Count = _,
	Ok = false;

  Chars1 ? C1, Chars2 ? C2, C1 = C2, Temp < Count |
	Temp++,
	self;

  Chars1 ? C1, Chars2 ? C2, C1 = C2, Temp = Count |
	Chars1' = _, Chars2' = _,
	Ok = true;

  Chars1 = [] |
	Chars2 = _, Temp = _, Count = _,
	Ok = false;

  Chars2 = [] |
	Chars1 = _, Temp = _, Count = _,
	Ok = false;

  Count < Temp |
	Chars1 = _, Chars2 = _,
	Ok = false.

extract_pathname(String, PathName, Ok) :-
  string(String) |
	string_to_dlist(String, Chars, []),
	extract_pathname1(Chars?, String, PathName, Ok);

  otherwise |
	String = _,
	PathName = undefined,
	Ok = false("not a string").

extract_pathname1(Chars, String, PathName, Ok) :-
  Slash := ascii("/"), Chars = [C1, C2 | _],  C1 = Slash, C2 =\= Slash |
	list_to_string(Chars, PathName),
	Ok = true,
	String = _;

  Slash := ascii("/"), Chars = [C1],  C1 = Slash |
	PathName = "/",
	Ok = true,
	String = _;

  Slash := ascii("/"), Chars = [C1, C2 | Chars'],  C1 = Slash, C2 = Slash |
	self; % ignore double slash - start of machine name

  Chars = [], string(String) |
	PathName = String,
	Ok = true;

  Chars ? _, otherwise |
	self.
	
strip_last_slash(Chars, String1, String2) :-
  String1 = "/" |
	Chars = _,
	String2 = String1;

  otherwise |
	strip_last_slash1(Chars, String1, String2).

strip_last_slash1(Chars, String1, String2) :-
  nth_char(string_length(String1), String1) =:= ascii('/') |
	strip_last(Chars, String2);

  otherwise |
	Chars = _,
	String2 = String1.

strip_last(Chars, String) + (Head, Tail)  :-

  initially | Head = Tail? ;

  Chars ? C, Chars' =\= [] |
	Tail ! C,
	self;

  Chars = [_ | []] | % last character
	Tail = [],
	list_to_string(Head, String).
	
  
basename(UnixPath, Name) + (NL = [], NX = 0) :-

  nth_char(string_length(UnixPath)-NX, UnixPath) =:= ascii('/'),
  NL = [] |
	Name = "" ;

  nth_char(string_length(UnixPath)-NX, UnixPath) =:= ascii('/'),
  NL =\= [],
  ground(NL) |
	list_to_string(NL, Name);

  string_length(UnixPath) =:= NX,
  NL = [] |
	Name = "";

  string_length(UnixPath) =:= NX,
  NL =\= [] |
  	Name = UnixPath ;

  otherwise,
  C := nth_char(string_length(UnixPath)-NX++, UnixPath) |
	NL' = [C | NL],
	self.


center_truncate(String, Field, Centered) :-

  string(String),
  Field < string_length(String) |
	string_to_dlist(String, DL, []),
	truncate_chars;

  otherwise,
  string(String),
  Shift := (Field - string_length(String)) |
	string_to_dlist(String, DL, []),
	center_chars.

truncate_chars(DL, Field, Centered) + (Head, Tail) :-

  initially | Head = Tail? ;

  Field-- > 0,
  DL ? C |
	Tail ! C,
	self;

  Field =< 0 |
	DL = _,
	Tail = [],
	list_to_string(Head, Centered).

center_chars(DL, Shift, Centered) :-

  Shift > 1,
  Shift -= 2 |
	DL' = [BLANK | DL],
	self;

  Shift =< 1,
  ground(DL) |
	list_to_string(DL, Centered).

/* Routines to construct/extract internet and simple userids */

internet_names(Simples, Domain, FullNames) :-
  string(Simples) |
	internet_name(Simples, Domain, FullNames);

  Simples ? Simple, string(Domain) |
	internet_name(Simple, Domain, FullName),
	FullNames ! FullName? ,
	self;

  Simples = [] |
	Domain = _,
	FullNames = [].

internet_name(Simple, Domain, FullName):-% If Simple is not already an internet
				 	% name in the form a@b, returns 
					% Simple@Domain in FullName; 
					% otherwise, returns Simple in FullName
  string(Simple) |
	is_simple(Simple, Ok),
	do_fullname(Ok?, Simple, Domain, FullName).

do_fullname(Ok, Simple, Domain, FullName) :-
  Ok = true |
	internet_fullname(Simple, Domain, FullName);
	
  Ok =\= true |
	Domain = _,
	FullName = Simple.

internet_fullname(Simple, Domain, FullName) :-
  Domain =\= "" |
	utils#append_strings([Simple, "@", Domain], FullName);

  Domain = "" |
	FullName = Simple.

is_simple(Simple, Ok) :-
	string_to_dlist(Simple, Chars, []),
	is_simple1(Chars?, Ok).

is_simple1(Chars, Ok) :-
  
Chars ? AT_SIGN |
	Chars' = _,
	Ok = false(not_simple);
  
  Chars ? DOT |
	Chars' = _,
	Ok = false(not_simple);

  Chars ? PERCENT |
	Chars' = _,
	Ok = false(not_simple);

  Chars ? C, C=\= AT_SIGN, C =\= DOT, C =\= PERCENT |
	self;

  Chars = [] |
	Ok = true.


extract1(Delim, FullName, Chars, SimpleName, Rest) + (S, SH = S?) :-
  Chars ? Delim |
	S = [], S' = [],
	self;

  S = [] |
	FullName = _, Delim = _,
	list_to_string(SH, SimpleName),
	list_to_string(Chars, Rest);

  Chars ? C, C =\= Delim |
	S = [C | S'?],
	self;

  Chars = [], unknown(S) |
	S = _, SH = _, Delim = _,
	SimpleName = FullName,
	Rest = false(unknown).

  
extract(Delim, FullName, SimpleName, Rest) :-
  string(FullName) |
	string_to_dlist(FullName, Chars, []),
	extract1(Delim, FullName, Chars?, SimpleName, Rest).
		
internet_split(FullName, SimpleName, DomainName) :-
  string(FullName) |
	string_to_dlist(FullName, Chars, []),
	extract1(AT_SIGN, FullName, Chars?, SimpleName, DomainName).

/*
** format_words/4 - transform In string to Out string.
**
** Input:  In ::= String.
**         Length ::= Integer. - Maximum line length for lines of Out.
**         Indent ::= Integer. - Number of spaces to indent second and subsequent
**                               lines of Out.
** Output: Out ::= String.
**
** Format: Spaces at the beginning of a line of In are preserved in Out.
**         Spaces preceding a word which is not at the beginning of a line of In
**         are preserved unless the it is the first word of a line of Out.
**         All line-feeds <lf> are preserved.
**         Out always ends with a line-feed (i.e. one is added if In does not end
**         with a line-feed).
**         In may begin with a line-feed; Out also begins with a line-feed, and
**         all subsequent lines are indented by Indent spaces.
**         A line of Out may be longer than Length if it consists of only one "word"
**         (and preceding spaces when it was the first word of a line of In).
*/

format_words(In, Length, Indent, Out) :-
  string(In) |
  string_to_dlist(In, Cs, []),
      tokens(Cs?, Tokens),
      indentation(Indent, Blanks),
      list_to_string(Blanks?, Indentation),
      concatenate_tokens.

/*
** tokens/2 - Convert character list Cs to Tokens list.
**
** Input:  Cs ::= [Character].
** Output: Tokens ::= [(word(Size, Head, Tail); blanks(Number, Head, Tail); eol)].
*/

tokens(Cs, Tokens) + (Type, Count, Head, Tail) :-

  initially |
    Type = new,
    Head = Tail?,
    Count = 0 ;

  Cs ? BLANK,
  Type =\= word,
  Count++  |
    Tail ! BLANK,
    Type' = blanks,
      self;

  Cs = [BLANK | _],
  Type = word |
    Tokens ! Type(Count, Head, Tail),
      tokens;

  Cs ? C,
  C > BLANK,
  Type =\= blanks,
  Count++ |
    Tail ! C,
    Type' = word,
      self;

  Cs = [C | _],
  C > BLANK,
  Type = blanks |
    Tokens ! Type(Count, Head, Tail),
      tokens;

  Cs ? C,
  C =:= LF,
  Type =\= word |
    Count = _,
    Head = _,
    Tail = _,
    Tokens ! eol,
      tokens;

  Cs ? C,
  C =:= LF,
  Type = word |
    Tokens ! Type(Count, Head, Tail),
    Tokens' ! eol,
      tokens;

  Cs ? C,
  C < BLANK,
  C =\= LF |
      self;

  Cs = [],
  Type =\= word |
    Count = _,
    Head = _,
    Tail = _,
    Tokens = [];

  Cs = [],
  Type = word |
      Tokens = [Type(Count, Head, Tail)].

/*
** indentation/2 - create list of Indent spaces.
**
** Input:  Indent ::= Integer.
** Output: Blanks ::= [BLANK] ; [].
*/

indentation(Indent, Blanks) :-

  Indent-- > 0 |
    Blanks ! BLANK,
      self;

  otherwise | Indent = _,
    Blanks = [] .  

/*
** concatenate_tokens/4 - concatenate Tokens characters into Out string.
**
** Input:  Tokens
**         Length
**         Indentation ::= String. - of spaces.
** Output: Out
*/

concatenate_tokens(Tokens, Length, Indentation, Out)
 + (NC, Previous, HCs, TCs) :-

  initially |
    Previous = eol,
    NC = 0,
    HCs = TCs? ;

  Tokens ? blanks(Count, BWs, TCs'),
  Previous = eol,
  NC += Count |
    TCs = BWs,
      self;

  Tokens ? word(Count, HWs, TCs'),
  Previous = eol,
  NC += Count |
    TCs = HWs,
    Previous' = word,
      self;

  Tokens ? word(Count, HWs, TCs'),
  NC += Count, NC' =< Length,
  Previous = blanks(HBs, TBs) |
    TCs = HBs,
    TBs = HWs,
    Previous' = word,
      self;

  Tokens ? word(Count, TWs, TCs''),
  Previous =\= eol,
  NC + Count > Length,
  string(Indentation),
  NC' := string_length(Indentation) + Count |
  string_to_dlist(Indentation, TCs', TWs),
    Previous' = word,
    TCs ! LF,
      self;

  Tokens ? blanks(Count, Head, Tail),
  Previous =\= eol,
  NC += Count, NC' =< Length |
    Previous = _,
    Previous' = blanks(Head, Tail),
      self;

  Tokens ? blanks(Count, _Head, _Tail),
  Previous =\= eol,
  NC + Count > Length,
  string(Indentation),
  NC' := string_length(Indentation) |
  string_to_dlist(Indentation, TCs', TCs''?),	% Next must be word/3
    Previous = _,
    TCs ! LF,
    Previous' = eol,
      self;

  Tokens ? eol,
  Tokens' ? word(Count, HWs, TCs''),
  string(Indentation),
  NC' := string_length(Indentation) + Count |
  string_to_dlist(Indentation, TCs', HWs),
    NC = _,
    Previous = _,
    TCs ! LF,
    Previous' = word,
      self;

  Tokens ? eol,
  Tokens' ? blanks(Count, BWs, TCs''),
  string(Indentation),
  NC' := string_length(Indentation) + Count |
  string_to_dlist(Indentation, TCs', BWs),
    NC = _,
    Previous = _,
    TCs ! LF,
    Previous' = eol,
      self;

  Tokens ? eol,
  Tokens' =\= [_(_,_,_) | _] |		% must be [eol|_] or []
    Previous = _,
    TCs ! LF,
    Previous' = eol,
      self;

  Tokens = [],
  Previous =\= eol |
    TCs ! LF,
    Previous' = eol,
      self;

  Tokens = [],
  Previous = eol |
    Length = _,
    Indentation = _,
    NC = _,
    TCs = [],
      list_to_string(HCs, Out).

% Change all occurences of Char1 in String1 to Chars2; return result in String2
change(String1, Char1, Chars2, String2) :-
  string(String1) |
	string_to_dlist(String1, Chars, []),
	string_to_dlist(Char1, [AChar1], []),
	string_to_dlist(Chars2, AChars2, []),
	change1(Chars?, AChar1?, AChars2?, String2).

change1(Chars, AChar1, AChars2, String2)  + (Head, Tail)  :-

  initially | Head = Tail? ;

  Chars ? C,  C =\= AChar1 |
        Tail ! C,
        self;

  Chars ? C, C = AChar1, ground(AChars2) |
        change2(Tail, Tail'?, AChars2),
        self;

  Chars = [] |
	AChar1 = _, AChars2 = _, Tail = [],
	list_to_string(Head, String2).

change2(Tail, Tail1, AChars2) :-

  AChars2 = [] |
	Tail = Tail1;

  AChars2 ? AChar |
	Tail ! AChar,
	self.

% generate a message of size Size PERCENT (%) characters
gen_message(Size, Msg)
+ (Counter = 0, Head, Tail) :-

  initially | Head = Tail? ;

  Counter < Size |
	Tail ! PERCENT,
	Counter++,
	self;

  Counter = Size |
	Tail = [],
	list_to_string(Head, Msg).

/* RETURNS ((NewDate - OldDate) > MaxDifference) ? true; false
 * Must first extract the last maximimum integer of the seconds part of the 
 * dates so we can do the comparison
*/
date_diff_bigger(NewDate, OldDate, MaxDifference, Reply) :-
  string(NewDate), string(OldDate), integer(MaxDifference) |
	extract(DOT, NewDate, NewDate1, _),
	extract(DOT, OldDate, OldDate1, _),
	get_last(SIGNIFICANT_DIGITS, NewDate1?, NewDate2),
	get_last(SIGNIFICANT_DIGITS, OldDate1?, OldDate2),
	convert_to_integer(NewDate2?, N),
	convert_to_integer(OldDate2?, O),
	date_diff1(N?, O?, MaxDifference, Reply).


date_diff1(N, O, MaxDifference, Reply) :-
  Diff := N - O ,
  Diff =< MaxDifference |
	Reply = false;

  Diff := N - O , Diff > MaxDifference |
	Reply = true;

  otherwise |
	N = _, O = _, MaxDifference = _,
	computation#display(
		"Doors Internal NonFatal Warning - Can't compute date"),
	Reply = false.
  
	
get_last(Ndigits, String1, String2) :- 
  string(String1), Len := string_length(String1), Len > Ndigits,
  StartPos := Len - Ndigits + 1 |
	get_last1(Len, StartPos, String1, String2);

  string(String1), string_length(String1) =< Ndigits |
	String2 = String1;

  otherwise |
	Ndigits = _,
	String1 = _,
	String2 = 0.

get_last1(Len, Pos, String1, String2)  + (S, SS = S?) :-
  C :=  nth_char(Pos, String1), Pos =< Len, integer(Pos) |
%	computation#display(term, Pos, type(ground)),
	S ! C,
	Pos++,
	self;

 Pos > Len |
	String1 = _,
	S = [],
	list_to_string(SS, String2).
  
	
