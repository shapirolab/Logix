-language(spifcp).

public(yes, no).

True ::= Send(yes) | Answer.

False ::= Send(no) | Answer.

Send(it) ::= it ! [] , 0 .

Answer ::=
	yes ? [] , screen#display('Too true!') ;
	no  ? [] , screen#display('Too bad!')  .
