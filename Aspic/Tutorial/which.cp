-language(spifcp).

True + (true, false) ::=
	Send(true) | Answer(true, false) .

False + (true, false) ::=
	Send(false) | Answer(true, false) .

Send(it) ::=
	it ! [] , 0 .

Answer(yes, no) ::=
	yes ? [] , screen#display('Too true!') ;
	no ?  [] , screen#display('Too bad!')  .
