-language(spifcp).
-export(['RunTT', 'RunTF', 'RunFT', 'RunFF']).

global([b1, b2, c]).

TT(b) ::= b ? {t,f} , t ! [] , 0 .

FF(b) ::= b ? {t,f} , f ! [] , 0 .

Test(b) ::=
	<< t, f . b ! {t,f} , (	f ? [] , screen#display("It's false");
				t ? [] , screen#display("It's true") ) >> .

AndB ::=
	c ? {t,f} , << x . b1 ! {x,f} , x ? [] , b2 ! {t,f} , 0 >> .

RunTT ::= Test(c) | AndB | TT(b1) | TT(b2) .

RunFT ::= Test(c) | AndB | FF(b1) | TT(b2) .

RunTF ::= Test(c) | AndB | TT(b1) | FF(b2) .

RunFF ::= Test(c) | AndB | FF(b1) | FF(b2) .


