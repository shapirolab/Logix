-language(spifcp).

export(RunTT, RunF1, RunF2, RunFF).
public(b1,b2,c).

TT(b) ::= b?{t,f}, t![], 0 .

FF(b) ::= b?{t,f}, f![], 0 .

Test+(t,f) ::= c!{t,f}, <<f?[], screen#display("It's false") ;
			 t?[], screen#display("It's true")>> .

AndB ::=	c?{t,f}, <<x . b1!{x,f}, x?[], b2!{t,f}, 0>> .

RunTT ::= Test | AndB | TT(b1) | TT(b2) .

RunF1 ::= Test | AndB | FF(b1) | TT(b2) .

RunF2 ::= Test | AndB | TT(b1) | FF(b2) .

RunFF ::= Test | AndB | FF(b1) | FF(b2) .


