-language(spifcp).

export(AndB).
public(b1,b2,c).

AndB ::= c?{t,f} , << x . b1!{x,f} , x?[] , b2!{t,f}, 0 >> .
