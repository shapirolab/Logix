-language(spifcp).

public(b1,b2).

RunTT ::= btest#Test |  boolean#TT(b1) |  boolean#TT(b2) |  band#AndB .

RunFT ::= btest#Test |  boolean#FF(b1) |  boolean#TT(b2) |  band#AndB .

RunTF ::= btest#Test |  boolean#TT(b1) |  boolean#FF(b2) |  band#AndB .

RunFF ::= btest#Test |  boolean#FF(b1) |  boolean#FF(b2) |  band#AndB .
