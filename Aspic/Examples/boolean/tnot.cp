-language(spifcp).

global(b).

RunT ::= btest#Test | boolean#TT(b) | bnot#NotB .

RunF ::= btest#Test | boolean#FF(b) | bnot#NotB .
