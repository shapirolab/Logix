-language(spifcp).
-include(rates).

export(TAU).
public(t2a(Think2a) , t6a(Think6a)).


TAU ::=
	<<
	
	THINK(t2a) | THINK(t6a)  .

THINK(t) ::=
    	    t ! [] , THINK
	>>

.


