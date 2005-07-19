/*
Circadian rhythm emulation

Aviv Regev

Last update by          $Author: bill $
                        $Date: 2005/07/19 10:23:56 $
Currently locked by     $Locker:  $
                        $Revision: 1.3 $
                        $Source: /home/qiana/Repository/Aspic/Examples/circadian/think.cp,v $

Copyright (C) 1999, Weizmann Institute of Science - Rehovot, ISRAEL

*/

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


