/*
Circadian rhythm emulation

Aviv Regev

Last update by          $Author: bill $
                        $Date: 2005/07/19 10:23:55 $
Currently locked by     $Locker:  $
                        $Revision: 1.4 $
                        $Source: /home/qiana/Repository/Aspic/Examples/circadian/hysteresis.cp,v $

Copyright (C) 1999, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-language(spifcp).
-include(rates).

export(MODULE).
public(pA(R1) , bA(R2) , t2a(Think2a) , utrA(R3) , 
       degmA(R4) , degpA(R12) , pR(R5) , rbs(R6) , bR(R8) , t6a(Think6a) ,
       utrR(R9) , degmR(R10) , degpR(R11)).


MODULE ::=

	activator#A_GENE |
	repressor#R_GENE |
	think#TAU | machineries#BASAL_TRANSCRIPTION |
	machineries#BASAL_TRANSLATION | machineries#RNA_DEGRADATION |
	machineries#PROTEIN_DEGRADATION

.



