/*
Circadian rhythm emulation

Aviv Regev

Last update by          $Author: bill $
                        $Date: 2005/07/19 10:23:56 $
Currently locked by     $Locker:  $
                        $Revision: 1.3 $
                        $Source: /home/qiana/Repository/Aspic/Examples/circadian/repressor.cp,v $

Copyright (C) 1999, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-language(spifcp).
-include(rates).

export(R_GENE , R_RNA , R_PROTEIN).
public(bR(R8),t6a(Think6a),utrR(R9),degmR(R10),degpR(R11),pR(R5),rbs(R6)).

R_GENE ::=

<< PROMOTED_R | BASAL_R .

PROMOTED_R ::=
    pR ? {e} , ACTIVATED_TRANSCRIPTION_R(e) .

BASAL_R ::=
    bR ? [] , BASAL_R | R_RNA .


ACTIVATED_TRANSCRIPTION_R(e) ::=
    t6a ? [] , R_RNA | ACTIVATED_TRANSCRIPTION_R ;
    e   ? [] , PROMOTED_R 

>>
.

R_RNA ::=

<< TRANSLATION_R + DEGRADATION_mR 

.

TRANSLATION_R ::=
    utrR ? [] , R_RNA | R_PROTEIN 
.
	
DEGRADATION_mR ::=
	degmR ? [] , true 
>>
.

R_PROTEIN ::=

<<

BINDING_A + DEGRADATION_R 

.

BINDING_A ::=
    rbs ? {e1} , BOUND_R_PROTEIN(e1)

.

BOUND_R_PROTEIN(e) ::=
    e ? []  , R_PROTEIN ;
    degpR ? [] , e ! [] , true 

.

DEGRADATION_R ::=
    degpR ? [] , true 

>>
.




