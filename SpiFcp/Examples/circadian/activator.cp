-language(spifcp).
-include(rates).

export(A_GENE , A_RNA , A_PROTEIN).
public(pA(R1) , bA(R2) , t2a(Think2a) , utrA(R3) , degmA(R4) , degpA(R12) ,
       pR(R5) , rbs(R6)).


A_GENE ::=

<< PROMOTED_A | BASAL_A .

PROMOTED_A ::=
    pA ? {e} , ACTIVATED_TRANSCRIPTION_A(e) .

BASAL_A ::=
    bA ? [] , BASAL_A | A_RNA .

ACTIVATED_TRANSCRIPTION_A(e) ::=
    t2a ? [] , A_RNA | ACTIVATED_TRANSCRIPTION_A ;
    e   ? [] , PROMOTED_A   
>>
.

A_RNA ::=

<< TRANSLATION_A + DEGRADATION_mA .

TRANSLATION_A ::=
    utrA ? [] , A_RNA | A_PROTEIN  .

DEGRADATION_mA ::=
    degmA ? [] , true

>>
.


A_PROTEIN ::=

<< e1(infinite) , e2(R7a) , e3(R7b) . PROMOTION_AR + BINDING_R + DEGRADATION_A .

PROMOTION_AR ::=
    pA ! {e2} , e2 ! [] , A_PROTEIN ;
    pR ! {e3} , e3 ! [] , A_PROTEIN 

.

BINDING_R ::=
    rbs ! {e1} , BOUND_A_PROTEIN

.

BOUND_A_PROTEIN ::=
    degpA ? [] , e1 ! [] , true ;
    e1 ? [] , A_PROTEIN 

.

DEGRADATION_A ::=
    degpA ? [] , true 
	>>

.






