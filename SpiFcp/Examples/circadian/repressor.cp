-language(spifcp).
-include(rates).

export(R_GENE , R_RNA , R_PROTEIN).
global(bR(R8),t6a(Think6a),utrR(R9),degmR(R10),degpR(R11),pR(R5),rbs(R6)).

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




