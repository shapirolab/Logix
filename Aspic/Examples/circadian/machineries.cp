/*
Circadian rhythm emulation

Aviv Regev

Last update by          $Author: bill $
                        $Date: 2005/07/19 10:23:56 $
Currently locked by     $Locker:  $
                        $Revision: 1.3 $
                        $Source: /home/qiana/Repository/Aspic/Examples/circadian/machineries.cp,v $

Copyright (C) 1999, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-language(spifcp).
-include(rates).

export(BASAL_TRANSCRIPTION , BASAL_TRANSLATION , RNA_DEGRADATION , 
       PROTEIN_DEGRADATION).
public(bA(R2),bR(R8),utrA(R3),utrR(R9),degmA(R4),degmR(R10),
       degpA(R12), degpR(R11)).


BASAL_TRANSCRIPTION ::=
    BASAL_TRANSCRIPTION_A | BASAL_TRANSCRIPTION_R .

BASAL_TRANSCRIPTION_A ::=
    bA ! [] , BASAL_TRANSCRIPTION_A .

BASAL_TRANSCRIPTION_R ::=
    bR ! [] , BASAL_TRANSCRIPTION_R .


BASAL_TRANSLATION ::=
    BASAL_TRANSLATION_A | BASAL_TRANSLATION_R .

BASAL_TRANSLATION_A ::=
    utrA ! [] , BASAL_TRANSLATION_A .

BASAL_TRANSLATION_R ::=
    utrR ! [] , BASAL_TRANSLATION_R .


RNA_DEGRADATION ::=
    RNA_DEGRADATION_A | RNA_DEGRADATION_R .

RNA_DEGRADATION_A ::=
    degmA ! [] , RNA_DEGRADATION_A .

RNA_DEGRADATION_R ::=
    degmR ! [] , RNA_DEGRADATION_R .


PROTEIN_DEGRADATION ::=
    PROTEIN_DEGRADATION_A | PROTEIN_DEGRADATION_R .

PROTEIN_DEGRADATION_A ::=
    degpA ! [] , PROTEIN_DEGRADATION_A .

PROTEIN_DEGRADATION_R ::=
    degpR ! [] , PROTEIN_DEGRADATION_R .







