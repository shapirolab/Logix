-language(biospi).

global(a,b,c).
baserate(1).


Cytoplasm::= membrane << Receptor | Signal_Mol1 | Signal_Mol2 >> .

Receptor::= p2c c ! [] , Receptor.

Signal_Mol1::= molecule << Domain1 | Domain2 .
			   Domain1::= c2p c ? [] , local b ! [] , Domain1 .
			   Domain2::= local b ? [] , s2s a ! [] , Domain2 >> .

Signal_Mol2::= molecule << Domain3 | Domain4 .
			   Domain3::= s2s a ? [] , local b ! [] , Domain3 .
			   Domain4::= local b ? [] , screen#display("Signal") | Domain4 >> .

