-language(biospi).

public(b,c,d,e).
baserate(1).


Cytoplasm::= membrane(<< Receptor | Signal_Mol1 | Signal_Mol2 >>) .

Receptor::= << p2c c ! {d} , screen#display("Signal_rec_to_Dom1_c") | Active_Receptor.
            
	       Active_Receptor::= p2c e ? {f1} , p2c f1 ! {d} , screen#display("Signal_rec_to_Dom1_f") | Active_Receptor >> .

Signal_Mol1+f::= molecule(<< Domain1 | Domain2 .

			     Domain1::= c2p c ? {d} , local b ! {d} , screen#display("Signal_Dom1_to_Dom2_b1") | Domain1 ;
					c2p f ? {d} , local b ! {d} , screen#display("Signal_Dom1_to_Dom2_b2") | Domain1 .

			     Domain2::= local b ? {d} , s2s d ! {e,f} , screen#display("Signal_Dom2_to_Dom3_d") | Domain2 >>) .

Signal_Mol2::= molecule(<< Domain3 | Domain4 .

			   Domain3::= s2s d ? {e1,f1} , local b !  {e1,f1} ,  screen#display("Signal_Dom3_to_Dom4_b") | Domain3 .

			   Domain4::= local b ? {e2,f2} , c2p e2 ! {f2} , screen#display("Signal_Dom4_to_rec_e") | Domain4 >>) .

