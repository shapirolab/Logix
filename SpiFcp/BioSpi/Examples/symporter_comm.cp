-language(biospi).

global(cell1, cell2, cell3, cell4).
baserate(1).


System::= molecule(<<Molecule1>>) | molecule(<<Molecule2>>) | 
	  molecule(<<Molecule1>>) | molecule(<<Molecule2>>) |
	  cell(<<Symporter_In | Symporter_Out>>) .

Molecule1::= s2s cell1 ? {sym} , <<enter sym , screen#display("molecule1 entered") | Molecule1 ;
			           s2s sym ? [] , Molecule1 >> ;
	     c2p cell2 ? {sym} , <<exit sym ,  screen#display("molecule1 exited") | Molecule1 ;
				   c2p sym ? [] , Molecule1 >> .

Molecule2::= enter cell3 , screen#display("molecule2 entered") | Molecule2 ;
	     exit  cell4 , screen#display("molecule2 exited") | Molecule2 .

Symporter_In+symI::= s2s cell1 ! {symI} , << accept cell3 , accept symI , Symporter_In ;
					     s2s symI ! [] , Symporter_In >> .
	 
Symporter_Out+symO::= p2c cell2 ! {symO}, << expel cell4 , expel symO, Symporter_Out ;
					     p2c symO ! [] , Symporter_Out >> .




