-language(biospi).

global(cell1, cell2).
baserate(1).


System::= molecule<<Molecule>> | molecule<<Molecule>> | 
	  molecule<<Molecule>> | molecule<<Molecule>> |
	  cell<<Porin>> .

Molecule::= enter cell1 , screen#display("molecule entered") | Molecule ;
	    exit cell2 ,  screen#display("molecule exited")  | Molecule .

Porin::= accept cell1 , Porin ;
	 expel cell2 , Porin .




