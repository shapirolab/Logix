-language(biospi).

global(bind, in, receptor_IC).
baserate(1).


System::= molecule(<<Ligand>>) | cell(<<Receptor|Receptor|Pit>>) .

Ligand+(lbb, lbb1, lig,lig1(infinite),lig2,lig3,lig4)::=

  << LBD | LBD .

     LBD::= s2s bind ! {lig,lig1,lig2,lig3,lig4} , Bound_LBD .

     Bound_LBD::= local lbb ! [] , enter lig2 , merge + lig3 ,
			local lbb1 ! [] , screen#display("ligand_in_pit") ;
		  local lbb ? [] , local lbb1 ? [] , 0 
  >> .

Receptor+(recbb,recbb1,recbb2) ::=

  << EC | molecule(<< IC >>) .

    
      EC::=
        s2s bind ? {cross_lig, cross_lig1, cross_lig2, cross_lig3, cross_lig4},
        p2c recbb ! {cross_lig, cross_lig1} ,

	<< 
	    Bound_EC .

      		Bound_EC::= p2c recbb1 ? [] , 
		
      		<< 
		  accept cross_lig2 , local cross_lig4 ! [] , 
		  molecule(<<
	  	  	merge - recbb2 , merge - cross_lig3 , local in ! [] ,
				screen#display("One_EC_in")
		  >>) ;
		  local cross_lig4 ? [] ,
		  molecule(<<
		    merge - recbb2 , local in ! [] ,
			screen#display("Other_EC_in")
		  >>)
		>>
	>>.

      IC::= c2p recbb ? {cross_lig,cross_lig1} , 

	<< s2s cross_lig ! [] , AIC ;
	   s2s cross_lig ? [] , AIC .

           AIC::= merge - receptor_IC , merge + cross_lig1 ,
			c2p recbb1 ! [] , merge + recbb2 ,
		    		screen#display("One_IC_in");
		  merge - cross_lig1 , c2p recbb1 ! [] , merge + recbb2 ,
				screen#display("Other_IC_in")
	>>
  >> .
 
Pit::=
    membrane(<<
	merge+ receptor_IC , local in ? [] , local in ? [] , 
		screen#display("Pit_full")
    >>)
.






