-language(biospi).

export(Mole,Cell,Expel).
public(cell1, cell2, public, N).
baserate(1).

Expel ::= {N-- > 0} , (expel public , Expel);
          {N =< 0} , 0 .

Mole ::= molecule(
  <<
    exit public , Molecule .
    Molecule ::= enter cell1 , Molecule ;
		 exit cell2 ,  Molecule
  >>
 )
.

Cell ::= cell(
  <<
    exit public , Porin .
    Porin ::= accept cell1 , Porin ;
	      expel cell2 , Porin
  >>
 )
.



