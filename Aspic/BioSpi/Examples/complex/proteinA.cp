-language(biospi).

global(complexAB,breakAB,breakAB1(infinite)).
baserate(1).


MoleculeA::= molecule(<<ProteinA>>) .

ProteinA+pa(infinite)::= << Domain1A | Domain1B. 

  Domain1A::= merge + complexAB ,
		screen#display("complex formed") | 
		<< local breakAB ! [] , local pa ! [] , 
		   molecule(<<merge +  pa , exit breakAB1 ,
			      screen#display("complex broken") | Domain1A
		   >>)
		>> .

  Domain1B::= local pa ? [] , molecule(<<merge - pa, Domain1B>>)
>> .
