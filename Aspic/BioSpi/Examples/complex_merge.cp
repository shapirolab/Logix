-language(biospi).

global(complexAB,breakAB,breakAB1(infinite),pa).
baserate(1).


System::= molecule<<ProteinA>> | molecule<<ProteinB>> .

ProteinA+pa(infinite)::= Domain1A | Domain1B. 

ProteinB::= merge - complexAB, local breakAB ? [] , expel breakAB1 , ProteinB .

Domain1A::= merge + complexAB , screen#display("complex formed") | 
				<< local breakAB ! [] , local pa ! [] , 
				   molecule<<merge +  pa , exit breakAB1 , 
				   screen#display("complex broken") | Domain1A>> >> .

Domain1B::= local pa ? [] , molecule<<merge - pa, Domain1B>> .  


 



