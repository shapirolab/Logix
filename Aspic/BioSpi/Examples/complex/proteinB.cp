-language(biospi).

public(complexAB,breakAB,breakAB1(infinite),pa).
baserate(1).


MoleculeB::= molecule(<<ProteinB>>) .

ProteinB::= merge - complexAB, local breakAB ? [] , expel breakAB1 , ProteinB .
