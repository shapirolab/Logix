-language(biospi).

public(c).
baserate(1).


System::= Vesicle | cell(<<Membrane>>) .

Vesicle::= membrane(<< merge + c , screen#display("vesicle merged") >>) .

Membrane::= merge - c , Membrane .



