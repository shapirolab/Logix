-language(spifcp).

public(c).

Test+(t,f) ::= c!{t,f},
		<<
			f?[] , screen#display("It's false") ;
			t?[] , screen#display("It's true")
		>>
.
