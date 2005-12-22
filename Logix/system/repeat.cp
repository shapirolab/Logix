-language(compound).
-mode(interrupt).
-export(run/1).

run(Specs) :-

	set(1, Specs, Output, []),
	computation # Output? .

set(Count, Input, Output, NextOut) :-

    Count > 0,
    Input ? Set |
	set + (Input = Set),
	set(Count, Input', NextOut', NextOut);

    Count > 0,
    Input =?= (Input', Set) |
	set,
	set(Count, Set, NextOut', NextOut);

    Count > 0,
    Input =?= N*Input',
    integer(N),
    Count' := Count*N |
	self;

    Count > 0,
    Input =?= N*Service#Input',
    integer(N),
    Count' := Count*N :
      Output = [Service#Output'? | NextOut] |
	self + (NextOut = []);

    Count > 0,
    Input =?= Service#Input', Input =\= _*_#_ :
      Output = [Service#Output'? | NextOut] |
	self + (NextOut = []);

    Count-- > 0,
    otherwise,
    Input =\= [] :
      Output ! Input |
	self;

    otherwise :
      Count = _,
      Input = _,
      Output = NextOut .
