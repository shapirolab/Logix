/* $Header: /home/qiana/Repository/Logix/system/transform/inherit/self.cp,v 1.1 1999/07/09 07:03:21 bill Exp $ */
-language(dfcp).
-export(transform).
-mode(trust).

transform(Attributes1, Clauses1, Attributes2, Clauses2, Errors1) :-
	Attributes2 ! export(Exports?),
	complete # module(Attributes1, Clauses1, Attributes1', Completed),
	utilities # update_attributes(Attributes1'?, Attributes2'),
	edit # procedures(Completed?, Edited, Errors1, Errors2?),
	probate # procedures(Edited?, Expanded, Errors2),
	rename # procedures(Expanded?, Renamed, Exports),
	unparse # procedures(Renamed?, Clauses2).
