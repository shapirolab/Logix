/*

colon.cp - Operator syntax source-to-source transformer for Logix

Author - Jacob Levy, 09/16/86
Revised - Bill Silverman 87-89.

Convert an operator declaration file to a normal source file.

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:03:16 $
Currently locked by 	$Locker:  $
			$Revision: 1.1.1.1 $
			$Source: /home/qiana/Repository/Logix/system/transform/syntax.cp,v $

Copyright (C) 1986, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([transform/5]).
-mode(trust).
-language(compound).


procedure transform([Any], [Any], [Any], [Any], [Any]).

transform(Options1, Terms, Options2, Clauses, Out) :-
    true :
      Options2 = [export([serve/2 | Exports]), mode(Mode) | Options],
      Clauses = Server?,
      Out = Output? |
	options(Options1, Exports, Mode, Options),
	server(Server, Procedures),
	transformer(Terms, Output, Procedures).


options(Options1, Exports, Mode, Options2) :-

    Options1 ? mode(Mode^) : Mode' = _ |
	options;

    Options1 ? export(Exports^) : Exports' = _ |
	options;

    Options1 ? Other,
    otherwise :
      Options2 ! Other |
	options;

    Options1 =\= [_ | _] :
      Mode = trust,
      Exports = [],
      Options1 = Options2 .


server(Server, Procedures) :-

    true :
      Server = [(serve([infix_operator(A, B, C, D, E) | Reqs],  S1) :-
			ReServe,
			infix_operator(RoA, B, C, D, E, S1, S2)
		),
		(serve([postfix_operator(A, B, C, D) | Reqs],  S1) :-
			ReServe,
			postfix_operator(RoA, B, C, D, S1, S2)
		),
		(serve([prefix_operator(A, B, C, D) | Reqs],  S1) :-
			ReServe,
			prefix_operator(RoA, B, C, D, S1, S2)
		),
		(serve([A | Reqs], S1) :-
		     otherwise :
			S1 = [A | S2] |
			serve(RoReqs, S2)
		),
		(serve([], S1) :-
		     true :
			S1 = [])
	       | Procedures ],

      A = `'A', RoA = ?'A', B = `'B', C = `'C', D = `'D', E = `'E',
      S1 = `'S1', S2 = `'S2', Reqs = `'Reqs', RoReqs = ?'Reqs',
      ReServe = serve(RoReqs, S2) .


transformer(Terms, Output, Procedures)
	+ (In = In, Pre = Pre, Post = Post, Heads = {In, Pre, Post}) :-

    Terms ? xfx(A, Pri) :
      In ! (infix_operator(A, `'PLeft', `'Pop', `'PRight',
			   `'Reply', `'S1', `'S2'
	    ) :-
		true :	`'PLeft' = Pri, `'Pop' = Pri, `'PRight' = Pri,
			`'Reply' = true, `'S1' = `'S2'
	    ) |
	transformer;

    Terms ? xfy(A, Pri),
    P := Pri - 1 :
      In ! (infix_operator(A, `'PLeft', `'Pop', `'PRight',
			   `'Reply', `'S1', `'S2'
	    ) :-
		true :	`'PLeft' = Pri, `'Pop' = Pri, `'PRight' = P,
			`'Reply' = true, `'S1' = `'S2'
	    ) |
	transformer;

    Terms ? yfy(A, Pri),
    P := Pri - 1 :
      In ! (infix_operator(A, `'PLeft', `'Pop', `'PRight',
			   `'Reply', `'S1', `'S2'
	    ) :-
		true :	`'PLeft' = P, `'Pop' = Pri, `'PRight' = P,
			`'Reply' = true, `'S1' = `'S2'
	    ) |
	transformer;

    Terms ? yfx(A, Pri),
    P := Pri - 1 :
      In ! (infix_operator(A, `'PLeft', `'Pop', `'PRight',
			   `'Reply', `'S1', `'S2'
	    ) :-
		true :	`'PLeft' = P, `'Pop' = Pri, `'PRight' = Pri,
			`'Reply' = true, `'S1' = `'S2'
	    ) |
	transformer;

    Terms ? fx(A, Pri) :
      Pre ! (prefix_operator(A, `'Pop', `'PRight', `'Reply', `'S1', `'S2') :-
		true :	`'Pop' = Pri, `'PRight' = Pri,
			`'Reply' = true, `'S1' = `'S2'
	    ) |
	transformer;

    Terms ? fy(A, Pri),
    P := Pri - 1 :
      Pre ! (prefix_operator(A, `'Pop', `'PRight', `'Reply', `'S1', `'S2') :-
		true :	`'Pop' = Pri, `'PRight' = P,
			`'Reply' = true, `'S1' = `'S2'
	    ) |
	transformer;

    Terms ? xf(A, Pri) :
      Post ! (postfix_operator(A, `'PLeft', `'Pop', `'Reply', `'S1', `'S2') :-
		true :	`'PLeft' = Pri,	`'Pop' = Pri,
			`'Reply' = true, `'S1' = `'S2'
	    ) |
	transformer;

    Terms ? yf(A, Pri),
    P := Pri - 1 :
      Post ! (postfix_operator(A, `'PLeft', `'Pop', `'Reply', `'S1', `'S2') :-
		true :	`'PLeft' = Pri, `'Pop' = P,
			`'Reply' = true, `'S1' = `'S2'
	    ) |
	transformer;

    Terms ? Other,
    otherwise :
      Procedures ! Other,
      Output ! non_operator(Other) |
	transformer;

    Terms = [],
    Heads = {Ins, Pres, Posts} :
      Procedures = Ins,
      A = `'A', B = `'B', C = `'C', D = `'D', E = `'E',
      S1 = `'S1', S2 = `'S2',
      In = [(infix_operator(A, B, C, D, E, S1, S2) :-
		 otherwise : S1 = [infix_operator(A, B, C, D, E) | S2]
	    )
	   | Pres],
      Pre = [(prefix_operator(A, B, C, D, S1, S2) :-
		  otherwise : S1 = [prefix_operator(A, B, C, D) | S2]
	     )
	    | Posts],
      Post = [(postfix_operator(A, B, C, D, S1, S2) :-
		   otherwise : S1 = [postfix_operator(A, B, C, D) | S2]
	      )
	     ],
      Output = [] .
