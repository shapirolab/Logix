/* $Header: /home/qiana/Repository/Logix/system/widgets/typed.cp,v 1.1 1999/07/09 07:03:23 bill Exp $ */
-monitor(alter).
-language(compound).
-export([off/0, on/0]).

List ::= [Any].
Truth ::= true ; false.

procedure on.
procedure off.

procedure alter(List).
procedure alter(List, Truth, Truth).

/*
** Filter input to transform.
**
** States: Not filtering (On = true)  -->  Filtering <= "off"
**         Filtering (On = false)  --> Not Filtering <= "on"
**
** While filtering, change all calls to completely transform a module,
** to instead omit the typed transformation, so as to suppress creation
** of the _type attribute for compiled modules.
**
*/


alter(In) + (On = true, Off = _) :-

    In ? off,
    On = true : On' = false |
	trap # filter(transform, _Goals, Calls),
	modify(Calls, Off),
	alter;

    In ? on,
    On = true |
	alter;

    In ? off,
    On = false |
	alter;

    In ? on,
    On = false :
      On' = true,
      Off = false,
      Off' = _ |
	alter;

    In = [] :
      On = Off |
	true;

    In ? Other,
    Other =\= on, Other =\= off |
	alter,
	fail(Other, unknown).


procedure modify(List, Truth).

modify(Calls, Off) :-

    unknown(Off),
    Calls ? trapped(_ # languages(A1,fcp,A3,A4,A5,A6,A7,A8),
		    changed(languages(A1,typed,A3,A4,A5,A6,A7,A8))^
	    ) |
	modify;

    otherwise,
    Calls ? trapped(_, false^) |
	modify;

    Off = false : Off' = true |
	transform # untrap,
	modify;

    Calls = [] : Off = _ |
	true.
