/* $Header: /home/qiana/Repository/Logix/system/block/pretty_printer.cp,v 1.2 2002/06/07 11:57:00 bill Exp $ */
/*
 *  Handles appropriate output of the block-module, according to the
 *  options.
 *
 *  The types used in this module are defined in module
 *  super.
 */

-export([pretty/3]).
-language(compound).
%-mode(trust).

/******* PRETTY **************************************************************/

procedure pretty(ServiceId, BlockedSource, OptionsTuple).

% Waits till the 1st and 3rd argument of the OptionsTuple are
% instantiated, than acts according to these options, possibly
% pretty-printing and writing the block-module.

pretty(RootId, BlockedSource, OptionsTuple) :-

    OptionsTuple = {no_name, _, no_text, _, _, Residue\Residue'},
    RootId ? Name |
	system# widgets # pretty # module(BlockedSource, Residue'),
	write(RootId', Name, Residue);

    OptionsTuple = {_, _, text(Residue), _, _, Residue\Residue'} :
      RootId = _ |
	system # widgets # pretty # module(BlockedSource, Residue');

    OptionsTuple = {name(Name), _, no_text, _, Residue\Residue'},
    RootId ? _ |
	system # widgets # pretty # module(BlockedSource, Residue'),
	write(RootId', Name, Residue);

    otherwise,
    OptionsTuple = {_, _, Text, _, _, _} : RootId = _, BlockedSource = _ |
	unify_without_failure(Text, text([])).

/******* WRITE ***************************************************************/


procedure write(ServiceId, Name, [String]).

% Creates a file Name.cp in Context, and writes the
% strings of list Strings into it.

write(NodeId, Name, Strings) :-
	auxils # append_strings(Name, '.cp', Name_cp),
	file # execute_in_context(NodeId,
				  put_file(Name_cp, Strings, [], _)
			).

/******* (END OF MODULE) *****************************************************/
