/* $Header: /home/qiana/Repository/Logix/system/lint/tree.cp,v 1.2 2002/11/16 11:35:23 bill Exp $ */
/*
 * tree is the module that handles the dictionary trees. The tree management
 * is done by binary_tree/2; the other procedures are exported for programming
 * convenience only. They handle fetch and transformation to lists.
 *
 */

-export([binary_tree/2, tree_fetch/3]).
%-mode(trust).
-language(compound).

/*
 *
 *
 *
 *
 *
 */

procedure binary_tree(Request, Meta).

binary_tree(In, Meta) + (Tree = nil) :-

    In ? add(Key, Value) |
	tree_search(add, Key, [Value], Tree, Tree'),
	binary_tree;

    In ? put(Key, Value) |
	tree_search(put, Key, Value, Tree, Tree'),
	binary_tree;

    In ? fetch(Key, Value) |
	tree_fetch(Key, Value, Tree),
	binary_tree;

    In ? reset :
      Tree = _, Tree' = nil |
	binary_tree;

    In ? tree_to_list(List) |
	tree_to_list(Tree, List, Meta),
	binary_tree;

    In ? get_tree(Tree^) |
	binary_tree;

    In ? dummy : Meta = _, Tree = _ |
	dummy_binary_tree(In');

    In ? real_dummy : In' = _, Meta = _, Tree = _ |
	true;

    In = [] : Meta = _, Tree = _ |
	true.


tree_search(Action, Key, Data, Tree, New_tree) :-

    Tree = nil : Action = _,
      New_tree = tree(Key, Data, nil, nil) |
	true;

    Tree = tree(Key1, List1, Tree', Right),
    Key @< Key1 :
      New_tree = tree(Key1, List1, New_tree', Right) |
	tree_search;

    Tree = tree(Key1, List1, Left, Tree'),
    Key1 @< Key :
      New_tree = tree(Key1, List1, Left, New_tree') |
	tree_search;

    Action = add,
    Tree = tree(Key, List, Left, Right),
    Data = [Value] :
      New_tree = tree(Key, [Value | List], Left, Right) |
	true;

    Action = put,
    Tree = tree(Key, _List, Left, Right) :
      New_tree = tree(Key, Data, Left, Right) |
	true.


procedure tree_fetch(Any, Any, Tree).

tree_fetch(Key, Value, Tree) :-

    Tree = tree(Key1, _, Tree', _),
    Key @< Key1 |
	tree_fetch;

    Tree = tree(Key1, _, _, Tree'),
    Key1 @< Key |
	tree_fetch;

    Tree = tree(Key, Data, _, _) :
      Value = Data |
	true;

    Tree = nil : Key = _,
      Value = none |
	true.


tree_to_list(Tree, List, Meta) + (Tail = []) :-

    Tree = nil : Meta = _,
      List = Tail |
	true;

    Tree = tree(Key, Value, Left, Tree') |
	tree_to_list(Left, List, Meta, [ {Meta, Key, Value} | List']),
	tree_to_list.


dummy_binary_tree(In) :-

    In ? add(_Key, _Value) |
	dummy_binary_tree;

    In ? fetch(_Key, none^) |
	dummy_binary_tree;

    In ? reset |
	dummy_binary_tree;

    In ? tree_to_list([]^) |
	dummy_binary_tree;

    In ? get_tree(nil^) |
	dummy_binary_tree;

    In ? dummy |
	dummy_binary_tree;

    In ? real_dummy : In' = _ |
	true;

    In = [] |
	true.
