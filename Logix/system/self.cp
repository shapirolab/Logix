/*

Primitives

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:02:57 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/system/self.cp,v $

Copyright (C) 1988, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-export([arity/2, arg/3,
	 length/2, string_hash/2, string_length/2, nth_char/3,
	 make_tuple/2, copy_skeleton/2, string_to_dlist/3, list_to_string/2,
	 tuple_to_dlist/3, list_to_tuple/2,
	 convert_to_integer/2, convert_to_real/2, convert_to_string/2,
	 convert_if_integer/2,
	 freeze/3, freeze/4, freeze/6, dfreeze/4, dfreeze/5, dfreeze/6, melt/3,
	 make_channel/2, write_channel/2, write_channel/3, close_channel/1,
	 make_vector/3, write_vector/3, write_vector/4,
	 store_vector/3, store_vector/4, read_vector/3, close_vector/2,
	 acos/2, asin/2, atan/2, cos/2, sin/2, tan/2,
	 exp/2, log/2, sqrt/2, pow/3,
	 random/1, srandom/1,
	 invalid_reason/2, make_invalid/2, execute/2
	]
).

-mode(interrupt).

