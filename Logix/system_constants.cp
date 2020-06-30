/*
** This module is part of EFCP.
**

     Copyright 2012 William Silverman
     Weizmann Institute of Science, Rehovot, Israel

** EFCP is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
** 
** EFCP is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
** GNU General Public License for more details.
** 
** You should have received a copy of the GNU General Public License
** along with EFCP; if not, see:

       http://www.gnu.org/licenses

** or write to:

       Free Software Foundation, Inc.
       51 Franklin Street, Fifth Floor
       Boston, MA 02110-1301 USA

       contact: bill@wisdom.weizmann.ac.il

**
*/

/*
       System Constant definitions
       2012
*/

/* Strings */

EMPTY_STRING => "".			% smallest string (length = 0)

ANON_VAR_NAME => "_".			%

ANON_VARIABLE => `ANON_VAR_NAME.	%  not equal to any other variable

GROUP_SEPARATOR => ".".			% separates procedures and
					% components of <new_scope>	
/* Values */

BITS_PER_BYTE => 8.

MAX_PROCEDURE_ARGS => 252.		% 2**8 - 4

MAX_STRING_LENGTH => 16777215.		% 2**24 - 1

MAX_INTEGER => 33554431.		% 2**25 - 1

MAX_TUPLE_ARGS => MAX_INTEGER.

MAX_VECTOR_CHANNELS => MAX_INTEGER.

MAX_REAL => 1.79769313486E+308.		% largest double-real (64 bits)

/* Emulator replies */

FALSE => 0.

TRUE  => 1.
