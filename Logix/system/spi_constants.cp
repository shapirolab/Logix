/*
** This module is part of EFCP.
**

     Copyright 2007 William Silverman
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
       SpiFcp Constant definitions
       1999
*/

-include(system_constants).

/* Strings */

EMPTY => EMPTY_STRING.
NULL => ANON_VAR_NAME.
DOT => GROUP_SEPARATOR.
RECEIVED_ARROW => "<-".
SENT_ARROW => "->".

/* Characters */

CHAR_EOL => 10.
CHAR_SPACE => 32.
CHAR_BANG => 33.
CHAR_DOUBLE_QUOTE => 34.
CHAR_HASH => 35.
CHAR_DOLLAR => 36.
CHAR_PRIME => 39.
CHAR_LEFT_PAREN => 40.
CHAR_RIGHT_PAREN => 41.
CHAR_ASTERISK => 42.
CHAR_PLUS => 43.
CHAR_MINUS => 45.
CHAR_DOT => 46.
CHAR_ZERO => 48.
CHAR_COLON => 58.
CHAR_LESS => 60.
CHAR_GREATER => 62.
CHAR_QUERY => 63.

CHAR_a => 97.
CHAR_b => 98.
CHAR_c => 99.
CHAR_d => 100.
CHAR_i => 105.
CHAR_p => 112.
CHAR_r => 114.
CHAR_s => 115.
CHAR_t => 116.
CHAR_z => 122.

CHAR_A => 65.
CHAR_Z => 90.

/* Action Codes for the Foreign Kernel */

SPI_POST =>            1.
SPI_CLOSE =>           2.
SPI_STEP =>            3.
SPI_INDEX =>           4.
SPI_RATE =>            5.

/* Sub-Channel Indices */

SPI_BLOCKED	     =>  1.	% TRUE iff non-empty queue and.
				% no transmission is possible
SPI_CHANNEL_TYPE     =>  2.	% see below.
SPI_CHANNEL_RATE     =>  3.	% Real.
SPI_CHANNEL_REFS     =>  4.	% Reference counter.
SPI_SEND_ANCHOR      =>  5.	% Head of SendQueue.
SPI_DIMER_ANCHOR     =>  5.	% Head of DimerQueue.
SPI_SEND_WEIGHT      =>  6.	% Sum of Send Multipliers.
SPI_DIMER_WEIGHT     =>  6.	% Sum of Dimer Multipliers.
SPI_RECEIVE_ANCHOR   =>  7.	% Head of ReceiveQueue.
SPI_RECEIVE_WEIGHT   =>  8.	% Sum of Receive Multipliers.
SPI_WEIGHT_TUPLE     =>  9.	% (constant) Weight computation parameters
SPI_NEXT_CHANNEL     => 10.	% (circular) Channel list.
SPI_PREVIOUS_CHANNEL => 11.	% (circular) Channel list.
SPI_CHANNEL_NAME     => 12.	% (constant) Created Channel name.

CHANNEL_SIZE => 12.

/* Channel Types */

SPI_CHANNEL_ANCHOR =>       0.
SPI_UNKNOWN =>              1. %
SPI_BIMOLECULAR =>          2. % baserate types
SPI_HOMODIMERIZED =>        3. %
SPI_DELAY =>                4. %
SPI_INSTANTANEOUS =>        6.
SPI_SINK =>                 7.
SPI_UNKNOWN_PRIME =>        9. %
SPI_BIMOLECULAR_PRIME =>   10. % custom computed rate type
SPI_HOMODIMERIZED_PRIME => 11. %
%   DELAY_PRIME =>         12. % nonesuch

SPI_TYPE_MASK =>            7.
SPI_PRIME_FLAG =>           8.
SPI_PRIME_MASK =>          15.
SPI_RANDOM_FLAG =>         16.

/* Weight Computation Values */

SPI_DEFAULT_WEIGHT_INDEX => 0.
SPI_DEFAULT_WEIGHT_NAME  => default.

/* Message Types */

SPI_MESSAGE_ANCHOR =>  0.
SPI_SEND =>            1.
SPI_RECEIVE =>         2.		/* Also used for delay */
SPI_DIMER =>           3.

/* Listed Operation tuple (1-5/6), Queued message tuple (1-9) */

SPI_MS_TYPE =>         1.		/* One of Message Types */
SPI_MS_CID =>          2.
SPI_MS_CHANNEL =>      3.
SPI_MS_MULTIPLIER =>   4.		/* Positive Integer */
SPI_MS_TAGS =>         5.		/* Integer or 2-tuple of Integers */
SPI_MS_SIZE =>         5.		/* For spifcp */
SPI_MS_AMBIENT =>      6.
SPI_AMBIENT_MS_SIZE => 6.		/* For biospi */
SPI_SEND_TAG =>        5.
SPI_DIMER_TAG =>       5.
SPI_RECEIVE_TAG =>     6.
SPI_COMMON =>          7.		/* {PId, MsList, Value^, Chosen^} */
SPI_MESSAGE_LINKS =>   8.		/* (circular) stored fcp 2-vector */
SPI_AMBIENT_CHANNEL => 9.

SPI_MESSAGE_SIZE =>    9.

SPI_NEXT_MS =>         1.
SPI_PREVIOUS_MS =>     2.

/* Transmission common tuple (1-4) */

SPI_OP_PID =>          1.
SPI_OP_MSLIST =>       2.
SPI_OP_VALUE =>        3.
SPI_OP_CHOSEN =>       4.

SPI_COMMON_SIZE =>     4.

/* Object Definitions */

OBJECT_ARITY => 2.
OBJECT_VALUES => 1.
OBJECT_REQUESTS => 2.
TIME_OBJECT_NAME => "SPITIME".
TIME_INITIAL_VALUE => 0.0.
TIME_LIMIT_VALUE => MAX_REAL.

/* Clause Tests */

EXCLUDE_CLAUSE_TESTS(Goal, Action) =>

  (
    Goal =?= spi_monitor#_ |
	Action;

    /* Obsolescent */
    Goal =?= global_channels(_,_) |
	Action;

    Goal =?= public_channels(_,_) |
	Action;

    Goal =?= spi_update_channel_refs(_,_,_) |
	Action
  )
.

MACRO_CLAUSE_TESTS(Goal, Action) =>

  (
    Goal =?= randomize_messages(_,_) |
	Action;

    Goal =?= serialize_messages(_,_) |
	Action;

    Goal =?= set_base_rate(_,_,_) |
	Action;

    Goal =?= get_channel_status(_,_,_) |
	Action
  )
.
