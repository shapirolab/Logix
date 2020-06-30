/*
** This module is part of EFCP.
**

     Copyright 2007 Yossi Goldberg, William Silverman
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
       1999, 2010
*/

/* Requests */

#define SPI_POST            1
#define SPI_CLOSE           2
#define SPI_STEP            3
#define SPI_INDEX           4
#define SPI_RATE            5

/* Sub-Channel Indices */

#define SPI_BLOCKED	    1	// TRUE iff non-empty queue and
				// no transmission is possible
#define SPI_CHANNEL_TYPE    2	// see below
#define SPI_CHANNEL_RATE    3	// Real
#define SPI_CHANNEL_REFS    4	// Reference counter
#define SPI_SEND_ANCHOR     5	// Head of SendQueue
#define SPI_DIMER_ANCHOR    5	// Head of DimerQueue
#define SPI_SEND_WEIGHT     6	// Sum of Send Multipliers
#define SPI_DIMER_WEIGHT    6	// Sum of Dimer Multipliers
#define SPI_RECEIVE_ANCHOR  7	// Head of ReceiveQueue
#define SPI_RECEIVE_WEIGHT  8	// Sum of Receive Multipliers
#define SPI_WEIGHT_TUPLE    9	// (constant) Weight computation parameters
#define SPI_NEXT_CHANNEL   10	// (circular) Channel list
#define SPI_PREV_CHANNEL   11	// (circular) Channel list
#define SPI_CHANNEL_NAME   12	// (constant) Created Channel name

#define CHANNEL_SIZE       12

/* Channel Types */

#define SPI_CHANNEL_ANCHOR	 0
#define SPI_UNKNOWN		 1
#define SPI_BIMOLECULAR		 2
#define SPI_HOMODIMERIZED	 3
#define SPI_DELAY		 4
#define SPI_INSTANTANEOUS	 6
#define SPI_SINK		 7
#define SPI_UNKNOWN_PRIME	 9
#define SPI_BIMOLECULAR_PRIME	10
#define SPI_HOMODIMERIZED_PRIME	11
/*  DELAY_PRIME                 12 */

#define SPI_TYPE_MASK		 7
#define SPI_PRIME_FLAG		 8
#define SPI_PRIME_MASK		15
#define SPI_RANDOM_FLAG		16

/* Weight Index Computation Values */

#define SPI_DEFAULT_WEIGHT_INDEX 0
#define SPI_DEFAULT_WEIGHT_NAME "default"

/* Message Types */

#define SPI_MESSAGE_ANCHOR  0
#define SPI_SEND            1
#define SPI_RECEIVE         2
#define SPI_DIMER           3

/* Listed Operation tuple (1-5/6), Queued message tuple (1-9) */

#define SPI_MS_TYPE         1		/* One of Message Types */
#define SPI_MS_CID          2
#define SPI_MS_CHANNEL      3
#define SPI_MS_MULTIPLIER   4		/* Positive Integer */
#define SPI_MS_TAGS         5
#define SPI_MS_SIZE         5         /* if not biospi */
#define SPI_MS_AMBIENT      6         /* NIL or ambient channel */
#define SPI_AMBIENT_MS_SIZE 6         /* biospi */
#define SPI_SEND_TAG        5
#define SPI_RECEIVE_TAG     6
#define SPI_MS_COMMON       7		/* {PId, MsList, Value^, Chosen^} */
#define SPI_MESSAGE_LINKS   8		/* (circular) stored fcp 2-vector */
#define SPI_AMBIENT_CHANNEL 9         /* FCP channel or [] */
#define SPI_MESSAGE_SIZE    9

/* Links within SPI_MESSAGE_LINKS */

#define SPI_NEXT_MS         1
#define SPI_PREVIOUS_MS     2

/* Transmission common tuple (1-4) */

#define SPI_OP_PID          1
#define SPI_OP_MSLIST       2
#define SPI_OP_VALUE        3
#define SPI_OP_CHOSEN       4

#define SPI_COMMON_SIZE     4

#define QUEUE               2
#define BLOCKED             3

/* Index Request */

#define SPI_WEIGHTER_NAME   1
#define SPI_WEIGHTER_INDEX  2

/* Internal named values */

#define TUPLE               1
#define STRING              2 
#define LIST                3
heapP cdr_down_list();

/* Object definitions */

#define OBJECT_ARITY        2
#define OBJECT_VALUES       1
#define OBJECT_REQUESTS     2
