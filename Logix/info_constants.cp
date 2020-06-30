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
       Guard Predicate "info" definitions
       2012
*/

INFO_CPU_TIME     =>  1.	%  cpu milliseconds*
INFO_FREE_HEAP    =>  2.	%  free heap space
INFO_USED_HEAP    =>  3.	%  used heap space
INFO_CREATIONS    =>  4.	%  creations*
INFO_REDUCTIONS   =>  5.	%  reductions*
INFO_SUSPENSIONS  =>  6.	%  suspensions*
INFO_TERMINATIONS =>  7.	%  terminations*
INFO_ACTIVATIONS  =>  8.	%  activations*
INFO_COLLECTIONS  =>  9.	%  garbage collections*
INFO_PQ_LENGTH    => 10.	%  length of process queue
INFO_FREED_HEAP   => 11.	%  average freed heap
INFO_CHARGE_TIME  => 12.	%  charged seconds*
INFO_GC_TIME      => 13.	%  garbage collection milliseconds*
INFO_COPIED_HEAP  => 14.	%  average copied heap

INFO_ARITY        => 14. 
				%  * ~ cumulated since start