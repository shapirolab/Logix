/*
** This module is part of EFCP.
**

     Copyright 2007 Avshalom Houri
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
**  global.c  -  declaration and initialisation of global variables
**		 This is the one file which does NOT include global.h
**
*/

#include "fcp.h"
#include "codes.h"

/*
**  Registers
*/

linkP	LinkBase;		/* Pointer to base of Link */
linkP	LinkEnd;		/* Pointer to end of Link */

heapP	HeapBase;		/* Pointer to base of Heap */
heapP	HeapEnd;		/* Pointer to end of Heap */
heapP   HeapStart;		/* Pointer to start of unfrozen Heap */
heapP	CurHeap;		/* Pointer to current Heap */
heapP	CurHeapEnd;		/* Pointer to the end of current heap */
heapP   CurHeapLimit;		/* Pointer to CurHeapEnd - Heap_TH */
heapP	OtherHeap;		/* Pointer to the other heap */
heapP	OtherHeapEnd;		/* Pointer to the end of other heap */

workerT	Worker;
workerP	W = &Worker;			/* Pointer to the worker */

/*
**  Data Structures
*/

heapP Constants[ConstSize];		/* Constants Table */

