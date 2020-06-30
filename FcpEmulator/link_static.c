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

#include	<stdio.h>
#include	<string.h>
#include	"fcp.h"

/* Made by mkmk */
#include "link_static.h"

int
link_lookup(Name)
     char *Name;
{
  char *Cp;
  register int I;

  if ((Cp = ((char *) rindex(Name,'/'))) != NULL) {
    Name = Cp+1;
  }
#if	NUM_FUNCTIONS
  for (I = 0; I < NUM_FUNCTIONS; I++) {
    if (strcmp(Name, FuncName[I]) == 0) {
      break;
    }
  }
  return((I < NUM_FUNCTIONS) ? (I+1) : 0);
#else
  return(0);
#endif
}

int
link_execute(Index, Parm)
     int Index;
     int *Parm;
{
#if	NUM_FUNCTIONS
  if (Index <= NUM_FUNCTIONS) {
    if ((*Func[Index-1])(Parm)) {
      return(True);
    }
  }
#endif
  return(False);
}
