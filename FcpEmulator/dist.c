/*
** This module is part of EFCP.
**

   Copyright 2007 Avraham Houri
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


** 
**     Free Software Foundation, Inc.
**     51 Franklin Street, Fifth Floor
**     Boston, MA 02110-1301 USA

       contact: bill@wisdom.weizmann.ac.il
*/

#include	"fcp.h"
#include	"codes.h"
#include	"global.h"
#include	"macros.h"
#include	<stdio.h>

extern	FILE *DbgFile;

/*
  There are 3 types of variables:
  	(1) a local variable: the read-only variable should point back to the
		writable variable, if both exist.
	(2) a shared writable variable: may have a local read-only variable,
		but is not connected to the shared read-only counterpart.
		Points to its id.
	(3) a shared read-only variable: is not connected to the shared
		writable counterpart. Points to its id.

   A shared variable is indicatied by putting a special "hanger" in its
   suspension list. The hanger is nulled however its next pointer points
   to a vector which contains the shared variable information. The normal
   next pointer in a hanger points to another hanger, forming the suspended
   processes list.

   The shared variable information contains:
   	1. SVR marker (To be used at gc in order to identify the SVR as an SVR)
	2. Type pointer to the shared occurence of the variable.
   	3. The variable's id.

   Remark: In the case of a shared read-only variable there is a writable
   occurence which is held solely by the emulator.
*/

bind_ro(RO, Term)
     register heapP RO, Term;
{
  return(do_bind_ro(Ref_Word(RO), Ref_Word(Term)));
}

do_bind_ro(RO, Term)
     register heapT RO, Term;
{
  /* if RO is a shared read-only variable - RO is bound to Term */

  register heapP Pa;
  register heapT Va = RO;

  deref(Va, Pa);
  if (IsRo(Va)) {
    if (is_shared_ro(Pa)) {
      asgn(*Pa, Pa, Term);
      return(True);
    }
  }
  return(False);
}

make_shared(Type, Var, Id)
     heapP Type, Var, Id;

{
  return(do_make_shared(Ref_Word(Type), Ref_Word(Var), Ref_Word(Id)));
}

do_make_shared(Type, Var, Id)
     heapT Type, Var, Id;

{
  /* Change the local variable Var to be a shared var with id Id. The type of
     the shared var is Type, and Var must correspond to type. The read-only and
     writable occurrences of Var are disconnected, and the occurrence that is
     not made shared is invalidated. */

  register heapP Pa;
  register heapT Va = Type;
  register heapP Pb;
  register heapT Vb = Var;
  register heapP Pc;

  deref(Va, Pa);
  if (!IsStr(Va)) {
    if (IsVar(Va)) {
      sus_tbl_add(Pa);
    }
    return(False);
  }
  if (Str_Length(Pa) != 2) {
    return(False);
  }
  deref(Vb, Pb);
  if (strcmp((char *) (Pa+2), "we") == 0) {
    if (!IsWrt(Vb)) {
      return(False);
    }
  }
  else {
    if (strcmp((char *) (Pa+2), "ro") == 0) {
      if (!IsRo(Vb)) {
	return(False);
      }
    }
    else {
      return(False);
    }
  }
  if (is_shared(Pb)) {
    return(False);
  }
  if (IsWrt(Vb)) {
    if (!IsZeroed(Vb)) {
      Pa = Var_Val(Vb);
      asgn(*Pa, Pa, Ref_Word(HP));
      *HP++ = Word(0, InvldTag);
      *HP++ = Ref_Word(Constants[ExportedC]);
    }
    chng(Vb, Pb, Var_Word(HP, WrtTag));
    Pb = HP;
    *HP++ = Var_Word(Pb, RoTag);
    Pa = Pb;
  }
  else {
    Pa = wrt_of_ro(Pb);
    if (IsWrt(*Pa)) {
      chng(*Pa, Pa, Ref_Word(HP));
      *HP++ = Word(0, InvldTag);
      *HP++ = Ref_Word(Constants[ExportedC]);
    }
    asgn(*Pb, Pb, Var_Word(Pb, RoTag));
    Pa = Pb;
  }
  Pc = HP;
  *HP++ = L_Ref_Word((Pc+2));
  *HP++ = Ref_Word(Var_Val(*Pb));
  *HP++ = L_Ref_Word(Nil);
  *HP++ = Ref_Word((Pc+4));
  /* shared variable vector */
  *HP = Word(SVR_Arity, VctrTag);
  *Marker_SVR(HP) = Ref_Word(SVRMarker);
  *Type_SVR(HP) = Ref_Word(Pa);
  *Id_SVR(HP) = Id;
  HP += SVR_Size;
  *Pb = Var_Word(Pc, RoTag); /* no need to trail */
  return(True);
}

make_unshared(Var)
     heapP Var;
{
  return(do_make_shared(Ref_Word(Var)));
}

do_make_unshared(Var)
     heapT Var;
{
  /* Succeeds if X is a shared variable, and should make it non-shared (local)
     variable and wake its suspended processes */

  register heapP Pa;
  register heapT Va = Var;
  register heapP Pb;

  deref(Va, Pa);
  if (!is_shared(Pa)) {
    return(False);
  }
  if (IsWrt(Va)) {
    Pa = Var_Val(Va);
  }
  asgn(*Pa, Pa, *Pa); /* wake up */
  Pa = Var_Val(*Pa);
  while (IsList(*Pa)) {
    Pb = L_Ref_Val(*Ref_SR(Pa));
    if (IsVctr(*Ref_Val(*Next_SR(Pb)))) {
      /* points to the SVR */
      *Next_SR(Pb) = *Type_SVR(Ref_Val(*Next_SR(Pb)));
      fprintf(DbgFile, "Made Unshared - Process:\n");
      pr_print(CP);
      return(True);
    }
    Pa = Ref_Val(*Next_SR(Pa));
  }
}

