/*
** This module is part of EFCP.
**

     Copyright 2007 Avraham Houri, Ehud Shapiro
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
#include	"codes.h"
#include	"global.h"
#include	"macros.h"

/*
	This Procedure computes the hash value of a string addressed by
	Pstring of Charcount characters in length (>= 0).
*/

int hash(Pstring, Charcount)
     char	*Pstring;
     int	Charcount;
{
  register int	I, H;

  H = 0;
  for (I = 0; I < Charcount; I++) {
    H += *Pstring++;
  }
  return(H);
}

realT real_val(heapP P)
{
  realT Real;

  bcopy(((char *) P), ((char *) &Real), sizeof(realT)); 
  return(Real);
}

real_copy(heapP From, heapP To)
{
  bcopy(((char *) From), ((char *) To), sizeof(realT)); 
}

real_copy_val(realT Real, heapP P)
{
  bcopy(((char *) &Real), ((char *) P), sizeof(realT)); 
}

int make_invalid(Var, Reason)
     heapP Var, Reason;
{
  return(do_make_invalid(Ref_Word(Var), Ref_Word(Reason)));
}

int do_make_invalid(Var, Reason)
     heapT Var, Reason;
{
  register heapP Pa;
  register heapT Va = Var;

  deref(Va, Pa);
  if (!IsVar(Va)) {
    return(False);
  }
  if (IsRo(Va)) {
    register heapP Pb = wrt_of_ro(Pa);

    if (IsWrt(*Pb)) {
      Pa = Pb;
      Va = *Pa;
    }
  }
  asgn(Va, Pa, Ref_Word(HP));
  *HP++ = Word(0, InvldTag);
  *HP++ = Reason;
  return(True);
}

int invalid(Var, Reason)
     heapP Var, Reason;
{
  return(do_invalid(Ref_Word(Var), Ref_Word(Reason)));
}

int do_invalid(Var, Reason)
     heapT Var, Reason;
{
  /* Succeeds if Var is invalid. */
  register heapP Pa;
  register heapT Va = Var;

  Pa = Ref_Val(Va);
  deref(Va, Pa);
  if (!IsInvld(Va)) {
    if (IsVar(Va)) {
      sus_tbl_add(Pa);
    }
    return(False);
  }
  return(ask_unify(Ref_Word((Pa+1)), Reason));
}

int is_shared(PVar)
     heapP PVar;
{
  register heapP Pa = shared_info(PVar);

  if (Pa != Nil) {
    if (IsVar(*Ref_Val(*Type_SVR(Pa)))) {
      return(True);
    }
  }
  return(False);
}

int is_shared_we(PVar)
     heapP PVar;
{
  register heapP Pa = shared_info(PVar);

  if (Pa != Nil) {
    if (IsWrt(*Ref_Val(*Type_SVR(Pa)))) {
      return(True);
    }
  }
  return(False);
}

int is_shared_ro(PVar)
     heapP PVar;
{
  register heapP Pa = shared_info(PVar);

  if (Pa != Nil) {
    if (IsRo(*Ref_Val(*Type_SVR(Pa)))) {
      return(True);
    }
  }
  return(False);
}

heapP new_list_element(List_Tail, Element)
     heapP List_Tail, Element;
{
  register heapP P = List_Tail, Stream_Var = HP, List_Cell;

  *HP = Var_Word((HP+2), WrtTag);
  HP++;
  List_Cell = HP;
  *HP++ = L_Ref_Word(Element);
  *HP = Var_Word((HP-2), RoTag);
  HP++;
  commited_asgn(P, Ref_Word(List_Cell));
  return(Stream_Var);
}

int non_empty_sus_list(Ptr)
     heapP Ptr;
{
  register heapP P = Ptr;

  while (IsList(*P)) {
    if (L_Ref_Val(*(L_Ref_Val(*Ref_SR(P)))) != Nil) {
      /* a Process is Waiting */
      return(True);
    }
    P = Ref_Val(*Next_SR(P));
  }
  return(False);
}

heapP produce_string(String)
     char *String;
{
  register char *PStr = String, *PChar = (char *) (HP+2);
  heapP Saved_HP = HP;

  while ((*PStr != '\0') && (!ended_heap(PChar))) {
    *PChar++ = *PStr++;
  }
  if (ended_heap(PChar)) {
    return(Nil);
  }
  {
    register int StrLen = (PChar - ((char *) (HP+2)));

    *HP++ = Str_Hdr1(CharType, 0);
    *HP = Str_Hdr2(hash((char *) (HP+1), StrLen), StrLen);
    HP++;
  }
  HP += Str_Words((HP-2));
  while (PChar < (char *) HP) {
    *PChar++ = '\0';
  }
  return(Saved_HP);
}

heapP shared_info(PVar)
     heapP PVar;
{
  register heapP Pa = PVar, Pb;
  register heapT Va = *PVar;
  
  deref(Va, Pa);
  if (IsWrt(Va)) {
    if (!IsZeroed(Va)) {
      Pa = Var_Val(Va);
      Va = *Pa;
    }
  }
  if (IsRo(Va)) {
    if (IsList(*Var_Val(Va))) {
      Pa = Var_Val(Va);
      while (IsList(*Pa)) {
	Pb = L_Ref_Val(*Ref_SR(Pa));
	Pb = Ref_Val(*Next_SR(Pb));
	if (IsVctr(*Pb)) {
	  /* points to the SVR */
	  return(Pb);
	}
	Pa = Ref_Val(*Next_SR(Pa));
      }
    }
  }
  return(Nil);
}

int var_info(Var,Info)
     heapP Var, Info;
{
  return(do_var_info(Ref_Word(Var), Ref_Word(Info)));
}

do_var_info(Var,Info)
     heapT Var, Info;
{
  /* succeeds if Var is a variable and fails otherwise. If Var is local, then
     Info=?=local; if Var is shared with id Id, then Info=?=shared(Type,Id)
     where Type is either 'ro' or 'we', depending on the variable type. */

  register heapP Pa;
  register heapT Va = Var;
  register heapP Pb;

  deref(Va, Pa);
  if (!IsVar(Va)) {
    return(False);
  }
  if (!is_shared(Pa)) {
    return(ask_unify(Ref_Word(Constants[LocalC]), Info));
  }
  Pb = shared_info(Pa);
  Pa = HP;
  *HP++ = Word(3, TplTag);
  *HP++ = Ref_Word(Constants[SharedC]);
  if (IsWrt(Va)) {
    *HP++ = Ref_Word(Constants[WeC]);
  }
  else {
    *HP++ = Ref_Word(Constants[RoC]);
  }
  *HP++ = *Id_SVR(Pb);
  return(ask_unify(Ref_Word(Pa), Info));
}

int waiting_to(Ptr)
     heapP Ptr;
{
  register heapP P = Ptr;

  deref_ptr(P);
  if (IsWrt(*P)) {
    if (IsZeroed(*P)) {
      return(False);
    }
    else {
      P = Var_Val(*P);
    }
  }
  if (!IsRo(*P)) {
    return(False);
  }
  else {
    P = Var_Val(*P);
    return(non_empty_sus_list(P));
  }
}

heapP wrt_of_ro(Ptr)
     heapP Ptr;
{
  register heapP P = Var_Val(*Ptr);

  while (IsList(*P)) {
    P = Ref_Val(*Next_SR(P));
  }
  deref_ptr(P);
  return(P);
}

heap_ended(P) 
heapP P;
{
    if(P < CurHeapLimit)
	return(False);

    if( P >= CurHeapEnd)  
	do_exit("Allocated beyond the end of the heap!!", MACHINE,
		0, True); 
    return(True);
}
