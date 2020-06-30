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


** 
**     Free Software Foundation, Inc.
**     51 Franklin Street, Fifth Floor
**     Boston, MA 02110-1301 USA

       contact: bill@wisdom.weizmann.ac.il
*/

/*
**	ctl.c - ctl modules creation and handling.
*/

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <sys/file.h>

#include "fcp.h"
#include "codes.h"
#include "global.h"
#include "macros.h"

#include "ctl.h"

#define check_if_tag(V, P, TagMacro) \
{ \
  if (!TagMacro) { \
    if (IsVar(V)) { \
      sus_tbl_add(P); \
    } \
    else { \
      err_tbl_add(MACHINE, ErBADARG); \
    } \
    return(False); \
  } \
}

#define toggle_trailer(Toggle, Trailer) \
{ \
  if (Toggle) { \
    Trailer = Cdr(Trailer); \
    deref_ptr(Trailer); \
    Toggle = False; \
  } \
  else { \
    Toggle = True; \
  } \
}

#define clear_skip_bytes(BPa, BPb) \
{ \
  while (BPa < BPb) { \
    *BPa++ = '\0'; \
  } \
}

int
ctl(T)
     heapP T;
{
  register heapT V = *(T+1);
  register heapP P = T+1;

  deref(V, P);
  check_if_tag(V, P, IsStr(V));
  switch (Arity_of(*T)) {
  case 2:
    if (strcmp((char *) (P+2), "ctl_alignment") != 0) {
      return(False);
    }
    return(False);
  case 3:
    if (strcmp((char *) (P+2), "make_ctl_module") != 0) {
      return(False);
    }
    return(make_ctl_module((T+2), (T+3)));
  default:
    return(False);
  }
}

make_ctl_module(Contents, BinaryModule)
     heapP Contents, BinaryModule;
{
  register heapP Pa, Pb;
  register heapT V;

  Pa = Contents;
  V = *Pa;
  deref(V, Pa);
  check_if_tag(V, Pa, IsList(V));
  Pb = BinaryModule;
  V = *Pb;
  deref(V, Pb);
  check_if_tag(V, Pb, IsWrt(V));
  asgn(V, Pb, Ref_Word(HP));
  {
    heapP MdlRef = HP, PrcdrRef = Nil;

    register heapP WordP = MdlRef;
    register opcodeP CodeP;
    register heapP Pc;
    register int Ia;
    codeU CU;

    int Toggle = False;
    heapP Trailer = Pa;

    WordP += MdlHdrWords;
    V = *Pa;
    do {
      if (ended_heap(WordP)) {
	err_tbl_add(MACHINE, ErHPSPACE);
	return(False);
      }
      V = Off_List(V);
      deref(V, Pb);
      switch (Tag_of(V)) {
      case WrtTag:
      case RoTag:
	sus_tbl_add(Pb);
	return(False);
      case StrTag:
	Ia = StrHdrWords + Str_Words(Pb);
	if (ended_heap((WordP+Ia))) {
	  err_tbl_add(MACHINE, ErHPSPACE);
	  return(False);
	}
	*WordP++ = Str_Hdr1(Str_Type(Pb++), (WordP - MdlRef));
	for (; Ia > 1; Ia--) {
	  *WordP++ = *Pb++;
	}
	break;
      case NilTag:
	break;
      case TplTag:
	Pc = Pb + 1;
	V = *Pc;
	deref(V, Pc);
	check_if_tag(V, Pc, IsInt(V));
	switch (Int_Val(V)) {
	case AnyEscape:
	  break;
	case InfoOffsetEscape:
	  /* Start of a procedure */
	  PrcdrRef = WordP;
	  WordP += CtlHdrWords;
	  CodeP = (opcodeP) WordP;
	  Pc = Pb + 2;
	  V = *Pc;
	  deref(V, Pc);
	  check_if_tag(V, Pc, IsInt(V));
	  /* Convert byte offset into word offset */
	  *(PrcdrRef + StrHdrWords) = Int_Val(V)/(heapTbits/charbits);
	  do {
	    toggle_trailer(Toggle, Trailer);
	    Pa = Cdr(Pa);
	    V = *Pa;
	    deref(V, Pa);
	    check_if_tag(V, Pa, IsList(V));
	    if (ended_heap(CodeP)) {
	      err_tbl_add(MACHINE, ErHPSPACE);
	      return(False);
	    }
	    V = Off_List(V);
	    deref(V, Pb);
	    switch (Tag_of(V)) {
	    case WrtTag:
	    case RoTag:
	      sus_tbl_add(Pb);
	      return(False);
	    case IntTag:
	      *CodeP++ = (opcodeT) (Int_Val(V) & MaxopcodeT);
	      break;
	    case NilTag:
	      break;
	    case TplTag:
	      Pc = Pb + 1;
	      V = *Pc;
	      deref(V, Pc);
	      check_if_tag(V, Pc, IsInt(V));
	      switch (Int_Val(V)) {
	      case AnyEscape:
		break;
	      case LabelOffsetEscape:
	      case StringOffsetEscape:
	      case ProceduralEscape:
	      case IterativeEscape:
		Pc = Pb + 2;
		V = *Pc;
		deref(V, Pc);
		check_if_tag(V, Pc, IsInt(V));
		CU.I = Int_Val(V);
		for (Ia = 0; Ia < (intbits/opcodeTbits); Ia++) {
		  *CodeP++ = CU.C[Ia];
		}
		break;
	      case IntegerWordValueEscape:
		Pc = Pb + 2;
		V = *Pc;
		deref(V, Pc);
		check_if_tag(V, Pc, IsInt(V));
		CU.H = V;
		for (Ia = 0; Ia < (heapTbits/opcodeTbits); Ia++) {
		  *CodeP++ = CU.C[Ia];
		}
		break;
	      case TupleWordArityEscape:
		Pc = Pb + 2;
		V = *Pc;
		deref(V, Pc);
		check_if_tag(V, Pc, IsInt(V));
		CU.H = On_Tag(Off_Tag(V), TplTag);
		for (Ia = 0; Ia < (heapTbits/opcodeTbits); Ia++) {
		  *CodeP++ = CU.C[Ia];
		}
		break;
	      case RegisterEscape:
		Pc = Pb + 2;
		V = *Pc;
		deref(V, Pc);
		check_if_tag(V, Pc, IsInt(V));
		*CodeP++ = ((char *) (X+Int_Val(V))) - ((char *) W);
		break;
	      case ProcessArgEscape:
		Pc = Pb + 2;
		V = *Pc;
		deref(V, Pc);
		check_if_tag(V, Pc, IsInt(V));
		*CodeP++ = (PR_Header+ Int_Val(V)) * (heapTbits/charbits);
		break;
	      case PrcdrInfoEscape:
		/* End of procedure */
		{
		  register opcodeP CodePa = CodeP;

		  to_word_boundry(CodeP);
		  clear_skip_bytes(CodePa, CodeP);
		}
		WordP = (heapP) CodeP;
		for (Ia = 0; Ia < 3; Ia++) {
		  Pc = Pb + 2 + Ia;
		  V = *Pc;
		  deref(V, Pc);
		  check_if_tag(V, Pc, IsInt(V));
		  if (Ia == 0) {
		    *WordP++ = Int_Val(V)/(heapTbits/charbits);
		  }
		  else {
		    *WordP++ = Int_Val(V);
		  }
		}
		Ia = (WordP - (PrcdrRef+StrHdrWords))*(heapTbits/charbits);
		if (Ia > MaxStr) {
		  err_tbl_add(MACHINE, ErBADARG);
		  return(False);
		}
		*WordP++ = (heapT) 0; /* null word at end of procedure */
		*PrcdrRef = Str_Hdr1(CtlType, (PrcdrRef-MdlRef));
		*(PrcdrRef+1) = Str_Hdr2(hash((char *) (PrcdrRef+StrHdrWords),
					      Ia), Ia);
		PrcdrRef = Nil;
		break;
	      case IndexedArgEscape:
		Pc = Pb + 2;
		V = *Pc;
		deref(V, Pc);
		check_if_tag(V, Pc, IsInt(V));
		*CodeP++ = (Int_Val(V) * (heapTbits/charbits)) & MaxopcodeT;
		break;
	      case RealValueEscape:
		Pc = Pb + 2;
		V = *Pc;
		deref(V, Pc);
		check_if_tag(V, Pc, IsReal(V));
		CU.D = real_val((Pc+1));
		for (Ia = 0; Ia < (realTbits/opcodeTbits); Ia++) {
		  *CodeP++ = CU.C[Ia];
		}
		break;
	      default:
		err_tbl_add(MACHINE, ErBADARG);
		return(False);
	      }
	      break;
	    default:
	      err_tbl_add(MACHINE, ErBADARG);
	      return(False);
	    }
	  }
	  while ((PrcdrRef != Nil)  && (Pa != Trailer));
	  break;
	case ModuleNameOffsetEscape:
	  Pc = Pb + 2;
	  V = *Pc;
	  deref(V, Pc);
	  check_if_tag(V, Pc, IsInt(V));
	  /* Convert byte offset into word offset */
	  *(MdlRef + StrHdrWords) = Int_Val(V)/(heapTbits/charbits);
	  break;
	default:
	  err_tbl_add(MACHINE, ErBADARG);
	  return(False);
	}
	break;
      default:
	err_tbl_add(MACHINE, ErBADARG);
	return(False);
      }
      toggle_trailer(Toggle, Trailer);
      Pa = Cdr(Pa);
      V = *Pa;
      deref(V, Pa);
    }
    while (IsList(V) && (Pa != Trailer));
    check_if_tag(V, Pa, IsNil(V));
    Ia = (WordP - (MdlRef+StrHdrWords))*(heapTbits/charbits);
    if (Ia > MaxStr) {
      err_tbl_add(MACHINE, ErBADARG);
      return(False);
    }
    *WordP++ = (heapT) 0; /* null word at end of moduel */
    HP = WordP; /* Update HP */
    *MdlRef = Str_Hdr1(MdlType, 0);
    *(MdlRef+1) = Str_Hdr2(hash((char *) (MdlRef+StrHdrWords), Ia), Ia);
  }
  return(True);
}
