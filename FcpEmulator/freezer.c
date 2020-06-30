/*
** This module is part of EFCP.
**

     Copyright 2007 Avshalom Houri, William Silverman
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
**	freezer.c  -  freeze/melt  a term. 
**
**      7/87
**
*/

#include	<stdio.h>
#include	<errno.h>

#include	"fcp.h"
#include	"codes.h"
#include	"global.h"
#include	"macros.h"

#define in_term(P, Base, End)	((P >= Base) && (P < End))

/*
**  FREEZE - freeze a term, producing a frozen term and a frozen variable
**           list.
**
**    Fcp Usage:  freeze(Term?, {Depth? ,Length?, StringLength?},
**			 FrozenTerm^, FrozenVarList^)
**
**	if Depth == [] -> no depth limit.
**	if Length == [] -> no length limit.
**	if StringLength == [] -> no string length limit.
**
*/
static int DepthF, LengthF, StringLengthF;
static int Depth, Length, StringLength;

do_freeze(Arg0, Arg1)
     heapT Arg0, Arg1;
{
  register heapT Va;
  register heapP Pa, Pb;

  Va = Arg1;
  deref(Va, Pa);
  if (IsNil(Va)) {
    DepthF = LengthF = StringLengthF = False;
    Depth = Length = StringLength = 0;
  }
  else {
    if (!IsTpl(Va) || (Va != Word(3, TplTag))) {
      if (IsVar(Va)) {
	sus_tbl_add(Pa);
      }
      return(False);
    }
    Va = *++Pa; /* Depth */
    deref(Va, Pb);
    if (IsNil(Va)) {
      DepthF = False;
      Depth = 0;
    }
    else {
      if (IsInt(Va) && (Va >= 0)) {
	DepthF = True;
	Depth = Int_Val(Va);
      }
      else {
	if (IsVar(Va)) {
	  sus_tbl_add(Pb);
	}
	return(False);
      }
    }
    Va = *++Pa; /* Length */
    deref(Va, Pb);
    if (IsNil(Va)) {
      LengthF = False;
      Length = 0;
    }
    else {
      if (IsInt(Va) && (Va >= 0)) {
	LengthF = True;
	Length = Int_Val(Va);
      }
      else {
	if (IsVar(Va)) {
	  sus_tbl_add(Pa);
	}
	return(False);
      }
    }
    Va = *++Pa; /* StringLength */
    deref(Va, Pb);
    if (IsNil(Va)) {
      StringLengthF = False;
      StringLength = 0;
    }
    else {
      if (IsInt(Va) && (Va >= 0)) {
	StringLengthF = True;
	StringLength = Int_Val(Va);
      }
      else {
	if (IsVar(Va)) {
	  sus_tbl_add(Pb);
	}
	return(False);
      }
    }
  }
  Va = Arg0;
  deref(Va, Pa);
  switch (Tag_of(Va)) {
  case IntTag:
    KOutA = Va;
    KOutB = Word(0, NilTag);
    return(True);
  case RealTag:
  case StrTag:
    KOutA = Ref_Word(Pa);
    KOutB = Word(0, NilTag);
    return(True);
  case NilTag:
    KOutA = Va;
    KOutB = Word(0, NilTag);
    return(True);
  }
  {
    heapP FrznTerm = HP, FrznVars;

    HP += 2;  /* String Header */
    *HP++ = Ref_Word(Pa);
    {
      trailP Old_ChngTRP = ChngTRP;
      
      if (!collect_frozen_term(FrznTerm+2)) {
	chng_trl_undo(Old_ChngTRP);
	return(False);
      }
      if (!complete_frozen_term((FrznTerm+2), &FrznVars)) {
	chng_trl_undo(Old_ChngTRP);
	return(False);
      }
      chng_trl_undo(Old_ChngTRP);
    }
    *FrznTerm = Str_Hdr1(FrznType, 0);
    {
      register int StrLen = sizeof(heapT)*((FrznVars-1)-(FrznTerm+2));
      
      *(FrznTerm+1) = Str_Hdr2(hash((char *)(FrznTerm+2),StrLen),StrLen);
    }
    KOutA = Ref_Word(FrznTerm);
    KOutB = Ref_Word(FrznVars);
  }
  return(True);
}

int
collect_frozen_term(Base)
  heapP Base;
{
  register heapP C_HP = Base;
  register heapP NotAddress = Nil, DLValid = Nil;
  register heapP LastCdr = Nil;
  int CurDepth, CurLength;
  heapP DLQ = ERRP - 1;
  register heapP DLF = DLQ, DLB = DLQ;

  while ((C_HP < HP) && !ended_heap(HP)) {
    tbls_overflow_check(STP, DLB);
    switch (Tag_of(*C_HP)) {
      case Tag(0x00, RefFlag):
      case Tag(0x01, RefFlag):
      case Tag(0x02, RefFlag):
      case Tag(0x03, RefFlag):
      case Tag(0x04, RefFlag):
      case Tag(0x05, RefFlag):
      case Tag(0x06, RefFlag):
      case Tag(0x07, RefFlag):
      case Tag(0x08, RefFlag):
      case Tag(0x09, RefFlag):
      case Tag(0x0a, RefFlag):
      case Tag(0x0b, RefFlag):
      case Tag(0x0c, RefFlag):
      case Tag(0x0d, RefFlag):
      case Tag(0x0e, RefFlag):
      case Tag(0x0f, RefFlag):
      if (!in_term(Ref_Val(*C_HP), Base, HP)) {
	break;
      }
      C_HP++;
      continue;
    case WrtTag:
    case RoTag:
    case IntTag:
      C_HP++;
      continue;
    case RealTag:
      C_HP++;
      C_HP += realTbits/heapTbits;
      continue;
    case StrTag:
      C_HP += 2 + Str_Words(C_HP);
      continue;
    case NilTag:
      C_HP++;
      continue;
    case Tag(0x00, L_RefFlag):
    case Tag(0x01, L_RefFlag):
    case Tag(0x02, L_RefFlag):
    case Tag(0x03, L_RefFlag):
    case Tag(0x04, L_RefFlag):
    case Tag(0x05, L_RefFlag):
    case Tag(0x06, L_RefFlag):
    case Tag(0x07, L_RefFlag):
    case Tag(0x08, L_RefFlag):
    case Tag(0x09, L_RefFlag):
    case Tag(0x0a, L_RefFlag):
    case Tag(0x0b, L_RefFlag):
    case Tag(0x0c, L_RefFlag):
    case Tag(0x0d, L_RefFlag):
    case Tag(0x0e, L_RefFlag):
    case Tag(0x0f, L_RefFlag):
      if (!in_term(L_Ref_Val(*C_HP), Base, HP)) {
	if (DepthF || LengthF) {
	  if (DepthF) {
	    CurDepth = (int) *DLF--;
	  }
	  if (LengthF) {
	    CurLength = (int) *DLF--;
	  }
	  LastCdr = DLValid = C_HP+1;
	}
	NotAddress = C_HP;
	break;
      }
    case L_IntTag: 
    case L_NilTag: 
      LastCdr = ++C_HP;
      continue;
    case TplTag:
      if (DepthF || LengthF) {
	if (DepthF) {
	  CurDepth = (int) *DLF--;
	}
	if (LengthF) {
	  CurLength = (int) *DLF--;
	}
	DLValid = C_HP + Arity_of(*C_HP);
      }
      C_HP++;
      continue;
    case VctrTag:
    case InvldTag:
    default:
      do_exit("freeze - collect:1", MACHINE, ErINVLDOBJ, False);
    }
    {
      register heapP P = C_HP;
      register heapT V = *P;

      if ((DepthF || LengthF) && (C_HP > DLValid)) {
	if (DepthF) {
	  CurDepth = 0;
	}
	if (LengthF) {
	  CurLength = 0;
	}
      }
      while ((P != Nil) && !ended_heap(HP)) {
	register heapP P0 = P;
	register heapT V0 = Off_List(*P0);
	
	trls_overflow_check(AsgnTRP, ChngTRP);
	tbls_overflow_check(STP, DLB);
	*P &= ListFlag;
	deref(V0, P0);
	if (!in_term(P0, Base, HP)) {
	  switch (Tag_of(V0)) {
	  case WrtTag:
	    chng_trl_add(V0, P0);
	    if (P > NotAddress) {
	      *P0 = Ref_Word(P);
	      *P = V0;
	    }
	    else {
	      *P |= *P0 = Ref_Word(HP);
	      P = HP;
	      *HP++ = V0;
	    }
	    if (IsZeroed(V0)) {
	      *P = Var_Word(P0, WrtTag);
	    }
	    else {
	      register heapP P1 = Var_Val(V0);
		
	      if (IsRef(*P1)) { /* ro copied */
		P1 = Ref_Val(*P1);
		*P = Var_Word(P1, WrtTag);
		*P1 = Var_Word(P0, RoTag);
	      }
	      else {
		chng_trl_add(*P1, P1);
		*P1 = Var_Word(P0, RoTag);
	      }
	    }
	    P = Nil;
	    continue;
	  case RoTag:
	    chng_trl_add(V0, P0);
	    if (P > NotAddress) {
	      *P0 = Ref_Word(P);
	      *P = V0;
	    }
	    else {
	      *P |= *P0 = Ref_Word(HP);
	      P = HP;
	      *HP++ = V0;
	    }
	    if (!IsRef(*Var_Val(V0))) {
	      *P = Var_Word(P0, RoTag);
	    }
	    P = Nil;
	    continue;
	  case IntTag:
	    *P |= V0;
	    P = Nil;
	    continue;
	  case RealTag:
	    chng_trl_add(V0, P0);
	    if ((P > NotAddress) && ((P+1) == HP)) {
	      HP--;
	    }
	    else {
	      *P |= Ref_Word(HP);
	    }
	    *HP = V0;
	    *P0++ = Ref_Word(HP++);
	    real_copy(P0, HP);
	    HP += realTbits/heapTbits;
	    P = Nil;
	    continue;
	  case StrTag:
	    if ((StringLengthF && (Str_Length(P0) > StringLength)) ||
		(Str_Offset(P0) != 0)) {
	      *P |= Ref_Word(P0);
	      P = Nil;
	      continue;
	    }
	    {
	      register int Count = 1 + Str_Words(P0);
	      
	      if (ended_heap((HP+Count))) {
		err_tbl_add(MACHINE, ErHPSPACE);
		return(False);
	      }
	      chng_trl_add(V0, P0);
	      if ((P > NotAddress) && ((P+1) == HP)) {
		HP--;
	      }
	      else {
		*P |= Ref_Word(HP);
	      }
	      *HP = V0;
	      *P0++ = Ref_Word(HP++);
	      for (; Count > 0; Count--) {
		*HP++ = *P0++;
	      }
	    }
	    P = Nil;
	    continue;
	  case NilTag:
	    *P |= V0;
	    P = Nil;
	    continue;
	  case Tag(0x00, L_RefFlag):
	  case Tag(0x01, L_RefFlag):
	  case Tag(0x02, L_RefFlag):
	  case Tag(0x03, L_RefFlag):
	  case Tag(0x04, L_RefFlag):
	  case Tag(0x05, L_RefFlag):
	  case Tag(0x06, L_RefFlag):
	  case Tag(0x07, L_RefFlag):
	  case Tag(0x08, L_RefFlag):
	  case Tag(0x09, L_RefFlag):
	  case Tag(0x0a, L_RefFlag):
	  case Tag(0x0b, L_RefFlag):
	  case Tag(0x0c, L_RefFlag):
	  case Tag(0x0d, L_RefFlag):
	  case Tag(0x0e, L_RefFlag):
	  case Tag(0x0f, L_RefFlag):
	  case L_IntTag:
	  case L_NilTag:
	    if (DepthF || LengthF) {
	      if (DepthF && (P != LastCdr)) {
		CurDepth++;
		if (CurDepth > Depth) {
		  *P |= Ref_Word(P0);
		  P = Nil;
		  continue;
		}
	      }
	      if (LengthF) {
		CurLength++;
		if (CurLength > Length) {
		  *P |= Ref_Word(P0);
		  P = Nil;
		  continue;
		}
	      }
	      if (Flag_of(V0) == L_RefFlag) {
		if (DLF == DLB) { /* empty queue */
		  DLF = DLB = DLQ; /* reset queue */
		}
		if (DepthF) {
		  *DLB-- = (heapT) CurDepth;
		}
		if (LengthF) {
		  *DLB-- = (heapT) CurLength;
		}
	      }
	    }
	    chng_trl_add(V0, P0);
	    if ((P > NotAddress) && ((P+1) == HP)) {
	      HP--;
	    }
	    else {
	      *P |= Ref_Word(HP);
  	    }
	    *HP++ = V0;
	    *P0 = Ref_Word((HP-1));
	    LastCdr = P = HP;
	    *HP++ = Ref_Word(Cdr(P0));
	    continue;
	  case TplTag:
	    if (DepthF || LengthF) {
	      if (DepthF) {
		CurDepth++;
		if (CurDepth > Depth) {
		  *P |= Ref_Word(P0);
		  P = Nil;
		  continue;
		}
	      }
	      if (LengthF) {
		CurLength++;
		if (CurLength > Length) {
		  *P |= Ref_Word(P0);
		  P = Nil;
		  continue;
		}
	      }
	      if (DLF == DLB) { /* empty queue */
		DLF = DLB = DLQ;
	      }
	      if (DepthF) {
		*DLB-- = (heapT) CurDepth;
	      }
	      if (LengthF) {
		*DLB-- = (heapT) CurLength;
	      }
	    }
	    chng_trl_add(V0, P0);
	    if ((P > NotAddress) && ((P+1) == HP)) {
	      HP--;
	    }
	    else {
	      *P |= Ref_Word(HP);
	    }
	    {
	      register int Count = Val_of(V0) - 1;
	      
	      if (ended_heap((HP+Count))) {
		err_tbl_add(MACHINE, ErHPSPACE);
		return(False);
	      }
	      *HP = V0;
	      *P0++ = Ref_Word(HP++);
	      for (; Count > 0; P0++, Count--) {
		CurLength++;
		switch (Tag_of(*P0)) {
		case Tag(0x00, RefFlag):
		case Tag(0x01, RefFlag):
		case Tag(0x02, RefFlag):
		case Tag(0x03, RefFlag):
		case Tag(0x04, RefFlag):
		case Tag(0x05, RefFlag):
		case Tag(0x06, RefFlag):
		case Tag(0x07, RefFlag):
		case Tag(0x08, RefFlag):
		case Tag(0x09, RefFlag):
		case Tag(0x0a, RefFlag):
		case Tag(0x0b, RefFlag):
		case Tag(0x0c, RefFlag):
		case Tag(0x0d, RefFlag):
		case Tag(0x0e, RefFlag):
		case Tag(0x0f, RefFlag):
		  *HP++ = *P0;
		  break;
		case WrtTag:
		  V0 = *P0;
		  chng_trl_add(V0, P0);
		  *P0 = Ref_Word(HP);
		  P = HP;
		  *HP++ = V0;
		  if (IsZeroed(V0)) {
		    *P = Var_Word(P0, WrtTag);
		  }
		  else {
		    register heapP P1 = Var_Val(V0);
		      
		    if (IsRef(*P1)) { /* ro copied */
		      P1 = Ref_Val(*P1);
		      *P = Var_Word(P1, WrtTag);
		      *P1 = Var_Word(P0, RoTag);
		    }
		    else {
		      chng_trl_add(*P1, P1);
		      *P1 = Var_Word(P0, RoTag);
		    }
		  }
		  break;
		case RoTag:
		  V0 = *P0;
		  chng_trl_add(V0, P0);
		  *P0 = Ref_Word(HP);
		  if (!IsRef(*Var_Val(V0))) {
		    *HP = Var_Word(P0, RoTag);
		  }
		  else {
		    *HP = V0;
		  }
		  HP++;
		  break;
		case IntTag:
		case NilTag:
		  *HP++ = *P0;
		  break;
		default:
		  do_exit("freeze - collect:2.0", MACHINE, ErINVLDOBJ, False);
		}
	      }
	    }
	    P = HP;
	    *HP++ = Ref_Word(P0);
	    continue;
	  case VctrTag:
	  case InvldTag:
	    *P |= Ref_Word(P0);
	    P = Nil;
	    continue;
	  default:
	    do_exit("freeze - collect:2.1", MACHINE, ErINVLDOBJ, False);
	  }
        }
	else {
	  *P |= Ref_Word(P0);
	  P = Nil;
	}
      }
      if (V == *C_HP) { /* excess term */
	C_HP++;
      }
    }
  }
  if (ended_heap(HP)) {
    err_tbl_add(MACHINE, ErHPSPACE);
    return(False);
  }
  return(True);
}

int
complete_frozen_term(Base, PFrznVars)
     heapP Base;
     heapP *PFrznVars;
{
  register heapP C_HP = Base;
  register heapP NotAddress = Nil;
  heapP FrznVarsStart = ERRP - 1;
  register heapP FVP = FrznVarsStart;

  while ((C_HP < HP) && !ended_heap(HP)) {
    trls_overflow_check(AsgnTRP, ChngTRP);
    tbls_overflow_check(STP, FVP);
    switch (Tag_of(*C_HP)) {
    case Tag(0x00, RefFlag):
    case Tag(0x01, RefFlag):
    case Tag(0x02, RefFlag):
    case Tag(0x03, RefFlag):
    case Tag(0x04, RefFlag):
    case Tag(0x05, RefFlag):
    case Tag(0x06, RefFlag):
    case Tag(0x07, RefFlag):
    case Tag(0x08, RefFlag):
    case Tag(0x09, RefFlag):
    case Tag(0x0a, RefFlag):
    case Tag(0x0b, RefFlag):
    case Tag(0x0c, RefFlag):
    case Tag(0x0d, RefFlag):
    case Tag(0x0e, RefFlag):
    case Tag(0x0f, RefFlag):
      if (!in_term(Ref_Val(*C_HP), Base, HP)) {
	break;
      }
      *C_HP = Ref_Word(((heapP) ((char *) Ref_Val(*C_HP) - (char *) C_HP)));
      C_HP++;
      continue;
    case WrtTag:
      {
	register heapP P = Var_Val(*C_HP);

	if (IsRef(*P)) {
	  if (Ref_Val(*P) == C_HP) {
	    *FVP-- = L_Ref_Word(P);
	    *C_HP++ = ZeroedWrt;
	    continue;
	  }
	  else {
	    P = Ref_Val(*P);
	  }
	}
	if (in_term(P, Base, HP)) {
	  if (P > C_HP) { /* ro after wrt */
	    *FVP-- = L_Ref_Word(Var_Val(*P));
	  }
	  *C_HP = Word(((heapP) ((char *) P - (char *) C_HP)), WrtTag);
	  C_HP++;
	}
	else {
	  *FVP-- = L_Ref_Word(Var_Val(*P));
	  *C_HP++ = Word(0, WrtTag);
	}
      }
      continue;
    case RoTag:
      {
	register heapP P = Var_Val(*C_HP);

	if (Ref_Val(*P) == C_HP) {
	  *FVP-- = L_Ref_Word(P); /* only ro or excess */
	  *HP = Var_Word(C_HP, WrtTag);
	  *C_HP = Word(((heapP) ((char *) HP - (char *) C_HP)), RoTag);
	  HP++;
	}
	else {
	  if (Ref_Val(*P) > C_HP) { /* wrt after ro */
	    *FVP-- = L_Ref_Word(P);
	  }
	  P = Ref_Val(*P);
	  *C_HP = Word(((heapP) ((char *) P - (char *) C_HP)), RoTag);
	}
      }
      C_HP++;
      continue;
    case IntTag:
      C_HP++;
      continue;
    case RealTag:
      C_HP++;
      C_HP += realTbits/heapTbits;
      continue;
    case StrTag:
      C_HP += 2 + Str_Words(C_HP);
      continue;
    case NilTag:
      C_HP++;
      continue;
    case Tag(0x00, L_RefFlag):
    case Tag(0x01, L_RefFlag):
    case Tag(0x02, L_RefFlag):
    case Tag(0x03, L_RefFlag):
    case Tag(0x04, L_RefFlag):
    case Tag(0x05, L_RefFlag):
    case Tag(0x06, L_RefFlag):
    case Tag(0x07, L_RefFlag):
    case Tag(0x08, L_RefFlag):
    case Tag(0x09, L_RefFlag):
    case Tag(0x0a, L_RefFlag):
    case Tag(0x0b, L_RefFlag):
    case Tag(0x0c, L_RefFlag):
    case Tag(0x0d, L_RefFlag):
    case Tag(0x0e, L_RefFlag):
    case Tag(0x0f, L_RefFlag):
      if (!in_term(L_Ref_Val(*C_HP), Base, HP)) {
	NotAddress = C_HP;
	break;
      }
      *C_HP++ = L_Ref_Word(((heapP) 
			    ((char *) L_Ref_Val(*C_HP) - (char *) C_HP)));
      continue;
    case L_IntTag: 
    case L_NilTag: 
      C_HP++;
      continue;
    case TplTag:
      C_HP++;
      continue;
    case VctrTag:
    case InvldTag:
    default:
      do_exit("freeze - complete:1", MACHINE, ErINVLDOBJ, False);
    }
    {
      register heapP P = C_HP;
      register heapP P0 = P;
      register heapT V0 = Off_List(*P0);
      
      *P &= ListFlag;
      deref(V0, P0);
      if (!in_term(P0, Base, HP)) {
	if ((IsStr(V0) && (Str_Offset(P0) != 0)) && 
	    (!(StringLengthF && (Str_Length(P0) > StringLength)))) {
	  register int Offset = Str_Offset(P0); /* > 0 */
	  register heapP MainStr = P0 - Offset;
	  
	  chng_trl_add(V0, P0);
	  if (IsRef(*MainStr)) { /* already copied */
	    *P |= *P0 = Ref_Word((Ref_Val(*MainStr)+Offset));
	  }
	  else {
	    register int Count = 1 + Str_Words(P0);
	    
	    if (ended_heap((HP+Count))) {
	      err_tbl_add(MACHINE, ErHPSPACE);
	      return(False);
	    }
	    if ((P > NotAddress) && ((P+1) == HP)) {
	      HP--;
	    }
	    else {
	      *P |= Ref_Word(HP);
	    }
	    *HP = Str_Hdr1(Str_Type(P0), 0); /* free it */
	    *P0++ = Ref_Word(HP++);
	    for (; Count > 0; Count--) {
	      *HP++ = *P0++;
	    }
	  }
	}
	else { /* excess term */
	  chng_trl_add(V0, P0);
	  if (P > NotAddress) {
	    *P0 = Ref_Word(P);
	    *P = Var_Word(P0, RoTag);
	  }
	  else {
	    *P |= *P0 = Ref_Word(HP);
	    *HP++ = Var_Word(P0, RoTag);
	  }
	}
      }
      else {
	*P |= Ref_Word(P0);
      }
    }
  }
  if (ended_heap(HP)) {
    err_tbl_add(MACHINE, ErHPSPACE);
    return(False);
  }
  *HP++ = 0; /* frozen term ended */
  if (ended_heap((HP + (FrznVarsStart - FVP)))) {
    err_tbl_add(MACHINE, ErHPSPACE);
    return(False);
  }
  *PFrznVars = HP;
  {
    register heapP P = FrznVarsStart;

    while (P != FVP) {
      *HP++ = *P--;
    }
  }
  *HP++ = Word(0, NilTag); /* frozen vars ended */
  return(True);
}

/*
**  MELT - melt a term.
**
**    Fcp Usage:  melt(FrozenTerm?, MeltedTerm^, MeltedVariableList^)
**
*/
do_melt(Arg)
     heapT Arg;
{
  register heapT Va = Arg;
  register heapP Pa;

  deref(Va, Pa);
  if (!IsStr(Va) || (Str_Type(Pa) != FrznType)) {
    switch (Tag_of(Va)) {
    case IntTag:
      KOutA = Va;
      KOutB = Word(0, NilTag);
      return(True);
    case RealTag:
    case StrTag:
      KOutA = Ref_Word(Pa);
      KOutB = Word(0, NilTag);
      return(True);
    case NilTag:
      KOutA = Va;
      KOutB = Word(0, NilTag);
      return(True);
    default:
      return(False);
    }
  }
  {
    heapP MltdTerm = HP, MltdVars;
    register heapP FTP;
    register heapP MTP;

    {
      register int Count = Str_Words(Pa) - 1; /* null word */
      register heapP Pb = Pa + 2;

      if (ended_heap((HP+Count))) {
	err_tbl_add(MACHINE, ErHPSPACE);
	return(False);
      }
      for (; Count > 0; Count--) {
	*HP++ = *Pb++;
      }
    }
    FTP = Pa + 2;
    MTP = MltdTerm;
    MltdVars = HP;
    while ((MTP < MltdVars) && !ended_heap(HP)) {
      switch (Tag_of(*MTP)) {
      case Tag(0x00, RefFlag):
      case Tag(0x01, RefFlag):
      case Tag(0x02, RefFlag):
      case Tag(0x03, RefFlag):
      case Tag(0x04, RefFlag):
      case Tag(0x05, RefFlag):
      case Tag(0x06, RefFlag):
      case Tag(0x07, RefFlag):
      case Tag(0x08, RefFlag):
      case Tag(0x09, RefFlag):
      case Tag(0x0a, RefFlag):
      case Tag(0x0b, RefFlag):
      case Tag(0x0c, RefFlag):
      case Tag(0x0d, RefFlag):
      case Tag(0x0e, RefFlag):
      case Tag(0x0f, RefFlag):
	{
	  register heapP Pb = ((heapP) ((char *) MTP + (int) Ref_Val(*MTP)));

	  if (!in_term(Pb, MltdTerm, MltdVars)) {
	    return(False);
	  }
	  *MTP++ = Ref_Word(Pb);
	}
	continue;
      case WrtTag:
	{
	  register heapP Pb = (heapP) ((char *) MTP + Int_Val(*MTP));

	  if (Pb == MTP) {
	    *HP++ = L_Ref_Word(MTP);
	    *MTP = ZeroedWrt;
	  }
	  else {
	    if (!in_term(Pb, MltdTerm, MltdVars) || !IsRo(*Pb)) {
	      return(False);
	    }
	    if (Pb > MTP) {
	      *HP++ = L_Ref_Word(MTP);
	    }
	    *MTP = Var_Word(Pb, WrtTag);
	  }
	}
	MTP++;
	continue;
      case RoTag:
	{
	  register heapP Pb = (heapP) ((char *) MTP + Int_Val(*MTP));

	  if (Pb == MTP) {
	    /* sole ro or excess term - directed vars */
	    *HP = L_Ref_Word((HP+2)); /* to new we */
	    HP++;
	    *HP = Ref_Word((HP+2)); /* cdr */
	    HP++;
	    *HP = Var_Word(MTP, WrtTag);
	    *MTP++ = Var_Word(HP, RoTag);
	    HP++;
	    continue;
	  }
	  if (!in_term(Pb, MltdTerm, MltdVars) || !IsWrt(*Pb)) {
	    return(False);
	  }
	  if (Pb > MTP) {
	    *HP++ = L_Ref_Word(Pb);
	  }
	  *MTP++ = Var_Word(Pb, RoTag);
	}
	continue;
      case IntTag:
	MTP++;
	continue;
      case RealTag:
	MTP += 1 + realTbits/heapTbits;
	continue;
      case StrTag:
	MTP += StrHdrWords + Str_Words(MTP);
	continue;
      case NilTag:
	MTP++;
	continue;
      case Tag(0x00, L_RefFlag):
      case Tag(0x01, L_RefFlag):
      case Tag(0x02, L_RefFlag):
      case Tag(0x03, L_RefFlag):
      case Tag(0x04, L_RefFlag):
      case Tag(0x05, L_RefFlag):
      case Tag(0x06, L_RefFlag):
      case Tag(0x07, L_RefFlag):
      case Tag(0x08, L_RefFlag):
      case Tag(0x09, L_RefFlag):
      case Tag(0x0a, L_RefFlag):
      case Tag(0x0b, L_RefFlag):
      case Tag(0x0c, L_RefFlag):
      case Tag(0x0d, L_RefFlag):
      case Tag(0x0e, L_RefFlag):
      case Tag(0x0f, L_RefFlag):
	{
	  register heapP Pb = ((heapP) ((char *) MTP + (int) L_Ref_Val(*MTP)));

	  if (!in_term(Pb, MltdTerm, MltdVars)) {
	    return(False);
	  }
	  *MTP++ = L_Ref_Word(Pb);
	}
	continue;
      case L_IntTag: 
      case L_NilTag: 
      case TplTag:
	MTP++;
	continue;
      case VctrTag:
      case InvldTag:
      default:
	return(False);
      }
    }
    if (ended_heap(HP)) {
      err_tbl_add(MACHINE, ErHPSPACE);
      return(False);
    }
    *HP++ = Word(0, NilTag);
    KOutA = Ref_Word(MltdTerm);
    KOutB = Ref_Word(MltdVars);
  }
  return(True);
}
