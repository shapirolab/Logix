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
**	kernels.c  -  kernel predicates.
*/

#include	<stdio.h>
extern	FILE *DbgFile, *OutFile;

#include	<errno.h>

#ifdef	ULTRIX_DL
/* for dynamic loading of foreign kernels, to be checked */
#undef	__mips
#undef	__pdp11
#define	__vax
#endif

#ifdef	SUNOS4d1d3
#include	<sys/unistd.h>
#include	<a.out.h>
#endif

#include	<signal.h>
#include	<sys/file.h>
#include	<sys/time.h>
#include	<time.h>

#include	"fcp.h"
#include	"codes.h"
#include	"global.h"
#include	"macros.h"

/*
**  CONVERT_TO_INTEGER  -- convert integer, real, or string to an integer.
**
**    Fcp Usage:  cnvrt_to_integer(X, I).
**
*/
do_cnv_to_integer(Arg)
     heapT Arg;
{
  register heapT Va = Arg;
  register heapP Pa;

  deref(Va, Pa);
  if (IsVar(Va)) {
    sus_tbl_add(Pa);
    return(False);
  }
  switch (Tag_of(Va)) {
  case IntTag:
    KOutA = Va;
    return(True);
  case RealTag:
    {
      register int Number = (int) real_val((Pa+1));

      if ((Number < MinInt) || (Number > MaxInt)) {
	return(False);
      }
      KOutA = Word(Number, IntTag);
      return(True);
    }
  case StrTag:
    {
      register int Count;
      int Number;
      char Char;

      Count = sscanf((char*) (Pa+2), "%d%c", &Number, &Char);
      if ((Count != 1) || (Number < MinInt) || (Number > MaxInt)) {
	return(False);
      }
      KOutA = Word(Number, IntTag);
      return(True);
    }
  default:
    return(False);
  }
}

/*
**  CONVERT_TO_REAL --  convert a integer, real, string to a real.
**
**      Fcp Usage: cnvrt_to_real(X,R)  X is integer, real or a string.
**
*/
do_cnv_to_real(Arg)
     heapT Arg;
{
  register heapT Va = Arg;
  register heapP Pa;

  deref(Va, Pa);
  if (IsVar(Va)) {
    sus_tbl_add(Pa);
    return(False);
  }
  switch (Tag_of(Va)) {
  case IntTag:
    KOutA = Ref_Word(HP);
    *HP++ = Word(0, RealTag);
    real_copy_val(((realT) Int_Val(Va)), HP);
    HP += realTbits/heapTbits;
    return(True);
  case RealTag:
    KOutA = Ref_Word(Pa);
    return(True);
  case StrTag:
    {
      register int Count;
      realT Number;
      char Char;

      Count = sscanf((char *)(Pa+2),"%lf%c", &Number, &Char);
      if (Count != 1) {
	return(False);
      }
      KOutA = Ref_Word(HP);
      *HP++ = Word(0, RealTag);
      real_copy_val(Number, HP);
      HP += realTbits/heapTbits;
      return(True);
    }
  default:
    return(False);
  }
}

/*
**  CONVERT_TO_STRING -- convert a integer, real, or string to a string.
**
**    Fcp Usage: cnvrt_to_string(X,S)  X is integer, real, or
**						 a string
**
*/
do_cnv_to_string(Arg)
     heapT Arg;
{
  register heapT Va = Arg;
  register heapP Pa;

  deref(Va, Pa);
  if (IsVar(Va)) {
    sus_tbl_add(Pa);
    return(False);
  }
  switch (Tag_of(Va)) {
  case IntTag:
    {
      register char *PChar = (char *) (HP+2);
      register int StrLen = 0;

      KOutA = Ref_Word(HP);
      sprintf(PChar, "%d", Int_Val(Va));
      while (*PChar++ != '\0') {
	StrLen++;
      }
      *HP++ = Str_Hdr1(CharType, 0);
      *HP = Str_Hdr2(hash((char *)(HP+1), StrLen), StrLen);
      HP += 1 + Str_Words((HP-1));
      while (PChar < (char *) HP) {
	*PChar++ = '\0';
      }
      return(True);
    }
  case RealTag:
    {
      register char *PChar = (char *) (HP+2);
      register int StrLen;
      double RVal;

      KOutA = Ref_Word(HP);
      RVal = real_val((Pa+1));
      {
	register char *P = PChar;

        PChar += sprintf(PChar, "%.14G", RVal);
	while (*P != '\0') {
	  if ((*P == 'E') || (*P == '.')) {
	    break;
	  }
	  P++;
	}
	/*
	  if the string does not include a decimal point or an exponent
	   add ".0" to the end
	*/
	if (*P == '\0') {
	  *PChar++ = '.';
	  *PChar++ = '0';
	}
      }

      StrLen = PChar - (char *) (HP+2);
      *HP++ = Str_Hdr1(CharType, 0);
      *HP = Str_Hdr2(hash((char *)(HP+1), StrLen), StrLen);
      HP += 1 + Str_Words((HP-1));
      while (PChar < (char *) HP) {
	*PChar++ = '\0';
      }
      return(True);
    }
  case StrTag:
    KOutA = Ref_Word(Pa);
    return(True);
  default:
    return(False);
  }
}      
     
/*
**  STRING_TO_DLIST  --  converts a string (first argument) into a difference
**			 list.
**
**	Fcp usage:	string_to_dlist(S,L,L1)
*/
do_string_to_dlist(Arg0, Arg2)
     heapT Arg0, Arg2;
{
  register heapT Va = Arg0;
  register heapP Pa;
  
  deref(Va, Pa);
  if (!IsStr(Va)) {
    if (IsVar(Va)) {
      sus_tbl_add(Pa);
    }
    return(False);
  }
  if (Str_Length(Pa) == 0) {
    KOutA = Arg2;
    return(True);
  }
  else {
    register int StrLen = Str_Length(Pa);

    if (heap_space(sizeof(heapT)) < StrLen) {
      err_tbl_add(MACHINE, ErHPSPACE);
      return(False);
    }
    {
      register char	*PChar = (char *) (Pa+2);

      KOutA = Ref_Word(HP);
      for (; StrLen > 0; StrLen--) {
	      *HP++ = Word(*PChar++, L_IntTag);
      }
      /*  append difference list L1  */
      *HP++ = Arg2;
      return(True);
    }
  }
}

/*
**  LIST_TO_STRING  --  convert a list (second argument) of numbers
**			to a string (third argument) where each number
**			occupies a byte, a half_word or words depending
**			on the value of the first argument, (0 = byte,
**			1 = half_word, 2 = words)
**
**	Fcp Usage: list_to_string(N,L,S)
*/
do_list_to_string(Arg0, Arg1)
     heapT Arg0, Arg1;
{
  register heapT Va = Arg0, Vb = Arg1;
  register heapP Pa, Pb;
  
  deref(Va, Pa);
  if (!IsInt(Va)) {
    if (IsVar(Va)) {
      sus_tbl_add(Pa);
    }
    return(False);
  }
  deref(Vb, Pb);
  if (!IsList(Vb)) {
    if (IsVar(Vb)) {
      sus_tbl_add(Pb);
    }
    return(False);
  }
  {
    register int Code = Int_Val(Va);
    register char *PChar = (char *) (HP + 2);
    register int Toggle = False;
    register heapP Trailer = Pb;

    do {
      Va = Off_List(Vb);
      deref(Va, Pa);
      if (IsVar(Va)) {
	sus_tbl_add(Pa);
	return(False);
      }
      if (ended_heap(PChar)) {
	err_tbl_add(MACHINE, ErHPSPACE);
	return(False);
      }
      switch (Code) {
      case 0 :
	if (!IsInt(Va)) {
	  return(False);
	}
	*PChar++ = (char) (Int_Val(Va) & MaxChar);
	break;
      case 1 :
	if (!IsInt(Va)) {
	  return(False);
	}
	*((short *) PChar) = (short) (Int_Val(Va) & MaxShort);
	PChar += shortbits/charbits;
	break;                                                                                                                                                                                                                                                                                                                                                                              
      case 2 :
	switch (Tag_of(Va)) {
	case IntTag:
	  *((int *) PChar) = Int_Val(Va);
	  PChar += intbits/charbits;
	  break;
	case RealTag:
	  real_copy((Pa+1), PChar);
	  PChar += realTbits/charbits;
	  break;
	case NilTag:
	  break;
	default:
	  return(False);
	}
	break;
      default:
	return(False);
      }
      if (Toggle) {
	Trailer = Cdr(Trailer);
	deref_ptr(Trailer);
	Toggle = False;
      }
      else {
	Toggle = True;
      }
      Pb = Cdr(Pb);
      Vb = *Pb;
      deref(Vb, Pb);
    }
    while (IsList(Vb) && (Pb != Trailer));
    if (!IsNil(Vb)) {
      if (IsVar(Vb)) {
	sus_tbl_add(Pb);
      }
      return(False);
    }
    {
      register int StrLen = (PChar - ((char *) (HP+2)));

      if ((StrLen > MaxStr)) {
	return(False);
      }
      KOutA = Ref_Word(HP);
      *HP++ = Str_Hdr1(CharType, 0);
      *HP = Str_Hdr2(hash((char *) (HP+1), StrLen), StrLen);
      HP++;
    }
    HP += Str_Words((HP-2));
    while (PChar < (char *) HP) {
      *PChar++ = '\0';
    }
    return(True);
  }
}

/*
**  MAKE_TUPLE  --  create a tuple on the heap of length N, (first
**		    argument).
**
**	Fcp Usage:	make_tuple(N,T)
*/
do_make_tuple(Arg)
     heapT Arg;
{
  register heapT Va = Arg;
  heapP	Pa;

  deref(Va, Pa);
  if (!IsInt(Va) || ((int) Va <= IntTag)) {
    if (IsVar(Va)) {
      sus_tbl_add(Pa);
    }
    return(False);
  }
  if (heap_space(sizeof(heapT)) < Int_Val(Va)) {
    err_tbl_add(MACHINE, ErHPSPACE);
    return(False);
  }
  KOutA = Ref_Word(HP);
  *HP++ = On_Tag(Off_Tag(Va), TplTag);
  {
    register int Count = Int_Val(Va);

    for ( ; Count > 0; Count--) {
      *HP++ = ZeroedWrt;
    }
  }
  return(True);
}

/*
 ** make_vector(N?, Ref^, Out^)
 */
do_make_vector(Arg)
     heapT Arg;
{
  register heapT Va = Arg;
  register heapP Pa, Pb;
  
  register int Arity;

  deref(Va, Pa);
  if (!IsInt(Va) || ((int) Va <= IntTag) || (Va > Word(MaxVctr, IntTag)) || 
      (Va > Word(MaxTpl, IntTag))) {
    if (IsVar(Va)) {
      sus_tbl_add(Pa);
    }
    return(False);
  }
  Arity = Int_Val(Va);
  if (heap_space((3*sizeof(heapT))) < Arity) {
    err_tbl_add(MACHINE, ErHPSPACE);
    return(False);
  }
  KOutA = Ref_Word(HP);
  *HP++ = Word(Arity, VctrTag);
  Pa = HP;
  HP += Arity;
  KOutB = Ref_Word(HP);
  *HP++ = Word(Arity, TplTag);
  Pb = HP;
  HP += Arity;
  while (Arity-- > 0) {
    *Pa++ = Ref_Word(HP);
    *HP = Var_Word(Pb, WrtTag);
    *Pb++ = Var_Word(HP, RoTag);
    HP++;
  }
  return(True);
}  

/*
 **	write_vector(N?, Element?, Ref?, Ref'^)
 */
heapP cdr_down_list();

do_write_vector(Arg0, Arg1, Arg2)
     heapT Arg0, Arg1, Arg2;
{
  register heapT Va = Arg0, Vb = Arg2;
  register heapP Pa, Pb;

  deref(Va, Pa);
  if (!IsInt(Va) || ((int) Va <= IntTag)) {
    if (IsVar(Va)) {
      sus_tbl_add(Pa);
    }
    return(False);
  }
  deref(Vb, Pa);
  if (!IsVctr(Vb) || (Off_Tag(Va) > Off_Tag(Vb))) {
    if (IsVar(Vb)) {
      sus_tbl_add(Pa);
    }
    return(False);
  }
  KOutA = Ref_Word(Pa);
  Pa = Pb = Pa + Int_Val(Va);
  deref_ptr(Pb);
  if (IsList(*Pb)) {
    Pb = cdr_down_list(Pb);
  }
  Va = *Pb;
  if (!IsWrt(Va)) {
    if (IsRo(Va)) {
      sus_tbl_add(Pb);
      return(False);
    }
    err_tbl_add(MACHINE, ErBADARG);
    return(False);
  }
  chng(*Pa, Pa, Ref_Word(HP)); /* destructive */
  *HP = Var_Word((HP+2), WrtTag);
  HP++;
  asgn(Va, Pb, Ref_Word(HP));
  *HP++ = Set_List(Arg1);
  *HP = Var_Word((HP-2), RoTag);
  HP++;
  return(True);
}

heapP
cdr_down_list(List)
     heapP List;
{
  register heapP Pa = List, Trailer = List;
  register int Toggle = False;

  /* cdr down the list, the two pointers method
     is used for the occur check (Silvermann Bill).
     */
  do {
    if (Toggle) {
      Trailer = Cdr(Trailer);
      deref_ptr(Trailer);
      Toggle = False;
    }
    else {
      Toggle = True;
    }
    Pa = Cdr(Pa);
    deref_ptr(Pa);
  }
  while (IsList(*Pa) && (Pa != Trailer));
  return(Pa);
}

/*
 **	store_vector(N?, Element?, Ref?, Ref'^)
 */
do_store_vector(Arg0, Arg1, Arg2)
     heapT Arg0, Arg1, Arg2;
{
  register heapT Va = Arg0, Vb = Arg2;
  register heapP Pa, Pb;

  deref(Va, Pa);
  if (!IsInt(Va) || ((int) Va <= IntTag)) {
    if (IsVar(Va)) {
      sus_tbl_add(Pa);
    }
    return(False);
  }
  deref(Vb, Pa);
  if (!IsVctr(Vb) || (Off_Tag(Va) > Off_Tag(Vb))) {
    if (IsVar(Vb)) {
      sus_tbl_add(Pa);
    }
    return(False);
  }
  KOutA = Ref_Word(Pa);
  Pa = Pb = Pa + Int_Val(Va);
  deref_ptr(Pb);
  if (IsList(*Pb)) {
    Pb = cdr_down_list(Pb);
  }
  Va = *Pb;
  if (!IsWrt(Va)) {
    if (IsRo(Va)) {
      sus_tbl_add(Pb);
      return(False);
    }
    err_tbl_add(MACHINE, ErBADARG);
    return(False);
  }
  chng(*Pa, Pa, Ref_Word(HP)); /* destructive */
  *HP++ = Set_List(Arg1);
  *HP = Var_Word((HP+2), WrtTag);
  HP++;
  asgn(Va, Pb, Ref_Word(HP));
  *HP++ = Set_List(Arg1);
  *HP = Var_Word((HP-2), RoTag);
  HP++;
  return(True);
}

/*
 **	read_vector(N?, Ref?, Element^)
 */
do_read_vector(Arg0, Arg1)
     heapT Arg0, Arg1;
{
  register heapT Va = Arg0, Vb = Arg1;
  register heapP Pa;

  deref(Va, Pa);
  if (!IsInt(Va) || ((int) Va <= IntTag)) {
    if (IsVar(Va)) {
      sus_tbl_add(Pa);
    }
    return(False);
  }
  deref(Vb, Pa);
  if (!IsVctr(Vb)) {
    if (IsVar(Vb)) {
      sus_tbl_add(Pa);
    }
    return(False);
  }
  if (Off_Tag(Va) > Off_Tag(Vb)) {
    err_tbl_add(MACHINE, ErBADARG);
    return(False);
  }
  Pa += Int_Val(Va);
  deref_ptr(Pa);
  if (!IsList(*Pa)) {
    err_tbl_add(MACHINE, ErBADARG);
    return(False);
  }
  {
    register heapP Trailer = Pa;
    register int Toggle = False;

    do {
      if (Toggle) {
      Trailer = Cdr(Trailer);
      deref_ptr(Trailer);
      Toggle = False;
      }
      else {
      Toggle = True;
      }
      Va = *Pa;
      Pa = Cdr(Pa);
      deref_ptr(Pa);
    }
    while (IsList(*Pa) && (Pa != Trailer));
  }
  if (IsList(*Pa)) {
    err_tbl_add(MACHINE, ErBADARG);
    return(False);
  }
  KOutA = Off_List(Va);
  return(True);
}

/*
 **	close_vector(N?, Ref?)
 */
do_close_vector(Arg0, Arg1)
     heapT Arg0, Arg1;
{
  register heapT Va = Arg0, Vb = Arg1;
  register heapP Pa;

  deref(Va, Pa);
  if (!IsInt(Va) || ((int) Va <= IntTag)) {
    if (IsVar(Va)) {
      sus_tbl_add(Pa);
    }
    return(False);
  }
  deref(Vb, Pa);
  if (!IsVctr(Vb)) {
    if (IsVar(Vb)) {
      sus_tbl_add(Pa);
    }
    return(False);
  }
  if (Off_Tag(Va) > Off_Tag(Vb)) {
    return(False);
  }
  Pa = Pa + Int_Val(Va);
  deref_ptr(Pa);
  if (IsList(*Pa)) {
    Pa = cdr_down_list(Pa);
  }
  if (!IsWrt(*Pa)) {
    return(True); /* stream is already closed */
  }
  asgn(*Pa, Pa, Word(0, NilTag));
  return(True);
}

/*  EXCEPTIONS -- Get the list of the exceptions which happened during
**  the process try.
**
**	KOutA is assigned to [] or to a list of binary tuples.
**
*/
do_exceptions()
{
  if (err_tbl_empty()) {
    KOutA = Word(0, NilTag);
    return(True);
  }
  if (ended_heap((HP + ((ERRP - ErrTbl)*5)))) {
    err_tbl_add(MACHINE, ErHPSPACE);
    return(False);
  }
  {
    register heapP SavedERRP = ERRP;

    KOutA =  Ref_Word(HP);
    while (!err_tbl_empty()) {
      *HP = L_Ref_Word((HP+2));
      HP++;
      *HP = Ref_Word((HP+4));
      HP++;
      *HP++ = Word(2, TplTag);
      switch ((int) *ERRP++) {
      case MACHINE:
	ERRP++;
	*HP++ = Ref_Word((Constants[MACHINE]));
	switch ((int) *ERRP) {
	case ErHPOVFL:
	case ErHPSPACE:
	case ErLINKSPACE:
	case ErUDFDCD:
	case ErINVLDOBJ:
	case ErBADARG:
	case ErEXITREQ:
	case ErNOTOPN:
	case ErNODEVSPC:
	case ErNOTFND:
	  *HP++ = Ref_Word((Constants[((int) *ERRP)]));
	  break;
	default:
	  *HP++ = Word(((int) *ERRP), IntTag);
	  break;
	}
      case SIGNAL :
	ERRP++;
	*HP++ = Ref_Word((Constants[SIGNAL]));
	{
	  register heapP PStr;

	  if ((((int) *ERRP) < 0) || (((int) *ERRP) >= NSIG)) {
	    PStr = produce_string("Unknown signal");
	  }
	  else {
#ifndef LINUX
#ifndef MACINTOSH
#ifndef CYGWIN
	    extern char *sys_siglist[];
#endif
#endif
#endif

	    PStr = produce_string("sys_siglist[((int) *ERRP)]");
	  }	    
	  *HP++ = Ref_Word(PStr);
	}
	break;
      case SYSTEM:
	ERRP++;
	*HP++ = Ref_Word((Constants[SYSTEM]));
	{
	  register heapP PStr;
	  char *EStr;
	  extern char *strerror(int);

	  EStr = (char *)strerror((int) *ERRP);
	  PStr = produce_string((EStr == NULL) ? "Unknown error" : EStr);
	  *HP++ = Ref_Word(PStr);
	}
	break;
      default:
	*HP++ = Word(((int) *ERRP), IntTag);
	ERRP++;
	*HP++ = Word(((int) *ERRP), IntTag);
	break;
      }
      ERRP++;
    }
    ERRP = SavedERRP;
  }
  *HP++ = Word(0, NilTag);
  return(True);
}
  
/*
**  INFO  --  get certain statistics out of the emulator.
**	      1st argument selects the info.
**
**	Fcp Usage:	info(ID,N)
*/
do_info(Arg)
     heapT Arg;
{
  register heapT Va = Arg;
  register heapP Pa;

  deref(Va, Pa);
  if (!IsInt(Va)) {
    if (IsVar(Va)) {
      sus_tbl_add(Pa);
    }
    return(False);
  }
  switch(Int_Val(Va)) {
  case  1: /* CpuTime */
    stop_time();
    start_time();
    KOutA = Word(((CpuTime-GCTime)/1000), IntTag);
    return(True);
  case  2: /* Free Heap Space */
    KOutA = Word(((int) heap_space(sizeof(heapT))), IntTag);
    return(True);
  case  3: /* Used Heap Space */
    KOutA = Word(((int) (HP - CurHeap)), IntTag);
    return(True);
  case  4: /* No. of Creations */
    KOutA = Word(Creations, IntTag);
    return(True);
  case  5: /* No. of Reductions */
    KOutA = Word(Reductions, IntTag);
    return(True);
  case  6: /* No. of Suspensions */
    KOutA = Word(Suspensions, IntTag);
    return(True);
  case  7: /* No. of Terminations */
    KOutA = Word(Terminations, IntTag);
    return(True);
  case  8: /* No. of Activations */
    KOutA = Word(Activations, IntTag);
    return(True);
  case  9: /* No. of Garbage Collections  */
    KOutA = Word(Collections, IntTag);
    return(True);
  case 10: /* Length of Process Queue */
    KOutA = Word(queue_length(), IntTag);
    return(True); 
  case 11: /* Freed Heap Average */
    KOutA = Word(FreedAverage, IntTag);
    return(True);
  case 12: /* Elapsed Time (Peter Gerstenhaber) */
    {
      struct timeval    Time_Val;
      struct timezone   Time_Zone;

      gettimeofday(&Time_Val, &Time_Zone);
      if (Time_Val.tv_usec >= 500000) {
        Time_Val.tv_sec++;
      }
      KOutA = Word(Time_Val.tv_sec, IntTag);
      return(True);
    }
  case 13: /* GCTime */
    KOutA = Word((GCTime/1000), IntTag);
    return(True);
  case 14: /* Copied Heap Average */
    KOutA = Word(CopiedAverage, IntTag);
    return(True);
  default:
    return(False);
  }
}

int
queue_length()
{
  register heapP Process = QF;
  register int   Length = 0;

  while (Process != Nil) {
    Length++;
    Process = Ref_Val(*(Next_PR(Process)));
  }
  return(Length);
}

/*
**  CODE_INFO  --  get information about code.
**
**	Fcp Usage:	code_info(Code, Info)
*/

do_code_info(Arg)
     heapT Arg;
{
  register heapT Va = Arg;
  register heapP Pa;

  deref(Va, Pa);
  if (IsVar(Va)) {
    sus_tbl_add(Pa);
    return(False);
  }
  switch (Tag_of(Va)) {
  case IntTag:
    if ((Va < 0) || (Int_Val(Va) >= (LP - LinkBase))) {
      return(False);
    }
    KOutA = Ref_Word(HP);
    *HP++ = Word(1, TplTag);
    *HP++ = Ref_Word(produce_string("native_code"));
    return(True);
  case StrTag:
    switch (Str_Type(Pa)) {
    case CtlType:
      {
	register heapP Info;
	heapP MdlName = Nil, PrcdrName = Nil;
	int Arity, Index;

	if (Str_Offset(Pa) != 0) {
	  register heapP Mdl = Pa - Str_Offset(Pa);
	  
	  if ((Info = Mdl_Info(Mdl)) != Nil) {
	    if ((MdlName = produce_string(Mdl_Name(Info))) == Nil) {
	      err_tbl_add(MACHINE, ErHPSPACE);
	      return(False);
	    }
	  }
	}
	if ((Info = Ctl_Info(Pa)) != Nil) {
	  if ((PrcdrName = produce_string(Ctl_Name(Info))) == Nil) {
	    err_tbl_add(MACHINE, ErHPSPACE);
	    return(False);
	  }
	  Arity = Ctl_Arity(Info);
	  Index = Ctl_Index(Info);
	}
	else {
	  Arity = 0;
	  Index = 0;
	}
	Info = produce_string("ctl");
	KOutA = Ref_Word(HP);
	*HP++ = Word(5, TplTag);
	*HP++ = Ref_Word(Info);
	*HP++ = Ref_Word(MdlName);
	*HP++ = Ref_Word(PrcdrName);
	*HP++ = Word(Arity, IntTag);
	*HP++ = Word(Index, IntTag);
      }
      return(True);
    case MdlType:
      {
	register heapP Info;
	heapP MdlName = Nil;

	if ((Info = Mdl_Info(Pa)) != Nil) {
	  if ((MdlName = produce_string(Mdl_Name(Info))) == Nil) {
	    err_tbl_add(MACHINE, ErHPSPACE);
	    return(False);
	  }
	}
	Info = produce_string("module");
	KOutA = Ref_Word(HP);
	*HP++ = Word(2, TplTag);
	*HP++ = Ref_Word(Info);
	*HP++ = Ref_Word(MdlName);
      }
      return(True);
    default:
      return(False);
    }
  default:
    return(False);
  }
}

#ifdef	SUNOS4d1d3


/* D. Peachey, Sask CSRL, 31-Jul-86 */
#ifdef PYR
#include <sys/mman.h>
#endif

char	*copy_str();

/*
**  LINK (adopted from Quintus Prolog) :
**
**	Fcp Usage:	link(ObjLibs?, Offset^).
**
**	ObjLibs is a list of names of object files or libraries to be
**	linked. The kernel generates a call to "ld" of the following form
**	(See the Unix documantation for more details):
**
**		ld -N -x -A ?? -T ?? -o FcpSymb objects libraries -lc
**
**	The ?? field after -A is the base program for load, which happens
**	to be the emulator symbol table. The ?? field after -T is the address
**	where the text, data and bss of the resulting linked file will be
**	loaded. This address must be a multiple of page size.
**	After the objects files are linked the text and data are loaded from
**	the linked file to the link area; A space in the size of the bss is
**	zeroed and Offset is assigned to the offset from the start of link area
**	to the start of the loaded text area.
*/
do_link(Arg)
     heapT Arg;
{
  register heapT Va = Arg;
  register heapP Pa;
  heapP ProcName = Null;
  linkP TextLoad = (linkP) align(LP, getpagesize());
  int Try_Static;

  deref(Va, Pa);
  if (IsList(Va)) {
      Try_Static = True;
  }
  else if(IsTpl(Va)) {
      Try_Static = False;
      Va = *(++Pa);
      deref(Va, Pa);
      if(!IsList(Va)) {
        if (IsVar(Va)) {
           sus_tbl_add(Pa);
        }
        return(False);
     }
  }else {
      if (IsVar(Va)) 
           sus_tbl_add(Pa);
      return(False);
  }

  {
    register char *PChar = (char *) HP;

    if (ended_heap(PChar)) {
      err_tbl_add(MACHINE, ErHPSPACE);
      return(False);
    }
    {
      extern char *ProgName;

      sprintf(PChar, "ld -N -x -A %s -T %x -o FcpSymb ", ProgName, TextLoad);
    }
    while (*PChar != '\0') {
      PChar++;
    }
    {
      register heapP Pb;
      register int Toggle = False;
      register heapP Trailer = Pa;

      Va = *Pa;
      do {
	Va = Off_List(Va);
	deref(Va, Pb);
	if (!IsNil(Va)) {
	  if ((!IsStr(Va)) || (Str_Type(Pb) != CharType)) {
	    if (IsVar(Va)) {
	      sus_tbl_add(Pb);
	    }
	    return(False);
	  }
	  if (ended_heap(PChar+Str_Length(Pb))) {
	    fprintf(DbgFile, "out of heap space\n");
	    err_tbl_add(MACHINE, ErHPSPACE);
	    return(False);
	  }
	  if (ProcName == Null) {
	    ProcName = Pb+2;
	  }
	  PChar = copy_str((char *) (Pb+2), PChar);
	  PChar = copy_str(" ", PChar);
	}
	if (Toggle) {
	  Trailer = Cdr(Trailer);
	  deref_ptr(Trailer);
	  Toggle = False;
	}
	else {
	  Toggle = True;
	}
	Pa = Cdr(Pa);
	Va = *Pa;
	deref(Va, Pa);
      }
      while (IsList(Va) && (Pa != Trailer));
    }
    if (!IsNil(Va)) {
      if (IsVar(Va)) {
	sus_tbl_add(Pa);
      }
      return(False);
    }
    PChar = copy_str("-lc", PChar);
    *PChar = '\0';
  }
  if(Try_Static == True) {
    int Index = link_lookup(ProcName);

    if (Index != 0) {
      KOutA = Word((-Index), IntTag);
#ifdef	DEBUG_LINK
      fprintf(DbgFile, "Static_Linked to %s via %d ...\n", ProcName, Index);
#endif
      return(True);
    }
  }
  if (TextLoad > LinkEnd) {
    fprintf(DbgFile, "out of space for linking\n");
    err_tbl_add(MACHINE, ErLINKSPACE);
    return(False);
  }
#ifdef	DEBUG_LINK
  fprintf(DbgFile, "%s\n", HP);
#endif
  {
    register int Code = system((char *) HP);

    fprintf(DbgFile, "system(%s) = %d\n", HP, Code);
    if (Code != 0) {
#ifndef LINUX
      extern char *sys_errlist[];
#endif
      
      fprintf(DbgFile, "error = %s\n", sys_errlist[errno]);
    }
    if (Code != 0) {
      err_tbl_add(SYSTEM, Code);
      return(False);
    }
  }
  {
    int Fd;

    if ((Fd = open("FcpSymb", O_RDONLY)) < 0 ) {
      fprintf(DbgFile, "error on opening of FcpSymb: errno = %d\n", errno);
      return(False);
    }
    {
      register int Count;
      struct exec Exec;

      Count = sizeof(struct exec);
      if (read(Fd, (char *) &Exec, Count) != Count) {
	fprintf(DbgFile, "error in read, count not = %d\n", Count);
	return(False);
      }
      Count = N_TXTOFF(Exec) - Count;
      if (lseek(Fd, Count, SEEK_CUR) < 0) {
	fprintf(DbgFile, "error in lseek\n");
	return(False);
      }
      Count = Exec.a_text + Exec.a_data + Exec.a_bss;
      if ((TextLoad + Count) >= LinkEnd) {
	err_tbl_add(MACHINE, ErLINKSPACE);
	fprintf(DbgFile, "CAN'T DO DYNAMIC LINK : OUT OF SPACE\n");
	return(False);
      }
      Count = Exec.a_text + Exec.a_data;/* D. Peachey */
      if (read(Fd, TextLoad, Count) != Count) {
	fprintf(DbgFile, "error in 2nd read, count != %d\n", Count);
	return(False);
      }
      /* D. Peachey */
      {
	register char *TmpPa, *TmpPb;
	
	TmpPa = TextLoad + Count;
	TmpPb = TmpPa + Exec.a_bss;
	while (TmpPa < TmpPb) {
	  *TmpPa++ = 0;
	}
      }
      Count += Exec.a_bss;
#ifdef PYR
      /* additional code for Pyramid only */
      /* enable execution on the pages containing the loaded code */
      mprotect(TextLoad, align(Count, getpagesize()),
	       (PROT_READ | PROT_WRITE | PROT_EXEC));
#endif
      /* D. Peachey, end of modification, 31-Jul-86 */
      LP = (linkP) align(TextLoad + Count, sizeof(int));
      unlink("FcpSymb");
#ifdef	DEBUG_LINK
      fprintf(DbgFile, "Dynamic linked to %s via %d ...\n", ProcName,
	     (TextLoad - LinkBase));
#endif
      KOutA = Word((TextLoad - LinkBase), IntTag);
      return(True);
    }
  }
}

char *
copy_str(PStr, PDest)
     char *PStr;
     char *PDest;
{
  register char	*PS = PStr;
  register char	*PD = PDest;

  while (*PS != '\0') {
    *PD++ = *PS++;
  }
  return(PD);
}

#else

/*
**	Fcp Usage:	link(ObjLibs?, Offset^).
*/

do_link(Arg)
     heapT Arg;
{
  register heapT Va = Arg;
  register heapP Pa;

  deref(Va, Pa);
  if(!IsList(Va)) {
    if (IsVar(Va)) {
      sus_tbl_add(Pa);
    }
    return(False);
  }

  Va = *Pa;
  Va = Off_List(Va);
  deref(Va, Pa);
  if ((!IsStr(Va)) || (Str_Type(Pa) != CharType)) {
    if (IsVar(Va)) {
      sus_tbl_add(Pa);
    }
    return(False);
  }

  {
    register int Index = link_lookup((Pa+StrHdrWords));

    if (Index != 0) {
      KOutA = Word((-Index), IntTag);
#ifdef	DEBUG_LINK
      fprintf(DbgFile, "Static_Linked to %s via %d ...\n", ProcName, Index);
#endif
      return(True);
    }
  }

  return(False);
}

#endif

/*
** Fcp Usage: priority(Old?, New?)
*/
do_priority(Arg0, Arg1)
     heapT Arg0, Arg1;
{
  register heapT Va = Arg0;
  register heapT Vb = Arg1;
  register heapP Pa, Pb;

  deref(Va, Pa);
  if (!IsNil(Va) && !IsStr(Va)) {
    if (IsVar(Va)) {
      sus_tbl_add(Pa);
    }
    return(False);
  }
  deref(Vb, Pb);
  if (!IsNil(Vb) && !IsStr(Vb)) {
    if (IsVar(Vb)) {
      sus_tbl_add(Pa);
    }
    return(False);
  }
  if (IsStr(Va)) {
    if (Str_Length(Pa) == 3) {
      if ((strcmp((char *) (Pa+2), "low") != 0) || (HighQ)) {
	err_tbl_add(MACHINE, ErBADARG);
	return(False);
      }
    }
    else {
      if (Str_Length(Pa) == 4) {
	if ((strcmp((char *) (Pa+2), "high") != 0) || (!HighQ)) {
	  err_tbl_add(MACHINE, ErBADARG);
	  return(False);
	}
      }
      else {
	err_tbl_add(MACHINE, ErBADARG);
	return(False);
      }
    }
  }
  if (IsStr(Vb)) {
    if (Str_Length(Pb) == 3) {
      if (strcmp((char *) (Pb+2), "low") != 0) {
	err_tbl_add(MACHINE, ErBADARG);
	return(False);
      }
      if (HighQ) {
	HighQ = False;
      }
    }
    else {
      if (Str_Length(Pb) == 4) {
	if (strcmp((char *) (Pb+2), "high") != 0) {
	  err_tbl_add(MACHINE, ErBADARG);
	  return(False);
	}
	if (!HighQ) {
	  HighQ = True;
	}
      }
      else {
	err_tbl_add(MACHINE, ErBADARG);
	return(False);
      }
    }
  }
  return(True);
}

/*
**  DEBUG  --  set/unset debug options. First arg is flag to set/unset
**		2nd arg is mode (0 = unset, nonzero = set)
**
**	Fcp  Usage:	debug(Type, Mode)
*/
do_debug(Arg0, Arg1)
     heapT Arg0, Arg1;
{
#ifdef	DEBUG
  register heapT Va = Arg0, Vb = Arg1;
  register heapP Pa;

  deref(Va, Pa);
  if (!IsInt(Va)) {
    if (IsVar(Va)) {
      sus_tbl_add(Pa);
    }
    return(False);
  }
  deref(Vb, Pa);
  if (!IsInt(Vb)) {
    if (IsVar(Vb)) {
      sus_tbl_add(Pa);
    }
    return(False);
  }
  switch (Int_Val(Va)) {
  case 1:
    Debug_Process = Int_Val(Vb);
    return(True);
  case 2:
    Debug_Clause = Int_Val(Vb);
    return(True);
  case 3:
    Debug_Guard = Int_Val(Vb);
    return(True);
  case 4:
    Debug_Outargs = Int_Val(Vb);
    return(True);
  case 5:
    Debug_Activation = Int_Val(Vb);
    return(True);
  case 6:
    Debug_Unset = Int_Val(Vb);
    return(True);
  default:
    return(False);
  }
#else
  return(True);
#endif
}

/*
**  TTYPUT_BYTE  --  Put the character given as an argument on the screen.
**
**	Fcp Usage:	ttyput_byte(C)
*/
do_ttyput_byte(Arg)
     heapT Arg;
{
  register heapT Va = Arg;
  register heapP Pa;

  deref(Va, Pa);
  if (IsVar(Va)) {
    sus_tbl_add(Pa);
    return(False);
  }
  if (!IsInt(Va)) {
    return(False);
  }
  putc(Int_Val(Va), OutFile);
  return(True);
}

/*
**  TTYPUT_INTEGER  --  Put the integer given as an argument on the screen.
**
**	Fcp Usage:	ttyput_integer(C)
*/
do_ttyput_integer(Arg)
     heapT Arg;
{
  register heapT Va = Arg;
  register heapP Pa;

  deref(Va, Pa);
  if (IsVar(Va)) {
    sus_tbl_add(Pa);
    return(False);
  }
  if (!IsInt(Va)) {
    return(False);
  }
  fprintf(OutFile, "%d", Int_Val(Va));
  return(True);
}

/*
**  TTYPUT_STRING  --  Put the string given as an argument on the screen.
**
**	Fcp Usage:	ttyput_string(C)
*/
do_ttyput_string(Arg)
     heapT Arg;
{
  register heapT Va = Arg;
  register heapP Pa;

  deref(Va, Pa);
  if (IsVar(Va)) {
    sus_tbl_add(Pa);
    return(False);
  }
  if (!IsStr(Va)) {
    return(False);
  }
  {
    register int StrLen = Str_Length(Pa);
    register char *PChar = (char *) (Pa+2);

    for (; StrLen > 0; StrLen--) {
      putc(*PChar++, OutFile);
    }
  }
  return(True);
}

/*
**  REQUEST -- add a request to the request stream.
**
**	Fcp Usage:	request(Request?, Answer^).
*/
do_request(Arg)
     heapT Arg;
{
  register heapT Va = Arg;
  register heapP Pa;

  deref(Va, Pa);
  if (!IsTpl(Va)) {
    if (IsVar(Va)) {
      sus_tbl_add(Pa);
    }
    return(False);
  }
  {
    register heapP NewListCell = HP;

    *HP = L_Ref_Word((HP+2));
    HP++;
    *HP++ = ZeroedWrt;
    *HP++ = Word(2, TplTag);
    *HP++ = Ref_Word(Pa);
    *HP = Var_Word((HP+1), WrtTag);
    HP++;
    KOutA = Ref_Word(HP);
    *HP = Var_Word((HP-1), RoTag);
    HP++;
    deref_ptr(McnInP);
    if (IsList(*McnInP)) {
      Pa = cdr_down_list(McnInP);
    }
    else {
      Pa = McnInP;
    }
    asgn(*Pa, Pa, Ref_Word(NewListCell));
  }
  return(True);
}
