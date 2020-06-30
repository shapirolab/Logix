/*
** This module is part of EFCP.
**

     Copyright 2007 Avshalom Houri, Ehud Shapiro
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
 **	streams.c  -  produce and handle streams.
 **
 */

#include	<stdio.h>
#ifdef	SUNOS5d3
#include	<sys/fcntl.h>
#endif
extern	FILE *DbgFile;

#include	<signal.h>
#include	<string.h>
#include	<errno.h>
#include	<sys/file.h>
#include	<sys/time.h>
#include	<time.h>

#include	"fcp.h"
#include	"codes.h"
#include	"global.h"
#include	"macros.h"

/* Machine Input  Routines */

handle_mcn_input()
{
  register heapP P = McnInP, Request;

  deref_ptr(P);		/* P = [{Req, Ans} | Rest] */
  P = L_Ref_Val(*P)+1;	/* P = Req */
  Request = Ref_Val(*P);
  P = Request+1;
  deref_ptr(P);
  if (!IsStr(*P) || (Str_Type(P) != CharType)) {
    answer_request(Constants[UnknownC]);
    return;
  }
  switch (Arity_of(*Request)) {
  case 1:
    switch (Str_Length(P)) {
    case 5:
      if (strcmp((char *) (P+2), "abort") == 0) {	/* {abort} */
	do_exit("", MACHINE, ErEXITREQ, False);
      }
      break;
    case 7:
      if (strcmp((char *) (P+2), "suspend") == 0) {	/* {suspend} */
	device_handlers(PauseC);
	kill(getpid(), SIGSTOP);
	answer_request(Constants[DoneC]);
	return;
      }
      else {
	if (strcmp((char *) (P+2), "gc_heap") == 0) {	/* {gc_heap} */
	  do_gc();
	  answer_request(Constants[DoneC]);
	  return;
	}
      }
      break;
    case 9:
      if (strcmp((char *) (P+2), "melt_heap") == 0) {	/* {melt_heap} */
	if (meltheap()) {
	  answer_request(Constants[DoneC]);
	}
	else {
	  answer_request(Constants[FalseC]);
	}	  
	return;
      }
      break;
    case 11:	/* {freeze_heap} */
      if (strcmp((char *) (P+2), "freeze_heap") == 0) {	/* {freeze_heap} */
	freezeheap();
	answer_request(Constants[DoneC]);
	return;
      }
      break;
    }
    break;
  case 2:
    switch (Str_Length(P)) {
    case 4:
      if (strcmp((char *) (P+2), "exit") == 0) {	/* exit(String) */
	P = Request + 2;
	deref_ptr(P);
	if (!IsInt(*P)) {
	  if (IsVar(*P)) {
	    answer_request(Constants[UnboundC]);
	    return;
	  }
	  break;
	}
	do_exit("", NORMAL, Int_Val(*P), False);
      }
      break;
    case 9:
      if (strcmp((char *) (P+2), "resolvent") == 0) {	/* {resolvent,
							   FileName} */
	P = Request + 2;
	deref_ptr(P);
	if (!IsStr(*P) || (Str_Type(P) != CharType)) {
	  if (IsVar(*P)) {
	    answer_request(Constants[UnboundC]);
	    return;
	  }
	  break;
	}
	{
	  register FILE *Fp, *TmpFp;
	  
	  Fp = fopen((char *) (P+2), "w");
	  if (Fp == NULL) {
	    answer_request(Constants[ErrorC]);
	    return;
	  }
	  TmpFp = DbgFile;
	  DbgFile = Fp;
	  prs_print();
	  fclose(Fp);
	  DbgFile = TmpFp;
	  answer_request(Constants[DoneC]);
	  return;
	}
      }
      break;
    case 10:
      if (strcmp((char *) (P+2), "save_state") == 0) {	/* {save_state,
							   FileName} */
	P = Request + 2;
	deref_ptr(P);
	if (!IsStr(*P) || (Str_Type(P) != CharType)) {
	  if (IsVar(*P)) {
	    answer_request(Constants[UnboundC]);
	    return;
	  }
	  break;
	}
	{
	  register int Fd;
	  
	  Fd = open((char *) (P+2), O_WRONLY | O_CREAT | O_TRUNC, 0666);
	  if (Fd < 0) {
	    answer_request(Constants[ErrorC]);
	    return;
	  }
	  do_gc();
	  if (CurHeap > OtherHeap) {  /* must be in lower heap for save */
	    do_gc();
	  }
	  {
	    int Size;
	    extern int RsrvSize, LinkSize, HeapSize, TrlsSize, TblsSize;

	    /* Save Sizes */
	    write(Fd, &RsrvSize, sizeof(RsrvSize)); /* Save RsrvSize */
	    write(Fd, &LinkSize, sizeof(LinkSize)); /* Save LinkSize */
	    write(Fd, &HeapSize, sizeof(HeapSize)); /* Save HeapSize */
	    write(Fd, &TrlsSize, sizeof(TrlsSize)); /* Save TrlsSize */
	    write(Fd, &TblsSize, sizeof(TblsSize)); /* Save TblsSize */
	    /* Save Link */
	    write(Fd, &LinkBase, sizeof(LinkBase)); /* Save LinkBase */
	    Size = (LP - LinkBase)*sizeof(linkT);
	    write(Fd, &Size, sizeof(Size)); /* Size of Saved Link */
	    if (write(Fd, LinkBase, Size) != Size) {  /* save the Link area */
	      close(Fd);
	      answer_request(Constants[ErrorC]);
	      return;
	    }
	    /* Save Heap */
	    write(Fd, &HeapBase, sizeof(HeapBase)); /* Save HeapBase */
	    write(Fd, &HeapStart, sizeof(HeapStart)); /* Save HeapStart */
	    write(Fd, &CurHeap, sizeof(CurHeap)); /* Save CurHeap */
	    P = HP;
	    deref_ptr(McnInP);
	    McnInP++;
	    save_vars(); /* Save vars on Heap */
	    McnInP--;
	    Size = (HP - HeapBase)*sizeof(heapT);
	    write(Fd, &Size, sizeof(Size)); /* Size of Saved Heap */
	    if (write(Fd, HeapBase, Size) != Size) {  /* Save Heap area */
	      HP = P;
	      close(Fd);
	      answer_request(Constants[ErrorC]);
	      return;
	    }
	  }
	  HP = P;
#ifdef	DLVS
	  dlvsSave(Fd);
#endif
	  close(Fd);
	  answer_request(Constants[DoneC]);
	  return;
	}
      }
      break;
    case 11:
      if (strcmp((char *) (P+2), "idle_output") == 0) {	/* {idle_output,
							   Mode} */
	P = Request + 2;
	deref_ptr(P);
	if (!IsInt(*P)) {
	  if (IsVar(*P)) {
	    answer_request(Constants[UnboundC]);
	    return;
	  }
	  break;
	}
	if (*P == Word(0, IntTag)) {
	  McnOutM &= ~(0x1 << IdleC);
	}
	else {
	  McnOutM |= 0x1 << IdleC;
	}
	answer_request(Constants[DoneC]);
	return;
      }
    }
    break;
  }
  answer_request(Constants[UnknownC]);
}

answer_request(Answer)
     heapP Answer;
{
  register heapP P = McnInP;

  deref_ptr(P);		/* P = [{Req, Ans} | Rest] */
  McnInP = Cdr(P);	/* McnInP = Rest */
  P = L_Ref_Val(*P)+2;	/* P = Ans */
  commited_asgn(P, Ref_Word(Answer));
}

/* Machine OutPut Routines */

mcn_output(Event)
     int Event;
{
  if ((Event < 0) || (Event > 31) || !((0x1 << Event) & McnOutM)) {
    return;
  }
  switch (Event) {
  case FailedC:
    {
      heapP Msg;
      
      Msg = HP;
      *HP++ = Word(2, TplTag);
      *HP++ = Ref_Word(Constants[FailedC]);
      *HP++ = Ref_Word(CP);
#ifdef	DEBUG
      fprintf(DbgFile, "Failing Process: ");
      pr_print(CP);
#endif
      if (IsInt(*Prcdr_PR(CP))) {
	*Next_PR(CP) = Word(0, NilTag);
      }
      else {
	register heapP Prcdr = Ref_Val(*Prcdr_PR(CP));
	
	if (Str_Offset(Prcdr) != 0) {
	  *Next_PR(CP) = Ref_Word((Prcdr - Str_Offset(Prcdr)));
	}
	else {
	  *Next_PR(CP) = Word(0, NilTag);
	}
      }
      *CP = On_Tag(Off_Tag((*CP)), TplTag);
      CP = Nil;
      McnOutP = new_list_element(McnOutP, Msg);
    }
    break;
  case GCC:
    {
      heapP Msg = HP;

      extern heapP LostProcesses;
      
      *HP++ = Word(2, TplTag);
      *HP++ = Ref_Word(Constants[GCC]);
      *HP++ = Ref_Word(LostProcesses);
      {
	register heapP P0 = LostProcesses;
	register heapP P1;
	
	while (P0 != Nil) {
	  P1 = L_Ref_Val(*Ref_SR(P0));
	  if (P1 != Nil) { 
#ifdef	DEBUG
	    fprintf(DbgFile, "Lost Process: ");
	    pr_print(P1);
#endif
	    if (IsInt(*Prcdr_PR(P1))) {
	      *Next_PR(P1) = Word(0, NilTag);
	    }
	    else {
	      register heapP Prcdr = Ref_Val(*Prcdr_PR(P1));
	      
	      if (Str_Offset(Prcdr) != 0) {
		*Next_PR(P1) = Ref_Word((Prcdr - Str_Offset(Prcdr)));
	      }
	      else {
		*Next_PR(P1) = Word(0, NilTag);
	      }
	    }
	    *P1 = On_Tag(Off_Tag((*P1)), TplTag);
	  }
	  P0 = Ref_Val(*Next_SR(P0));
	}
      }
      McnOutP = new_list_element(McnOutP, Msg);
    }
    break;
  case IdleC:
    McnOutP = new_list_element(McnOutP, Constants[IdleC]);
    break;
  case BootC:
    McnOutP = new_list_element(McnOutP, Constants[BootC]);
    break;
  }
  HB = HP;
}

