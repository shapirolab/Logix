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
**	timer.c - a device for generating time signals.
**
**	August 1988.
**
** See the manual page getitimer(2) and the file /usr/include/sys/time.h.
**
** {open, real, Stream^}
** {open, virtual, Stream^}
** {open, profiling, Stream^}
**
** {close, real}
** {close, virtual}
** {close, profiling}
**
** {get_timer, real, {{Int_Secs^, Int_Micros^}, {Val_Secs^, Val_Micros^}}}
** {get_timer, virtual, {{Int_Secs^, Int_Micros^}, {Val_Secs^, Val_Micros^}}}
** {get_timer, profiling, {{Int_Secs^, Int_Micros^}, {Val_Secs^, Val_Micros^}}}
**
** {set_timer, real, {{Int_Secs?, Int_Micros?}, {Val_Secs?, Val_Micros?}}}
** {set_timer, virtual, {{Int_Secs?, Int_Micros?}, {Val_Secs?, Val_Micros?}}}
** {set_timer, profiling, {{Int_Secs?, Int_Micros?}, {Val_Secs?, Val_Micros?}}}
**
** {read_timer, real, Out, Out1?}
** {read_timer, virtual, Out, Out1?}
** {read_timer, profiling, Out, Out1?}
**
*/

#define RealTimer	1
#define VirtualTimer	2
#define ProfilingTimer	3

#include <stdio.h>
#include <sys/ioctl.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include <sys/time.h>
#include <time.h>

#include "fcp.h"
#include "codes.h"
#include "global.h"
#include "macros.h"

static int RealOpen = False;
static int VirtualOpen = False;
static int ProfilingOpen = False;

int
timer(T)
     heapP T;
{
  register heapT V = *(T+1);
  register heapP P = T+1;

  deref(V, P);
  if (!IsStr(V)) {
    if (IsVar(V)) {
      sus_tbl_add(P);
    }
    return(False);
  }
  switch (Arity_of(*T)) {
  case 2:
    if (strcmp((char *) (P+2), "close") != 0) {
      return(False);
    }
    switch (timer_type(T+2)) {
    case RealTimer:
      if (reset_select_entry(SignalC, SIGALRM)) {
	RealOpen = False;
	return(True);
      }
      return(False);
    case VirtualTimer:
      if (reset_select_entry(SignalC, SIGVTALRM)) {
	VirtualOpen = False;
	return(True);
      }
      return(False);
    case ProfilingTimer:
      if (reset_select_entry(SignalC, SIGPROF)) {
	ProfilingOpen = False;
	return(True);
      }
      return(False);
    default:
      return(False);
    }
  case 3:
    switch (Str_Length(P)) {
    case 4:
      if (strcmp((char *) (P+2), "open") != 0) {
	return(False);
      }
      {
	register int TimerType = timer_type(T+2);
	
	if (TimerType == 0) {
	  return(False);
	}
	P = T+3;
	V = *P;
	deref(V, P);
	if (!IsWrt(V)) {
	  if (IsRo(V)) {
	    sus_tbl_add(P);
	  }
	  return(False);
	}
	switch (TimerType) {
	case RealTimer:
	  return(RealOpen =
		 set_select_entry(SignalC, SIGALRM, P, ((int (*)()) Null)));
	case VirtualTimer:
	  return(VirtualOpen =
		 set_select_entry(SignalC, SIGVTALRM, P, ((int (*)()) Null)));
	case ProfilingTimer:
	  return(ProfilingOpen =
		 set_select_entry(SignalC, SIGPROF, P, ((int (*)()) Null)));
	}
      }
    case 9:
      if (strcmp((char *) (P+2), "get_timer") == 0) {
	int  TimerType = timer_type(T+2);

	if (TimerType == 0) {
	  return(False);
	}
	{
	  heapP P0, P1, P2, P3;

	  P = T+3;
	  V = *P;
	  deref(V, P);
	  if (!IsTpl(V) || (V != Word(2, TplTag))) {
	    if (IsVar(V)) {
	      sus_tbl_add(P);
	    }
	    return(False);
	  }
	  if (!two_vars_tuple(P+1, &P0, &P1)) {
	    return(False);
	  }
	  if (!two_vars_tuple(P+2, &P2, &P3)) {
	    return(False);
	  }
	  {
	    
	    struct itimerval TimerVal;

	    switch (TimerType) {
	    case RealTimer:
	      if (!RealOpen) {
		err_tbl_add(MACHINE, ErNOTOPN);
		return(False);
	      }
	      if (getitimer(ITIMER_REAL, &TimerVal) != 0) {
		extern int errno;

		err_tbl_add(SYSTEM, errno);
		return(False);
	      }
	      break;
	    case VirtualTimer:
	      if (!VirtualOpen) {
		err_tbl_add(MACHINE, ErNOTOPN);
		return(False);
	      }
	      if (getitimer(ITIMER_VIRTUAL, &TimerVal) != 0) {
		extern int errno;

		err_tbl_add(SYSTEM, errno);
		return(False);
	      }
	      break;
	    case ProfilingTimer:
	      if (!ProfilingOpen) {
		err_tbl_add(MACHINE, ErNOTOPN);
		return(False);
	      }
	      if (getitimer(ITIMER_PROF, &TimerVal) != 0) {
		extern int errno;

		err_tbl_add(SYSTEM, errno);
		return(False);
	      }
	      break;
	    }
	    asgn(*P0, P0, Word(((int) TimerVal.it_interval.tv_sec), IntTag));
	    asgn(*P1, P1, Word(((int) TimerVal.it_interval.tv_usec), IntTag));
	    asgn(*P2, P2, Word(((int) TimerVal.it_value.tv_sec), IntTag));
	    asgn(*P3, P3, Word(((int) TimerVal.it_value.tv_usec), IntTag));
	    return(True);
	  }
	}
      }
      if (strcmp((char *) (P+2), "set_timer") != 0) {
	return(False);
      }
      {
	int  TimerType = timer_type(T+2);
	
	if (TimerType == 0) {
	  return(False);
	}
	{
	  heapT V0, V1, V2, V3;

	  P = T+3;
	  V = *P;
	  deref(V, P);
	  if (!IsTpl(V) || (V != Word(2, TplTag))) {
	    if (IsVar(V)) {
	      sus_tbl_add(P);
	    }
	    return(False);
	  }
	  if (!two_positive_ints_tuple(P+1, &V0, &V1)) {
	    return(False);
	  }
	  if (!two_positive_ints_tuple(P+2, &V2, &V3)) {
	    return(False);
	  }
	  {
	    struct itimerval TimerVal;

	    TimerVal.it_interval.tv_sec = (long) Int_Val(V0);
	    TimerVal.it_interval.tv_usec = (long) Int_Val(V1);
	    TimerVal.it_value.tv_sec = (long) Int_Val(V2);
	    TimerVal.it_value.tv_usec = (long) Int_Val(V3);
	    switch (TimerType) {
	    case RealTimer:
	      if (!RealOpen) {
		err_tbl_add(MACHINE, ErNOTOPN);
		return(False);
	      }
	      if (setitimer(ITIMER_REAL, &TimerVal, NULL) != 0) {
		extern int errno;

		err_tbl_add(SYSTEM, errno);
		return(False);
	      }
	      return(True);
	    case VirtualTimer:
	      if (!VirtualOpen) {
		err_tbl_add(MACHINE, ErNOTOPN);
		return(False);
	      }
	      if (setitimer(ITIMER_VIRTUAL, &TimerVal, NULL) != 0) {
		extern int errno;

		err_tbl_add(SYSTEM, errno);
		return(False);
	      }
	      return(True);
	    case ProfilingTimer:
	      if (!ProfilingOpen) {
		err_tbl_add(MACHINE, ErNOTOPN);
		return(False);
	      }
	      if (setitimer(ITIMER_PROF, &TimerVal, NULL) != 0) {
		extern int errno;

		err_tbl_add(SYSTEM, errno);
		return(False);
	      }
	      return(True);
	    }
	  }
	}
      }
    }
  case 4:
    if (strcmp((char *) (P+2), "read_timer") != 0) {
      return(False);
    }
    {
      int  TimerType = timer_type(T+2);
      
      if (TimerType == 0) {
	return(False);
      }
      P = T+3;
      V = *P;
      deref(V, P);
      if (!IsWrt(V)) {
	if (IsRo(V)) {
	  sus_tbl_add(P);
	}
	return(False);
      }
      {
	register int SignalCount;

	switch (TimerType) {
	case RealTimer:
	  SignalCount = read_device_count(SignalC, SIGALRM);
	  next_select_token(SignalC, SIGALRM);
	  break;
	case VirtualTimer:
	  SignalCount = read_device_count(SignalC, SIGVTALRM);
	  next_select_token(SignalC, SIGVTALRM);
	  break;
	case ProfilingTimer:
	  SignalCount = read_device_count(SignalC, SIGPROF);
	  next_select_token(SignalC, SIGPROF);
	  break;
	}
	asgn(V, P, Ref_Word(HP));
	*HP++ = Word(SignalCount, L_IntTag);
      }
      *HP++ = Ref_Word((T+4));
      return(True);
    }
  default:
    return(False);
  }
}

int
timer_type(Ptr)
     heapP Ptr;
{
  register heapP P = Ptr;
  register heapT V = *P;

  deref(V, P);
  if (!IsStr(V)) {
    if (IsVar(V)) {
      sus_tbl_add(P);
    }
    return(0);
  }
  switch (Str_Length(P)) {
  case 4:
    if (strcmp((char *) (P+2), "real") != 0) {
      return(0);
    }
    return(RealTimer);
  case 7:
    if (strcmp((char *) (P+2), "virtual") != 0) {
      return(0);
    }
    return(VirtualTimer);
  case 9:
    if (strcmp((char *) (P+2), "profiling") != 0) {
      return(0);
    }
    return(ProfilingTimer);
  default:
    return(0);
  }
}

int
two_vars_tuple(PTuple, PPVar0, PPVar1)
     heapP PTuple, *PPVar0, *PPVar1;
{
  register heapP P0 = PTuple, P1;
  register heapT V = *P0;

  deref(V, P0);
  if (!IsTpl(V) || (V != Word(2, TplTag))) {
    if (IsVar(V)) {
      sus_tbl_add(P0);
    }
    return(False);
  }
  P0 = P0+1;
  P1 = P0+1;
  V = *P0;
  deref(V, P0);
  if (!IsWrt(V)) {
    if (IsRo(V)) {
      sus_tbl_add(P0);
    }
    return(False);
  }
  V = *P1;
  deref(V, P1);
  if (!IsWrt(V)) {
    if (IsRo(V)) {
      sus_tbl_add(P1);
    }
    return(False);
  }
  *PPVar0 = P0;
  *PPVar1 = P1;
  return(True);
}

int
two_positive_ints_tuple(PTuple, PInt0, PInt1)
     heapP PTuple, PInt0, PInt1;
{
  register heapP P0 = PTuple, P1;
  register heapT V0 = *P0, V1;

  deref(V0, P0);
  if (!IsTpl(V0) || (V0 != Word(2, TplTag))) {
    if (IsVar(V0)) {
      sus_tbl_add(P0);
    }
    return(False);
  }
  P0 = P0+1;
  P1 = P0+1;
  V0 = *P0;
  deref(V0, P0);
  if (!IsInt(V0) || (V0 < 0)) {
    if (IsVar(V0)) {
      sus_tbl_add(P0);
    }
    return(False);
  }
  V1 = *P1;
  deref(V1, P1);
  if (!IsInt(V1) || (V1 < 0)) {
    if (IsVar(V1)) {
      sus_tbl_add(P1);
    }
    return(False);
  }
  *PInt0 = V0;
  *PInt1 = V1;
  return(True);
}
