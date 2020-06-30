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
 **	extern.c  -  errors handling, and reporting procedures.
 **
 */

#include	<stdio.h>
extern	FILE *DbgFile, *OutFile;

#include	<signal.h>
#include	<string.h>
#include	<errno.h>
#include	<sys/file.h>
#include	<sys/time.h>
#include	<time.h>
#include	<sys/resource.h>

#include	"fcp.h"
#include	"codes.h"
#include	"global.h"
#include	"macros.h"
#include	"opcodes.h"

/*#define GC_STATS*/

/*
** Reporting
*/

print_reason(Msg, Type, Value)
char *Msg;
int Type, Value;
{
  if (strcmp(Msg, "") != 0) {
    fprintf (DbgFile, "%s : ", Msg);
  }
  switch(Type) {
  case NORMAL:
    break;
  case MACHINE:
    fprintf(DbgFile, "%s : ", (char *) (Constants[Type]+2));
    switch(Value) {
    case ErHPOVFL:
    case ErUDFDCD:
    case ErINVLDOBJ:
    case ErEXITREQ:
      fprintf(DbgFile, "%s\n", (char *) (Constants[Value]+2));
      break;
    case ErTRLSOVFL:
      fprintf(DbgFile, "%s\n", (char *) (Constants[Value]+2));
      {
	extern int TrlsSize;

	fprintf(DbgFile, "(Current Size = %d). ", TrlsSize/sizeof(trailT));
	fprintf(DbgFile, "Process:\n");
	pr_print(CP);
      }
      break;
    case ErTBLSOVFL:
      fprintf(DbgFile, "%s\n", (char *) (Constants[Value]+2));
      {
	extern int TblsSize;

	fprintf(DbgFile, "(Current Size = %d). ", TblsSize/sizeof(heapT));
	fprintf(DbgFile, "Process:\n");
	pr_print(CP);
      }
      break;
    default:
      fprintf(DbgFile, "No. %d\n", Value);
      break;
    }
    break;
  case SIGNAL:
#ifndef CYGWIN
    if (DbgFile != stderr) {
#endif
      fprintf(DbgFile, "%s: Signal No. = %d\n", 
	      (char *) (Constants[Type]+2), Value);
#ifndef CYGWIN
    }
    psignal(Value, (char *) (Constants[Type]+2));
#endif
    break;
  case SYSTEM:
    if (DbgFile != stderr) {
      fprintf(DbgFile, "%s: Error No. = %d\n",
	      (char *) (Constants[Type]+2), Value);
    }
    {
      int tmp_errno = errno;

      errno = Value;
      perror(((char *) (Constants[Type]+2)));
      errno = tmp_errno;
    }
    break;
  default:
    fprintf(DbgFile, "Error : Type = %d, No. %d\n",Type, Value);
    break;
  }
}

print_core(Msg, Type, Value)
char *Msg;
int Type, Value;
{
  FILE *Fp, *TmpFp;
  
  Fp = fopen("core.logix", "w");
  if (Fp == NULL) {
    fprintf(DbgFile, "Can not open 'core.logix'\n");
  }
  else {
    fprintf(DbgFile, "Dump is written to ./'core.logix'\n");
    TmpFp = DbgFile;
    DbgFile = Fp;
    print_reason(Msg, Type, Value);
  }
  prs_print();
  print_stats();
  if (Fp != NULL) {
    fclose(Fp);
    DbgFile = TmpFp;
  }
}

#ifdef	DEBUG
int
print_guard_kernel(Value)
     int Value;
{
  switch(Value) {
  case equals:			/* "=", 2 */
    fprintf(DbgFile, "=");
    return(2);
  case is_var:			/* "var", 1 */
    fprintf(DbgFile, "var");
    return(1);
  case is_ro:			/* "ro", 1 */
    fprintf(DbgFile, "ro");
    return(1);
  case is_nonvar:	 	/* "nonvar", 1 */
    fprintf(DbgFile, "nonvar");
    return(1);
  case is_known:	  	/* "known", 1 */
    fprintf(DbgFile, "known");
    return(1);
  case is_integer:	  	/* "integer", 1 */
    fprintf(DbgFile, "integer");
    return(1);
  case is_string:	  	/* "string",1 */
    fprintf(DbgFile, "string");
    return(1);
  case is_tuple:	  	/* "tuple", 1 */
    fprintf(DbgFile, "tuple");
    return(1);
  case is_list:			/* "list", 1 */
    fprintf(DbgFile, "list");
    return(1);
  case is_module:		/* "module", 1 */
    fprintf(DbgFile, "module");
    return(1);
  case identical:		/* "==", 2 */
    fprintf(DbgFile, "==");
    return(2);
  case wait_equals:		/* "=?=", 2 */
    fprintf(DbgFile, "=?=");
    return(2);
  case is_vector:		/* "vector", 1 */
    fprintf(DbgFile, "vector");
    return(1);
  case not_identical:		/* "\=", 2 */
    fprintf(DbgFile, "\\=");
    return(2);
  case wait_not_equals:		/* "=\=", 2 */
    fprintf(DbgFile, "=\\=");
    return(2);
  case is_less:			/* "@<", 2 */
    fprintf(DbgFile, "@<");
    return(2);
  case is_constant:		/* "constant", 1 */
    fprintf(DbgFile, "constant");
    return(1);
  case is_compound:		/* "compound", 1 */
    fprintf(DbgFile, "compound");
    return(1);
  case lt:			/* "<", 2 */
    fprintf(DbgFile, "<");
    return(2);
  case le:			/* "=<", 2 */
    fprintf(DbgFile, "=<");
    return(2);
  case plus:			/* "plus", 3 */
    fprintf(DbgFile, "plus");
    return(3);
  case times:			/* "times", 3 */
    fprintf(DbgFile, "times");
    return(3);
  case div:			/* "div", 3 */
    fprintf(DbgFile, "div");
    return(3);
  case mod:			/* "mod", 3 */
    fprintf(DbgFile, "mod");
    return(3);
  case diff:			/* "diff", 3 */
    fprintf(DbgFile, "diff");
    return(3);
  case is_real:			/* "real", 1 */
    fprintf(DbgFile, "real");
    return(1);
  case is_number:		/* "number", 1 */
    fprintf(DbgFile, "number");
    return(1);
  case priority:		/* "priority", 2 */
    fprintf(DbgFile, "priority");
    return(2);
  case bitwise_and:		/* "bitwise_and", 3 */
    fprintf(DbgFile, "bitwise_and");
    return(3);
  case bitwise_or:		/* "bitwise_or", 3 */
    fprintf(DbgFile, "bitwise_or");
    return(3);
  case bitwise_not:		/* "bitwise_not", 2 */
    fprintf(DbgFile, "bitwise_not");
    return(2);
  case freeze:			/* "freeze", 4 */
    fprintf(DbgFile, "freeze");
    return(4);
  case melt:			/* "melt", 3 */
    fprintf(DbgFile, "melt");
    return(3);
  case invalid_g:		/* "invalid", 2 */
    fprintf(DbgFile, "invalid");
    return(2);
  case var_info_g:		/* "var_info", 2 */
    fprintf(DbgFile, "var_info");
    return(2);
  case make_unshared_g:		/* "make_unshared", 2 */
    fprintf(DbgFile, "make_unshared");
    return(1);
  case bind_ro_g:		/* "bind_ro", 2 */
    fprintf(DbgFile, "bind_ro");
    return(2);
  case arity:			/* "arity", 2 */
    fprintf(DbgFile, "arity");
    return(2);
  case arg:			/* "arg", 3 */
    fprintf(DbgFile, "arg");
    return(3);
  case string_length:		/* "string_length", 2 */
    fprintf(DbgFile, "string_length");
    return(2);
  case ask_unknown:		/* "ask_unknown", 1 */
    fprintf(DbgFile, "ask_unknown");
    return(1);
  case string_hash:		/* "string_hash", 2 */
    fprintf(DbgFile, "string_hash");
    return(2);
  case make_invalid_g:		/* "make_invalid", 2 */
    fprintf(DbgFile, "make_invalid");
    return(2);
  case make_tuple:		/* "make_tuple", 2 */
    fprintf(DbgFile, "make_tuple");
    return(2);
  case list_to_string:		/* "list_to_string", 3 */
    fprintf(DbgFile, "list_to_string");
    return(3);
  case string_to_dlist:		/* "string_to_dlist", 3 */
    fprintf(DbgFile, "string_to_dlist");
    return(3);
  case cnv_to_integer:		/* "cnvrt_to_integer", 2 */
    fprintf(DbgFile, "cnvrt_to_integer");
    return(2);
  case cnv_to_real:		/* "cnvrt_to_real", 2 */
    fprintf(DbgFile, "cnvrt_to_real");
    return(2);
  case cnv_to_string:		/* "cnvrt_to_string", 2 */
    fprintf(DbgFile, "cnvrt_to_string");
    return(2);
  case exceptions:		/* "exceptions", 1 */
    fprintf(DbgFile, "exceptions");
    return(1);
  case otherwise:		/* "otherwise", 0 */
    fprintf(DbgFile, "otherwise");
    return(0);
  case activate:		/* "activate", 3 */
    fprintf(DbgFile, "activate");
    return(3);
  case deschedule:		/* "deschedule", 0 */
    fprintf(DbgFile, "deschedule");
    return(0);
  case ttyput_byte:		/* "ttyput_byte", 1 */
    fprintf(DbgFile, "ttyput_byte");
    return(1);
  case ttyput_string:		/* "ttyput_string", 1 */
    fprintf(DbgFile, "ttyput_string");
    return(1);
  case ttyput_integer:		/* "ttyput_integer", 1 */
    fprintf(DbgFile, "ttyput_integer");
    return(1);
  case machine_output:		/* "machine_output", 1 */
    fprintf(DbgFile, "machine_output");
    return(1);
  case request:			/* "request", 2 */
    fprintf(DbgFile, "request");
    return(2);
  case store_vector:		/* "store_vector", 4 */
    fprintf(DbgFile, "store_vector");
    return(4);
  case read_vector:		/* "read_vector", 3 */
    fprintf(DbgFile, "read_vector");
    return(3);
  case make_vector:		/* "make_vector", 2 */
    fprintf(DbgFile, "make_vector");
    return(3);
  case write_vector:		/* "write_vector", 4 */
    fprintf(DbgFile, "write_vector");
    return(4);
  case close_vector:		/* "close_vector", 2 */
    fprintf(DbgFile, "close_vector");
    return(2);
  case link:			/* "link", 2 */
    fprintf(DbgFile, "link");
    return(2);
  case do_execute:		/* "execute", 2 */
    fprintf(DbgFile, "execute");
    return(2);
  case info:			/* "info", 2 */
    fprintf(DbgFile, "info");
    return(2);
  case debug:			/* "debug", 2 */
    fprintf(DbgFile, "debug");
    return(2);
  case code_info:		/* "code_info", 2 */
    fprintf(DbgFile, "code_info");
    return(2);
  case make_shared_g:		/* "code_info", 3 */
    fprintf(DbgFile, "make_shared");
    return(3);
  default:
    fprintf(DbgFile, "Invalid Guard Kernel Opcode = %d", Value);
    return(0);
  }
}
#endif

prs_print()
{
  fflush(OutFile);
  if (CP != Nil) {
    fprintf(DbgFile, "Current Process:\n");
    pr_print(CP);
  }
  if (QF != Nil) {
    fprintf(DbgFile, "Processes Still in Active Queue:\n");
    print_queue(QF);
  }
  if (Suspensions > (Activations + Failures + Losses)) {
    fprintf(DbgFile, "Suspended Processes:\n");
    pr_list_print(SQF);
  }
}

pr_hdr_print(PR)
heapP	PR;
{
  register int	ArgsNo;

  if (IsInt(*Prcdr_PR(PR))) {
    fprintf(DbgFile,"Native_Code(%d:%d)/%d\n", Val_of(*Prcdr_PR(PR)),
		Val_of(*Index_PR(PR)), ArgsNo_PR(PR));
  }
  else {
    register heapP Prcdr = Ref_Val(*Prcdr_PR(PR));
    register heapP Info;

    if (Str_Offset(Prcdr) != 0) {
      register heapP Mdl = Prcdr - Str_Offset(Prcdr);

      if ((Info = Mdl_Info(Mdl)) != Nil) {
	fprintf(DbgFile, "%s#", Mdl_Name(Info));
      }
      else {
	fprintf(DbgFile, "?#");
      }
    }
    if (Str_Type(Prcdr) == PrcdrType) {
      if ((Info = Prcdr_Info(Prcdr)) != Nil) {
	fprintf(DbgFile, "%s/%d\n", Prcdr_Name(Info), Prcdr_Arity(Info));
      }
      else {
	fprintf(DbgFile, "OLD?");
      }
    }
    else {
      if ((Info = Ctl_Info(Prcdr)) != Nil) {
	fprintf(DbgFile, "%s/%d/%d\n", Ctl_Name(Info), Ctl_Arity(Info),
		Ctl_Index(Info));
      }
      else {
	fprintf(DbgFile, "CTL?");
      }
    }
  }
}

pr_print(PR)
heapP	PR;
{
  register int	ArgsNo;

  if (IsInt(*Prcdr_PR(PR))) {
    fprintf(DbgFile,"Native_Code(%d:%d)/%d\n", Val_of(*Prcdr_PR(PR)),
		Val_of(*Index_PR(PR)), ArgsNo_PR(PR));
    ArgsNo = ArgsNo_PR(PR);
  }
  else {
    register heapP Prcdr = Ref_Val(*Prcdr_PR(PR));
    register heapP Info;

    if (Str_Offset(Prcdr) != 0) {
      register heapP Mdl = Prcdr - Str_Offset(Prcdr);

      if ((Info = Mdl_Info(Mdl)) != Nil) {
	fprintf(DbgFile, "%s#", Mdl_Name(Info));
      }
      else {
	fprintf(DbgFile, "?#");
      }
    }
    if (Str_Type(Prcdr) == PrcdrType) {
      if ((Info = Prcdr_Info(Prcdr)) != Nil) {
	fprintf(DbgFile, "%s", Prcdr_Name(Info));
	ArgsNo = Prcdr_Arity(Info);
      }
      else {
	fprintf(DbgFile, "OLD?");
	ArgsNo = ArgsNo_PR(PR);
      }
    }
    else {
      if ((Info = Ctl_Info(Prcdr)) != Nil) {
	fprintf(DbgFile, "%s/%d", Ctl_Name(Info), Ctl_Index(Info));
	ArgsNo = Ctl_Arity(Info);
      }
      else {
	fprintf(DbgFile, "CTL?");
	ArgsNo = ArgsNo_PR(PR);
      }
    }
  }
  {
    register heapP ArgPtr = Args_PR(PR);
    register int ArgsNoPR = ArgsNo_PR(PR);

    fprintf(DbgFile, "(");
    if (ArgsNo <= ArgsNoPR) {
      print_args(ArgPtr, ArgsNo);
    }
    else {
      print_args(ArgPtr, ArgsNoPR-1);
      fprintf(DbgFile, ", ");
      ArgPtr = ArgPtr+(ArgsNoPR-1);
      deref_ptr(ArgPtr); /* reach to packaging tuple */
      print_args((ArgPtr + 1), Val_of(*ArgPtr));
    }
    fprintf(DbgFile, ").\n");
  }
}

#define MaxDeep	3

static	int	Deep = 0;

print_args(Ptr, ArgsNo)
     heapP Ptr;
     int   ArgsNo;
{
  if (ArgsNo > 0) {
    register heapP P = Ptr;
    register int Count = ArgsNo;

    if (Deep != MaxDeep ) {
      for (; Count > 1; Count--, P++) {
	print_term(Ref_Word(P), Null);
	fprintf(DbgFile, ", ");
      }
      print_term(Ref_Word(P), Null);
    }
    else
      fprintf(DbgFile, "*%i*", Count);
  }
}

print_term(Val, Ptr)
     heapT Val;
     heapP Ptr;
{
  if ((Deep != MaxDeep)) {
    register heapT V = Val;
    register heapP P = Ptr;

    Deep++;
    deref(V, P);
    switch(Tag_of(V)) {
    case WrtTag:
      fprintf(DbgFile, "X%d", (P - CurHeap));
      if (!IsZeroed(*P)) {
	fprintf(DbgFile, ":%d",(Var_Val(*P) - CurHeap));
      }
      break;
    case RoTag:
      fprintf(DbgFile, "X%d", (P - CurHeap));
      fprintf(DbgFile, "?");
      break;
    case IntTag:
      fprintf(DbgFile, "%d", Int_Val(V));
      break;
    case RealTag:
      fprintf(DbgFile, "%g", real_val((P+1)));
      break; 
    case StrTag:
      switch (Str_Type(P)) {
      case CharType:
	{
	  register int  StrLen = Str_Length(P);
	  register char *PChar = (char *) (P+2);
	  
	  while (StrLen-- > 0) {
	    putc(*PChar++, DbgFile);
	  }
	}
	break;
      case PrcdrType:
	fprintf(DbgFile, "_PROCEDURE(");
	{
	  register heapP Info;

	  if (Str_Offset(P) != 0) {
	    register heapP Mdl = P - Str_Offset(P);
	    
	    if ((Info = Mdl_Info(Mdl)) != Nil) {
	      fprintf(DbgFile, "%s#", Mdl_Name(Info));
	    }
	    else {
	      fprintf(DbgFile, "?#");
	    }
	  }
	  if ((Info = Prcdr_Info(P)) != Nil) {
	    fprintf(DbgFile, "%s/%d", Prcdr_Name(Info), Prcdr_Arity(Info));
	  }
	  else {
	    fprintf(DbgFile, "OLD?");
	  }
	}
	fprintf(DbgFile, ")");
	break;
      case CtlType:
	fprintf(DbgFile, "_PROCEDURE(");
	{
	  register heapP Info;
	  if (Str_Offset(P) != 0) {
	    register heapP Mdl = P - Str_Offset(P);
	    
	    if ((Info = Mdl_Info(Mdl)) != Nil) {
	      fprintf(DbgFile, "%s#", Mdl_Name(Info));
	    }
	    else {
	      fprintf(DbgFile, "?#");
	    }
	  }
	  if ((Info = Ctl_Info(P)) != Nil) {
	    fprintf(DbgFile, "%s/%d/%d", Ctl_Name(Info), Ctl_Arity(Info),
		    Ctl_Index(Info));
	  }
	  else {
	    fprintf(DbgFile, "CTL?");
	  }
	}
	fprintf(DbgFile, ")");
	break;
      case MdlType:
	fprintf(DbgFile, "_MODULE");
	{
	  register heapP Info = Mdl_Info(P);

	  if (Info != Nil) {
	    fprintf(DbgFile, "(%s)", Mdl_Name(Info));
	  }
	  else {
	    fprintf(DbgFile, "(?)");
	  }
	}
	break;
      case FrznType:
	fprintf(DbgFile, "_FROZEN");
	break;
      }
      break;
    case NilTag:
      fprintf(DbgFile, "[]");
      break;
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
      fprintf(DbgFile, "[");
      {
	register int I = 20;

	while (IsList(*P) && (I > 0)) {
	  print_term(Off_List(*P), P);
	  I--;
	  P = Cdr(P);
	  deref_ptr(P);
	  if (IsList(*P)) {
	    fprintf(DbgFile, ",");
	    if (I == 0) {
	      fprintf(DbgFile, " ...");
	    }
	  }
	  else {
	    if (!IsNil(*P)) {
	      fprintf(DbgFile, "|");
	      print_term(*P, P);
	    }
	  }
	}
      }
      fprintf(DbgFile, "]");
      break;
    case TplTag:
      fprintf(DbgFile, "{");
      print_args(P+1, Int_Val(V));
      fprintf(DbgFile, "}");
      break;
    case VctrTag:
      fprintf(DbgFile, "(");
      print_args(P+1, Int_Val(V));
      fprintf(DbgFile, ")");
      break;
    case InvldTag:
      fprintf(DbgFile, "_INVALID(");
      print_term(*(P+1), (P+1));
      fprintf(DbgFile, ")");
      break;
    }
    Deep--;
  }
  else {
    fprintf(DbgFile, "*");
  }
}

print_queue(Ptr)
     heapP Ptr;
{
  register heapP P = Ptr;

  while (P != Nil) {
    pr_print(P);
    P = Ref_Val(*Next_PR(P));
  }
}

pr_list_print(Ptr)
     heapP Ptr;
{
  register heapP P0 = Ptr;

  while (P0 != Nil) {
    register heapP P1 = L_Ref_Val(*Ref_SR(P0));

    if (P1 != Nil) {
      pr_print(P1);
    }
    P0 = Ref_Val(*Next_SR(P0));
  }
}

print_suspension_list(Ptr)
     heapP Ptr;
{
  register heapP P0 = Ptr;

  while (IsList(*P0)) {
    register heapP P1 = L_Ref_Val(*L_Ref_Val(*Ref_SR(P0)));

    if (P1 != Nil) {
      pr_print(P1);
    }
    P0 = Ref_Val(*Next_SR(P0));
  }
}

print_stats()
{
  fprintf(DbgFile, "\n");
  fprintf(DbgFile, "Creations    : %d\n", Creations);
  fprintf(DbgFile, "Suspensions  : %d\n", Suspensions);
  fprintf(DbgFile, "Activations  : %d\n", Activations);
  if (Failures != 0) {
    fprintf(DbgFile, "Failures     : %d\n", Failures);
  }
  if (Losses != 0) {
    fprintf(DbgFile, "Losses       : %d\n", Losses);
  }
  fprintf(DbgFile, "Switches     : %d\n", Switches);
  fprintf(DbgFile, "Reductions   : %d\n", Reductions);
  fprintf(DbgFile, "Terminations : %d\n", Terminations);
  fprintf(DbgFile, "Collections  : %d\n", Collections);
  fprintf(DbgFile, "\n");

  fprintf(DbgFile, "Reductions Time is %d milliseconds, %.2f RPS\n",
	  (CpuTime - GCTime) / 1000,
	  (Reductions*1000000.0)/((CpuTime - GCTime) * 1.0));

  if (CurHeap < OtherHeap) {
    fprintf(DbgFile, "Used Total of %d Heap Words\n",
	   ((CurHeap-HeapStart)+(Collections*FreedAverage)+(HB-CurHeap)));
  }
  else {
    fprintf(DbgFile, "Used Total of %d Heap Words\n",
	   ((OtherHeap-HeapStart)+(Collections*FreedAverage)+(HB-CurHeap)));
  }

#if	(GC_STATS && (SUNOS4d1d3))
  fprintf(DbgFile, "Total Time is %d milliseconds, %.2f RPS\n",
	  (CpuTime / 1000), (Reductions*1000000.0)/(CpuTime * 1.0));
  fprintf(DbgFile, "GCtime is %d milliseconds, %.2f%% of Total Time\n",
	  (GCTime / 1000),
	  ((GCTime*1.0) / (CpuTime*1.0)) * 100);
  fprintf(DbgFile, "Copied Total of %d Heap Words\n",
	  (Collections*CopiedAverage));
  {
    struct rusage	R_Usage;

    if (getrusage(RUSAGE_SELF, &R_Usage) < 0) {
      do_exit("getrusage - RUSAGE_SELF", SYSTEM, errno, False);
    }
      
    fprintf(DbgFile, "Minor (Non I/O) Page Faults: %d\n",
	    R_Usage.ru_minflt - GCMinFlt);
    fprintf(DbgFile, "Major (I/O) Page Faults: %d\n",
	    R_Usage.ru_majflt - GCMajFlt);
    fprintf(DbgFile, "GC Minor (Non I/O) Page Faults: %d\n", GCMinFlt);
    fprintf(DbgFile, "GC Major (I/O) Page Faults: %d\n", GCMajFlt);
  }
#endif
  return(True);
}
