/*
** This module is part of EFCP.
**

   Copyright 2007 Michael Hirsch, William Silverman 
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
**	interface.c - unix interface functions.
**
**	February 1986
**
*/

#include <string.h>
#ifdef MACINTOSH
#define _XOPEN_SOURCE
#endif

/**********************************************************************
			interface.c

Interface accepts the following commands:
	{flush}			flushes stdout.
	system(Command?)	executes via the unix shell Command
	date(Date^)		returns system date in Date
	userinfo(User_Name^, User_Home_Directory^)
				returns the current username and
				associated home directory.
	whoami(User_Name^)	returns the current user name.
	homedir(User_Name?,  Home_Directory^)
				returns the home directory for User_Name 
	processid(ProcessId^)   returns a composite process identifier
				for the emulator process.
	getenv(Name, Value)	returns the value for the environment
				variable Name. [] if does not exist.
	sprint(Format?, Value?, Formed^)
				returns a formatted string for Value,
				a constant, which agrees in type with
				the Format specification (see "The C
				Programming Language, ANSI C, pps.
				243-245).
				
/**********************************************************************/

#define		PROG_ID		"interface"

#include	<pwd.h>
#include	<errno.h>
#include	<math.h>
#include	<netdb.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<signal.h>
#include	<sys/types.h>
#include	<sys/stat.h>
#include	<sys/time.h>
#include	<time.h>
#include	<sys/file.h>
#include	<sys/param.h>

#include	"fcp.h"
#include	"codes.h"
#include	"global.h"
#include	"macros.h"

extern FILE *OutFile;

#define	Flush	1
#define	Command	2
#define	Date	4
#define UserData 5

/* #define	DateLEN	12 */
#define	DateLEN	14
#define	GmtLEN	8

#define	writable(P,T)					\
  {							\
    P = T;						\
    deref_ptr(P);					\
    if (!IsWrt(*P)) {					\
      if (IsRo(*P)) {					\
        sus_tbl_add(P);					\
      }							\
      return(False);					\
    }							\
  }

#define string_var(T_Save, T)				\
  {							\
    heapT P;						\
    T_Save  = (T);					\
    deref_ptr(T_Save);					\
    P = *(T_Save);				        \
    if (!IsStr(P)) {					\
        if (IsVar(P)) {					\
          sus_tbl_add(T_Save);				\
        }						\
      return(False);					\
    }							\
  }

#define integer_var(P,T)				\
  {							\
    heapP PT = (T);					\
							\
    P = *(PT);						\
    deref_val(P);					\
    if (!IsInt(P)) {					\
        if (IsVar(P)) {					\
          sus_tbl_add((PT));				\
        }						\
      return(False);					\
    }							\
  }

#define const_var(T_Save, T)				\
  {							\
    heapT P;						\
    T_Save  = (T);					\
    deref_ptr(T_Save);					\
    P = *(T_Save);				        \
    if (!IsInt(P) && !IsReal(P) && !IsStr(P)) {		\
        if (IsVar(P)) {					\
          sus_tbl_add(T_Save);				\
        }						\
      return(False);					\
    }							\
  }

/*
**  INTERFACE -- distribute call to UNIX interface
**	Fcp Usage: execute(Offset,{Functor,Arguments})
*/

interface(T)
     heapP T;
{
  int	Arity;
  heapP	Index;
  char  *op;


  deref_ptr(T);
  if (!IsTpl(*T)) {
    return(False);
  }
  Arity = Arity_of(*T);
  Index = ++T;
  deref_ptr(Index);
  /*** The following code may be deleted once all calls are translated to
    normal English notation ***/
  if (IsInt(*Index)) {
    return(False);
  }
  else
    /*** End of code which may be deleted in translation ***/
    if (IsStr(*Index))
      op = (char *) (Index+2);
    else {
      if (IsVar(*Index)) 
	sus_tbl_add(Index);
      return(False);
    }

  switch(*op)
    {
    case 'f' :
      if ((!strcmp(op, "flush"))  &&  (Arity == 1)) {
	fflush(OutFile);	
	return(True);
      }
      else
	return(False);
    case 's' :
      if (!strcmp(op, "sprint")  &&  (Arity == 4)) {
	heapP Format, Value, Output, PStr;
	int Result;
	char *Pch, *Fch, C;
	int params[4] = {0, 0, 0, 0}, type;

	string_var(Format, ++T);

	Value = (++T);
	deref_ptr(Value);

	writable(Output, ++T);

	Fch = (char *)(Format + StrHdrWords);

/*printf("verify\n"); /*debugging */

	if ((type = verify_format(Fch, params)) < 0) {
/*printf("unverified\n"); /*debugging */
	  return(False);
	}
/*printf("type = '%c', params = %i{%i, %i}, count = %i\n", type, 
  params[0], params[1], params[2], params[3]); /*debugging*/

	Pch = (char *)(HP + StrHdrWords);
	{
	  int wrap = 2*sizeof(heapT) + params[3];

	  /* measure the content of the field and convert if enough room */
	  if (IsInt(*Value) && ((type == 0) || (type == '%'))) {
	    if (heap_space(sizeof(heapT)) < (params[1] + wrap)) {
/*  printf("character output about to overflow heap\n"); /* debugging */
	      err_tbl_add(MACHINE, ErHPSPACE);
	      return(False);
	    }
	    Result = sprintf(Pch, Fch, Int_Val(*Value));
	  }
	  else {
	    if (IsStr(*Value) && (type == 's')) {
	      unsigned int length = Str_Length((char *)(Value + StrHdrWords));
	      unsigned int max = params[1];

	      switch (params[0])
		{
		case 2:
		  if (params[2] < length) {
		    length = params[2];
		  }
		case 1:
		  if (max > length) {
		    break;
		  }
		case 0:
		  max = length;
		}
	      if (heap_space(sizeof(heapT) < (max + wrap))) {
/*  printf("string output about to overflow heap\n"); /* debugging */
		err_tbl_add(MACHINE, ErHPSPACE);
		return(False);
	      }

	      Result = sprintf(Pch, Fch, (char *)(Value + StrHdrWords));
	    }
	    else if (IsInt(*Value) && (type == 'i')) {
	      int val = abs(Int_Val((int) *Value));
	      int intfield = (val ? floor(log10(val)) : 0) + 1;
	      
	      if (intfield < params[1]) {
		intfield = params[1];
	      }
	      if (intfield < params[2]) {
		intfield = params[2];
	      }

	      if (heap_space(sizeof(heapT)) < (intfield + wrap)) {
/*  printf("integer output about to overflow heap\n"); /* debugging */
		err_tbl_add(MACHINE, ErHPSPACE);
		return(False);
	      }
	      Result = sprintf(Pch, Fch, Int_Val((int) *Value));
	    }
	    else if (IsReal(*Value) && ('e' <= type) && (type <= 'g')) {
	      double val = fabs(real_val((Value+1)));
	      int intfield = floor(log10(val)) + 1;

	      switch (params[0])
		{
		case 2:
		  if (60 > params[2]) {
		    wrap += params[2] - 60;
		  }
		case 1:
		  if (params[1] > intfield) {
		    intfield = params[1];
		  }
		case 0:
		  wrap += intfield + 60; /***** magic number *****/
		}

	      if (heap_space(sizeof(heapT)) < wrap) {
/*  printf("real output about to overflow heap\n"); /* debugging */
		err_tbl_add(MACHINE, ErHPSPACE);
		return(False);
	      }
	      Result = sprintf(Pch, Fch, real_val((Value+1)));
	    }
	    else {
	      if (IsVar(*Value)) {
		sus_tbl_add(Value);
	      }
	      return(False);
	    }
	  }
	}

	PStr = HP;
	*HP++ = Str_Hdr1(CharType, 0);
	*HP++ = Str_Hdr2(hash(Pch, Result), Result);
	Pch = ((char *)HP) + Result;
	HP += Str_Words(PStr);

	/* Fill last word of string with nulls */
	while (Pch < (char *) HP) {
	  *Pch++ = '\0';
	}

	asgn(*Output, Output, Ref_Word(PStr));
	return(True);
      }
      else
      {
	heapP command_line;
	int system_reply;

	if ((!strcmp(op, "system"))  &&  (Arity == 2)) {
	  string_var(command_line, ++T);
	  system_reply = system((char *) (command_line+2));
	  return(True);
	}
	if ((!strcmp(op, "system"))  &&  (Arity == 4)) {
	  heapP terminate_code;
	  heapP error_number;

	  string_var(command_line, ++T);
	  writable(terminate_code, ++T);
	  writable(error_number, ++T);
	  errno = 0;
	  system_reply = system((char *) (command_line+2));
	  asgn(*terminate_code, terminate_code, Word(system_reply, IntTag));
	  asgn(*error_number, error_number, Word(errno, IntTag));
	  return(True);
	}
	else
	  return(False);
      }
    case 'd' :
      {
	heapP DateTime;
	heapP PStr = HP;
	struct tm *localtime();
	struct timeval tval;
	struct timezone tzone;

	if ((strcmp(op, "date"))  ||  (Arity != 2)) {
	  return(False);
	}
	writable(DateTime, ++T);
	gettimeofday(&tval,&tzone);
	packdate(localtime(&(tval.tv_sec)),HP+2);
	*HP++ = Str_Hdr1(CharType, 0);
	*HP++ = Str_Hdr2(hash((char *) (PStr + 2), DateLEN), DateLEN);
	HP += Str_Words(PStr);
	asgn(*DateTime, DateTime, Ref_Word(PStr));
	return(True);
      }
    case 'e' :
      {
#if defined(MACINTOSH)
	extern const int sys_nerr;
#else
	extern int sys_err;
#endif
#if !(defined(LINUX) || defined(MACINTOSH) || defined(CYGWIN))
	extern char *sys_errlist[];
#endif

	heapP ErrorString;
	heapT ErrorNumber;
	int error_number;

	if ((strcmp(op, "errno"))  ||  (Arity != 3)) {
	  return(False);
	}
	integer_var(ErrorNumber, ++T);
	error_number = Int_Val(ErrorNumber);
	writable(ErrorString, ++T);
	make_string(strerror(error_number), ErrorString);
	return(True);
      }
    case 'g' :
      if ((!strcmp(op, "getenv")) && (Arity == 3)) {
	heapP Name, Result;
	char *Value;

	string_var(Name, ++T);
	writable(Result, ++T);

	if ((Value = getenv(((char *) (Name + StrHdrWords)))) != NULL) {
	  make_string(Value, Result);
	}
	else {
	  asgn(*Result, Result, Word(0, NilTag));
	}
	return(True);
      }
      {
	heapP DateTime, GmTime;
	heapP PStr = HP;
	struct tm *gmtime();
	struct tm time;
	struct timeval tval;
	struct timezone tzone;

	if (!(strcmp(op, "gmtime")) && (Arity == 2)) {
	  writable(DateTime, ++T);
	  gettimeofday(&tval,&tzone);
	  sprintf ((char *) (HP+2), "%12ld.%06ld\'0'",
		   tval.tv_sec, tval.tv_usec);
	  *HP++ = Str_Hdr1(CharType, 0);
	  *HP++ = Str_Hdr2(hash((char *) (PStr + 2), 19), 19);
	  HP += Str_Words(PStr);
	  asgn(*DateTime, DateTime, Ref_Word(PStr));
	  return(True);
	}
	if (!(strcmp(op, "gmt2date")) && (Arity == 3)) {
	  string_var(GmTime, ++T);
	  if ( (Str_Length(GmTime) != 19) ||
	      (((char *)(GmTime+2))[12] != '.') ||
	      (sscanf((char *) (GmTime+2), "%ld.%ld\0",
		      &(tval.tv_sec), &(tval.tv_usec)) != 2) )
	    return(False);
	  writable(DateTime, ++T);
	  packdate(gmtime(&(tval.tv_sec)),HP+2);
	  *HP++ = Str_Hdr1(CharType, 0);
	  *HP++ = Str_Hdr2(hash((char *) (PStr + 2), DateLEN), DateLEN);
	  HP += Str_Words(PStr);
	  asgn(*DateTime, DateTime, Ref_Word(PStr));
	  return(True);
	}
	if (!(strcmp(op, "gmdate")) && (Arity == 2)) {
	  writable(DateTime, ++T);
	  gettimeofday(&tval,&tzone);
	  packdate(gmtime(&(tval.tv_sec)),HP+2);
	  *HP++ = Str_Hdr1(CharType, 0);
	  *HP++ = Str_Hdr2(hash((char *) (PStr + 2), DateLEN), DateLEN);
	  HP += Str_Words(PStr);
	  asgn(*DateTime, DateTime, Ref_Word(PStr));
	  return(True);
	}
	if ((strcmp(op, "gm2local")) || (Arity != 3))
	  return(False);
	string_var(GmTime, ++T);
	if ((Str_Length(GmTime) != 14) ||
	    (strcmp("00000000000000", (char *) (GmTime+2)) > 0) ||
	    (strcmp("99999999999999", (char *) (GmTime+2)) < 0)) {
	  if ((Str_Length(GmTime) != 12) ||
	      (strcmp("000000000000", (char *) (GmTime+2)) > 0) ||
	      (strcmp("999999999999", (char *) (GmTime+2)) < 0))
	    return(False);
	}
	writable(DateTime, ++T);
	unpackdate((char *) (GmTime+2), &(time));
	time.tm_isdst = 0;
	gettimeofday(&tval,&tzone);
	tval.tv_sec = mktime(&(time)) + (tzone.tz_minuteswest * 60);
	tval.tv_usec = 0;
	packdate(localtime(&(tval.tv_sec)),HP+2);
	*HP++ = Str_Hdr1(CharType, 0);
	*HP++ = Str_Hdr2(hash((char *) (PStr + 2), DateLEN), DateLEN);
	HP += Str_Words(PStr);
	asgn(*DateTime, DateTime, Ref_Word(PStr));
	return(True);
      }
    case 'u' :
      {
	heapP User_Name, User_HomeDir;
	struct passwd *pwe;

	if ((strcmp(op, "userinfo"))  ||  (Arity != 3)) {
	  return(False);
	}
	writable(User_Name, ++T);
	writable(User_HomeDir, ++T);

	pwe = getpwuid(geteuid());
      
	make_string(pwe -> pw_name, User_Name);
	make_string(pwe -> pw_dir, User_HomeDir);
	return(True);
      }
    case 'w' :
      {
	char  *name;
	heapP User_Name;
	struct passwd *pwe;

	if ((strcmp(op, "whoami"))  ||  (Arity != 2)) {
	  return(False);
	}
	writable(User_Name, ++T);

	/****
	if (name = (char *) getlogin()) {
	  make_string(name, User_Name);
	}
	else /****/{
	  if (pwe = getpwuid(geteuid())) {
	    make_string(pwe -> pw_name, User_Name);
	  }
	  else {
	    asgn(*User_Name, User_Name, Word(0, NilTag));
	  }
	}
	return(True);
      }
    case 'h' :
      {
	heapP User_Name, Home_Dir;
	struct passwd *pwe;

	if ((strcmp(op, "homedir"))  ||  (Arity != 3)) {
	  return(False);
	}
	string_var(User_Name, ++T);
	writable(Home_Dir, ++T);
	pwe = getpwnam((char *) (User_Name+2));
	if (pwe) {
	  make_string(pwe -> pw_dir, Home_Dir);
	}
	else {
	  asgn(*Home_Dir, Home_Dir, Word(0, NilTag));
	}
	return(True);
      }
    case 'n' :  {
      char name[MAXHOSTNAMELEN+1];
      struct hostent *hostP;
      heapP HostName, HostId, DomainName;

      if ((strcmp(op, "nethost")) || (Arity != 4)) {
	  return(False);
	}

	writable(HostName, ++T);
	writable(HostId, ++T);
	writable(DomainName, ++T);
	gethostname(name, MAXHOSTNAMELEN);
	hostP = gethostbyname(name);
	make_string(hostP->h_name, HostName);
	strcpy(name, (char *) inet_ntoa(hostP->h_addr_list[0]));
	make_string(name, HostId);
        getdomainname(name, MAXHOSTNAMELEN);
	make_string(name, DomainName);
	return(True);
      }
    case 'p' :
      {
	heapP ProcessId;
	char name[MAXHOSTNAMELEN+1];
	char *pid;
	char *send;
	int  len;
	int  id;

	if ((strcmp(op, "processid"))  ||  (Arity != 2)) {
	  return(False);
	}
	writable(ProcessId, ++T);
	gethostname(name, MAXHOSTNAMELEN);
	for (pid = name; *pid != '\0' && (pid - name < MAXHOSTNAMELEN);
	     pid++) {}
	sprintf(pid, ".%d", getpid());
	make_string(name, ProcessId);
	return(True);
      }
    default:
      return(False);
    }
}

make_string(Pwx, SP)
     char *Pwx;
     heapP SP;
{
  heapP PStr = HP;
  char  *Name = (char *)(PStr+2);
  char  *send;
  int   len;

  for (send = Name; *Pwx != '\0'; *send++ = *Pwx++) {
  }
  len = send - Name;

  *HP++ = Str_Hdr1(CharType, 0);
  *HP++ = Str_Hdr2(hash(Name, len), len);
  HP += Str_Words(PStr);

  while (send < (char *)HP) {
    *send++ = '\0';
  }

  deref_ptr(SP);
  asgn(*SP, SP, Ref_Word(PStr));
}

unpackdate(String, time)
     struct tm *time;
     char *String;
{
  if (String[12] && String[13]) {
    time->tm_year = (String[0]-'0')*1000 + (String[1]-'0')*100 +
                    (String[2]-'0')*10  + String[3] - '0';
    time->tm_mon  = (String[4]-'0')*10 + String[5] - '1';
    time->tm_mday = (String[6]-'0')*10 + String[7] - '0';
    time->tm_hour = (String[8]-'0')*10 + String[9] - '0';
    time->tm_min  = (String[10]-'0')*10 + String[11] - '0';
    time->tm_sec  = (String[12]-'0')*10 + String[13] - '0';
  }
  else {
    time->tm_year = (String[0]-'0')*10 + String[1] - '0';
    time->tm_mon  = (String[2]-'0')*10 + String[3] - '1';
    time->tm_mday = (String[4]-'0')*10 + String[5] - '0';
    time->tm_hour = (String[6]-'0')*10 + String[7] - '0';
    time->tm_min  = (String[8]-'0')*10 + String[9] - '0';
    time->tm_sec  = (String[10]-'0')*10 + String[11] - '0';
  }
  time->tm_wday = 1;
  time->tm_yday = 0;
}


int verify_format(Fch, params)
     char *Fch;
     int params[4];
{
  char *F;
  unsigned int px = 0, type;

/* Scan format up to conversion spec */

  for(F = Fch; *F; F++) {
    if (*F == '%') {
/*printf("found %% \n"); /* debugging */
      ++F;
      if (*F == '%') {
/*printf("found %%%%\n"); /* debugging */
	continue;
      }

/* Scan to first non-modifier */

      while (*F) {
	switch (*F) 
	  {
	  case '+': case '-': case ' ': case '0': case '#':
	    break;
	  default :
	    goto notmod;
	  }
	++F;
      }
    notmod:
/*printf("done mods\n"); /* debugging */

/* modifications may be followed by parameters
   scan and convert parameter 
*/
      while (*F) {
	if (('0' > *F || *F > '9') && (*F != '.')) {
	  break;
	}
	else {
	  if (*F == '.') {
	    px = 2;
	    params[2] = 0;
	  }
	  else {
	    if (!px) {
	      px = 1;
	    }
	    params[px] = params[px]*10 + (*F - '0');
	  }
	}
	++F;
      }
      params[0] = px;
/*printf("done px = %i - [%i,%i]\n", px, params[0], params[1]); /* debugging */

/* 'h' or 'l' or 'L'  may precede the conversion letter */ 

      while (*F) {
	if ((*F != 'h') && (*F != 'l') && (*F != 'L')) {
	  break;
	}
	++F;
      }

/* Check for legitimate conversion characters - otherwise none!? */

      switch (*F)
	{
	case 'd': case 'D': case 'i': case 'I':case 'o': case 'O': 
	case 'u': case 'U': case 'x': case 'X':
	  type = 'i';
	  break;
	case 'e': case 'E': case 'g': case 'G':
	  type = 'e';
	  break;
	case 'f': case 'F':
	  type = 'f';
	  break;
	case 's': case 'S':
	  type = 's';
	  break;
	case 'n': case 'N': case 'p': case 'P': case '*':
	  return(-1);
	case 'a': case 'A': case 'b': case 'B': case 'c': case 'C': case 'H':
	case 'j': case 'J': case 'k': case 'K': case 'm': case 'M': case 'Q':
	case 'r': case 'R': case 'T': case 'w': case 'W': case 'y': case 'Y':
	case 'Z': case '`': case '~': case '!': case '@': case '$': case '%':
	case '^': case '&': case '(': case ')': case '_': case '=': case '[':
	case ']': case '{': case '}': case '\\': case '|': case ';': case ':':
	case '"': case '<': case '>': case '?':
	  type = '%';
	  break;
	default:
	  type = 0;
	}
/*printf("conversion type * %c *\n", *F); /* debugging */
      ++F;

/* exclude trailing '%' (but not "%%") - count trailing characters */

      while (*F) {
	if (*F == '%') {
	  ++F;
	  if (*F != '%') {
	    return(-1);
	  }
	}
	++params[3];
	++F;
      }
/*printf("verified\n"); /* debugging */
      return(type);
    }
    ++params[3];
  }
  return(type);
}
