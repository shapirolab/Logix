/* $Header: /home/qiana/Repository/FcpEmulator/interface.c,v 1.10 2007/02/21 16:18:42 bill Exp $ */
/*
**	interface.c - unix interface functions.
**
**	Michael Hirsch and Bill Silverman		February 1986
**
**	Last update by:	     $Author: bill $
**		       	     $Date: 2007/02/21 16:18:42 $
**	Currently locked by: $Locker:  $
**			     $Revision: 1.10 $
**			     $Source: /home/qiana/Repository/FcpEmulator/interface.c,v $
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
/**********************************************************************/

#define		PROG_ID		"interface"

#include	<pwd.h>
#include	<errno.h>
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
      {
	heapP command_line;

	if ((!strcmp(op, "system"))  &&  (Arity == 2)) {
	  string_var(command_line, ++T);
	  system((char *) (command_line+2));
	  return(True);
	}
	if ((!strcmp(op, "system"))  &&  (Arity == 4)) {
	  heapP terminate_code;
	  heapP error_number;
	  int system_reply;

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
	  sprintf ((char *) (HP+2), "%12d.%06d\'0'",
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
	      (sscanf((char *) (GmTime+2), "%d.%d\0",
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
