/*
** This module is part of EFCP.
**

     Copyright 2007 Avshalom Houri, Daniel Szoke
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
**	file.c - foreign kernals for handling files.
**
**	This was taken from ~avshalom/Fcp/Emulator/k_effects.c with:
**	1) "f_switch" added.
**	2) Procedures declared static.
**	3) 'A' register declared a parameter (i.e. local) to procedures.
*/

#include	<sys/unistd.h>
#include	<sys/param.h>
#include	<sys/types.h>
#include	<sys/stat.h>
#include	<sys/time.h>
#include	<time.h>
#include	<sys/file.h>
#include	<stdio.h>
#ifdef	SUNOS5d3
#include	<sys/fcntl.h>
#endif
#include	<errno.h>
#include	<string.h>
#include	<strings.h>
#include	<stdlib.h>

#if !(defined(LINUX) || defined(MACINTOSH) || defined(CYGWIN))
extern char *sys_errlist[];
#endif

extern FILE *DbgFile;

#include	"fcp.h"
#include	"codes.h"
#include	"global.h"
#include	"macros.h"

#define	DATELEN		14
/*#define	DATELEN		12 */

#define	GET_FILE	1
#define	GET_MODULE	2
#define	PUT_FILE	3
#define	APPEND_FILE	4
#define	PUT_MODULE	5
#define GETWD		6
#define	FILEINFO	7
#define ISDIRECTORY     8
#define BOPEN           9               /* buffered file I/O */
#define BCLOSE          10
#define BREAD           11
#define BWRITE          12



/*
*	F_SWITCH: Branch on different procedures.
*/

static get_file();
static get_string_file();
static do_get_file();
static put_file();
static append_file();
static put_module();
static getworkingdirectory();
static fileinfo();
static isdirectory();

file(T)
     heapP T;
{
  heapT	sw = *(T+1);

  deref_val(sw);
  switch(Int_Val(sw)) {
  case GET_FILE:
    return(get_file(T+2));
  case GET_MODULE:
    return(get_string_file(T+2));
  case PUT_FILE:
    return(put_file(T+2));
  case APPEND_FILE:
    return(append_file(T+2));
  case PUT_MODULE:
    return(put_module(T+2));
  case GETWD:
    return(getworkingdirectory(T+2));
  case FILEINFO:
    return(fileinfo(T+2));
  case ISDIRECTORY:
    return(isdirectory(T+2));

  case BOPEN:
      return(b_open(T+2,  Arity_of(*T)-1));
  case BCLOSE:
      return(b_close(T+2, Arity_of(*T)-1));
  case BREAD:
      return(b_read(T+2,  Arity_of(*T)-1));
  case BWRITE:
      return(b_write(T+2, Arity_of(*T)-1));

  default:
    return(False);
  }
}  /* f_switch */

/*
**  GET_FILE  --  reads the contents of a file into a string.
**		  The status flag returns errno if an error is encountered.
**
**	Fcp Usage:	string(F), var(S), var(Status), get_file(F,S,Status)
*/
static get_file(A)
     heapP	A;
{
  heapP	F = A;
  heapP	S = A+1;
  heapP	Status = A+2;

  deref_ptr(F);
  deref_ptr(S);
  deref_ptr(Status);
  return(do_get_file(False, F, S, Status));
}

/*
**  GET_STRING_FILE  --  reads the contents of a file into a string.
**		  	 The status flag returns errno if an I/O error is
**			 encountered, or -1 if file is not a string file.
**
**	Fcp Usage:	string(F), var(S), var(Status),
**			get_string_file(F,S,Status)
*/
static get_string_file(A)
heapP	A;
{
  heapP	F = A;
  heapP	S = A+1;
  heapP	Status = A+2;

  deref_ptr(F);
  deref_ptr(S);
  deref_ptr(Status);
  return(do_get_file(True, F, S, Status));
}

static do_get_file(String_File, F, S, Status)
int	String_File;
heapP	F;
heapP	S;
heapP	Status;
{
  heapP	PStr, P;
  
  extern int errno;
  char *sp;
  int slen;
  int fd, cc;

  errno = 0;
  fd = open( (char *)(F+2), O_RDONLY);
  if (fd < 0) {	
    asgn(*Status, Status, Word(errno, IntTag));
    return(True);
  }
  PStr = HP;
  P = PStr;
  if (String_File) {
    /*
     *  read string header
     */
    if (read(fd, P, sizeof(heapT)*2) != sizeof(heapT)*2) {
      if (errno) {
	asgn(*Status, Status, Word(errno, IntTag));
      }
      else {
	asgn(*Status, Status, Word(-1, IntTag));
      }
      close(fd);
      return(True);
    }
#ifdef	ULTRIX
    cnv_w(PStr);
    cnv_w(PStr+1);
#endif
    if (!IsStr(*P) ) {
      close(fd);
      asgn(*Status, Status, Word(-1, IntTag));
      return(True);
    }
  }
  P += 2;
  sp = (char *) P;
  slen = 0;
  while ((!ended_heap(sp+BUFSIZ)) && (cc = read(fd, sp, BUFSIZ)) > 0) {
    sp += cc;
    slen += cc;
  }
  close(fd);
  if (ended_heap(sp + BUFSIZ)) {
    err_tbl_add(MACHINE, ErHPSPACE);
    return(False);
  }
  if (cc < 0) {
    asgn(*Status, Status, Word(errno, IntTag));
    return(True);
  }
  if (String_File) {
    if (slen < Str_Length(PStr)) {
      asgn(*Status, Status, Word(-1, IntTag));
      return(True);
    }
    if (slen > Str_Length(PStr)) {
      sp -= Str_Length(PStr) - slen;
    }
#ifdef	ULTRIX
    cnv_w(PStr);
    cnv_w(PStr+1);
    if (!imp_convert(PStr)) {
      asgn(*Status, Status, Word(-2, IntTag));
      return(True);
    }
#endif
    HP = P;
  }
  else {
    *HP++ = Str_Hdr1(CharType, 0);
    *HP++ = Str_Hdr2(hash((char *) (PStr+2), slen), slen);
  }
  HP += Str_Words(PStr);
  while (sp < (char *)HP) {
    *sp++ = '\0';
  }
  asgn(*S, S, Ref_Word(PStr));
  asgn(*Status, Status, Word(0, IntTag));
  return(True);
}

#define	PUT_REG_FILE		0
#define	APPEND_REG_FILE		1
#define	PUT_STRING_FILE		2

/*
**  PUT_FILE  --  writes a string into a file
**		  The status flag returns errno if an error is encountered.
**
**	Fcp Usage:	wait(F), wait(S), string(F), string(S), var(Status),
**			put_file(F,S,Status)
*/
static put_file(A)
heapP	A;
{
  heapP	F = A;
  heapP	S = A+1;
  heapP	Status = A+2;

  deref_ptr(F);
  deref_ptr(S);
  deref_ptr(Status);
  return(do_put_file(PUT_REG_FILE, F, S, Status));
}

/*
**  APPEND_FILE  --  appends a string to a file
**		  The status flag returns errno if an error is encountered.
**
**	Fcp Usage:	wait(F), wait(S), string(F), string(S), var(Status),
**			append_file(F,S,Status)
*/
static append_file(A)
heapP	A;
{
  heapP	F = A;
  heapP	S = A+1;
  heapP	Status = A+2;

  deref_ptr(F);
  deref_ptr(S);
  deref_ptr(Status);
  return(do_put_file(APPEND_REG_FILE, F, S, Status));
}

/*
**  PUT_MODULE  --  Like put_file but also writes the string header.
**
**	Fcp Usage:	wait(F), wait(S), string(F), string(S), var(Status),
**			put_module(F,S,Status)
*/
static put_module(A)
heapP	A;
{
  heapP	F = A;
  heapP	S = A+1;
  heapP	Status = A+2;

  deref_ptr(F);
  deref_ptr(S);
  deref_ptr(Status);
  return(do_put_file(PUT_STRING_FILE, F, S, Status));
}

do_put_file(File_Type, F, S, Status)
int	File_Type;
heapP	F;
heapP	S;
heapP	Status;
{
  extern int errno;
  char *sp;
  int fd, cc, slen;

  errno = 0;
  if (File_Type == APPEND_REG_FILE) {
    fd = open((char *)(F+2), O_WRONLY | O_CREAT | O_APPEND , 0666);
  }
  else {
    fd = open((char *)(F+2), O_WRONLY | O_CREAT | O_TRUNC, 0666);
  }
  if (fd < 0) {	
    asgn(*Status, Status, Word(errno, IntTag));
    return(True);
  }
  /*
   *  get the length of the string, then write the bytes
   */
  if (File_Type == PUT_STRING_FILE) {
    /*  write string header out with the string  */
    slen = Str_Length(S) + (2*sizeof(heapT));
#ifdef	ULTRIX
    if (!exp_convert(S)) {
      close(fd);
      asgn(*Status, Status, Word(-10, IntTag));
      return(True);
    }
#endif
    sp = (char *)S;
  }
  else {
    /*  just write string out with no header */
    slen = Str_Length(S);
    sp = (char *)(S+2);
  }
  while (slen > 0) {
    cc = (slen > BUFSIZ) ? BUFSIZ : slen;
    if (write(fd, sp, cc) != cc) {
#ifdef	ULTRIX
      if (File_Type == PUT_STRING_FILE) {
        imp_convert(S);
      }
#endif
      close(fd);
      asgn(*Status, Status, Word(errno, IntTag));
      return(True);
    }
    slen -= cc;
    sp += cc;
  }
#ifdef	ULTRIX
  if (File_Type == PUT_STRING_FILE) {
    imp_convert(S);
  }
#endif
  close(fd);
  asgn(*Status, Status, Word(0, IntTag));
  return(True);
}

/*
**  FILEINFO -- use system call stat to get file modify date, and construct
**		a string which is directly comparable with other dates.
**
**
**	Fcp Usage: string(FileName), var(FileInfo), fileinfo(FileName,FileInfo)
*/
static fileinfo(A)
     heapP A;
{
  struct stat buf;
  struct tm *localtime();
  heapP	FileName = A;
  heapP	FileInfo = A+1;
  heapP	PStr;

  deref_ptr(FileName);
  deref_ptr(FileInfo);
  PStr = HP;
  buf.st_mtime = 0;
  if (!IsStr(*FileName) || stat(((char *) (FileName+2)), &buf)) {
    asgn(*FileInfo, FileInfo, Word(0, IntTag));
    return(True);
  }
  else {
    packdate(localtime(&(buf.st_mtime)),HP+2);
    *HP++ = Str_Hdr1(CharType, 0);
    /* date has offset 0 */
    *HP++ = Str_Hdr2(hash((char *) (PStr+2), DATELEN), DATELEN);
    HP += Str_Words(PStr);
    asgn(*FileInfo, FileInfo, Ref_Word(PStr));
    return(True);
  }
}

/*
**  ISDIRECTORY -- use system call stat to find out whether a file is a
**		   directory or not
**
**
**	Fcp Usage: string(FileName), var(FileInfo), fileinfo(FileName,Status)
**
**      Status = 1 (true) | 0 (false)
*/
static isdirectory(A)
     heapP A;
{
  struct stat buf;
  heapP	FileName = A;
  heapP	Status = A+1;
  int	result_flag;

  deref_ptr(FileName);
  deref_ptr(Status);
  
  buf.st_mtime = 0;
  if (!IsStr(*FileName) || stat(((char *) (FileName+2)), &buf)){
    result_flag = 0;
  }
  else {
    if ((buf.st_mode & S_IFDIR) == S_IFDIR) {
      result_flag = 1;
    }
    else {
      result_flag = 0;
    }
  }
  asgn(*Status, Status, Word(result_flag, IntTag));
  return(True);
}

/*
**  GETWD -- uses the getwd system call to get the current working directory
**
**	Fcp Usage: var(Directory), getwd(Directory)
*/
static getworkingdirectory(A)
     heapP A;
{
/*	heapP	Directory = A; */
/* kludge - ioserver forces first arg to be ground */
  heapP	Directory = A+1;
  heapP	Pstr;
  char  *Pchars;
  char  *zeroes;
  int   slen;
  char	*pwd = getenv("PWD");

  deref_ptr(Directory);
  Pstr = HP;
  Pchars = (char *) (Pstr + 2);
  if (pwd != NULL) {
    strcpy(((char *) Pchars), pwd);
  }
  else {
    if (ended_heap(((heapP) (Pchars + MAXPATHLEN)))) {
      return(False);
    }
    if ((char *)getcwd(Pchars, MAXPATHLEN) == NULL) {
      return(False);
    }
  }
  slen = strlen(Pchars);
  *HP++ = Str_Hdr1(CharType,0);
  *HP++ = Str_Hdr2(hash((char *)(Pstr + 2),slen),slen);

  HP += Str_Words(Pstr);

  zeroes = Pchars + slen;
  while ((zeroes < (char *) HP)) {
    *zeroes++ = '\0';
  }

  asgn(*Directory, Directory, Ref_Word(Pstr));
  return(True);
}
/*

packdate(time, String)
     struct tm *time;
     char *String;
{
  String[0]  = time->tm_year / 10 + '0';
  String[1]  = time->tm_year % 10 + '0';
  String[2]  = time->tm_mon  /  9 + '0';
  String[3]  = (time->tm_mon+1) % 10 + '0';
  String[4]  = time->tm_mday / 10 + '0';
  String[5]  = time->tm_mday % 10 + '0';
  String[6]  = time->tm_hour / 10 + '0';
  String[7]  = time->tm_hour % 10 + '0';
  String[8]  = time->tm_min  / 10 + '0';
  String[9]  = time->tm_min  % 10 + '0';
  String[10] = time->tm_sec  / 10 + '0';
  String[11] = time->tm_sec  % 10 + '0';
  String[12] = '\0';
  String[13] = '\0';
  String[14] = '\0';
  String[15] = '\0';
}
*/
packdate(time, String)
     struct tm *time;
     char *String;
{
  String[0]  = ((time->tm_year+1900) / 1000) % 10 + '0';
  String[1]  = ((time->tm_year+1900) / 100) % 10 + '0';
  String[2]  = (time->tm_year / 10) % 10 + '0';
  String[3]  = time->tm_year    % 10 + '0';
  String[4]  = time->tm_mon  /  9 + '0';
  String[5]  = (time->tm_mon+1) % 10 + '0';
  String[6]  = time->tm_mday / 10 + '0';
  String[7]  = time->tm_mday % 10 + '0';
  String[8]  = time->tm_hour / 10 + '0';
  String[9]  = time->tm_hour % 10 + '0';
  String[10] = time->tm_min  / 10 + '0';
  String[11] = time->tm_min  % 10 + '0';
  String[12] = time->tm_sec  / 10 + '0';
  String[13] = time->tm_sec  % 10 + '0';
  String[14] = '\0';
  String[15] = '\0';
  String[16] = '\0';
  String[17] = '\0';
  String[18] = '\0';
  String[19] = '\0';
}



/**********************************************************************
			  Buffered I/O Code
The following code is used for buffered I/O
/**********************************************************************/
#define	CONSTANTS		-1

#define integer_var(P)					\
  {							\
    deref_ptr(P);					\
    if (!IsInt(*P)) {					\
      if (IsVar(*P)) {					\
	sus_tbl_add(P);					\
      }							\
      return(False);					\
    }							\
  }

#define writable_var(P)					\
  {							\
    deref_ptr(P);					\
    if (!IsWrt(*P)) {					\
      return(False);					\
    }							\
  }

#define set_true(P)					\
    asgn(*P, P, Word(0, IntTag))


/**********************************************************************
  b_open File Number Return:
	  Number	Reason
	     0		invalid access requested
	    <0		system error number
	    >0		not an error, but the file number
/**********************************************************************/
int
b_open(T, Arg_Cnt)
     heapP	T;
     int	Arg_Cnt;
{
  heapP Name     = T,
	Access   = T+1,
	FileDesc = T+2;


  int	fd, open_flags;

  if (Arg_Cnt != 3) {
    return(False);
  }
  deref_ptr(Name);
  deref_ptr(Access);
  writable_var(FileDesc);
  switch ( *((char *) (Access+2)) )
    {
    case 'a':
      open_flags = O_WRONLY | O_CREAT | O_APPEND;
      break;
    case 'r':
      open_flags = O_RDONLY;
      break;
    case 'w':
      open_flags = O_WRONLY | O_CREAT | O_TRUNC;
      break;
    default:
      asgn(*FileDesc, FileDesc, Word(0, IntTag));
      return(True);
    }
  errno = 0;
  fd = open((char *) (Name+2), open_flags, 0666);
  if (fd <= 0) {
    asgn(*FileDesc, FileDesc, Word(-errno, IntTag));
  }
  else {
    asgn(*FileDesc, FileDesc, Word(fd, IntTag));
  }
  return(True);
}


int
b_close(T, Arg_Cnt)
     heapP	T;
     int	Arg_Cnt;
{
  heapP	Fd = T;

  if (Arg_Cnt != 1) {
    return(False);
  }
  deref_ptr(Fd);
  close(Int_Val(*Fd));
  return(True);
}  


/**********************************************************************
	File I/O Status Return:
	   Status	Reason
	   ------	------
	     >0		system error number
	     0		normal return - no error
	    -1		eof
  	    -2		unexpected eof
	    -3		invalid data type in file
	    -4		invalid data type in write request
/**********************************************************************/

int
b_read(T, Arg_Cnt)
     heapP	T;
     int	Arg_Cnt;
{
  heapP	Fd = T,
	DataType = T+1,
	DataItem = T+2,
	St  = T+3;

  if (Arg_Cnt != 4) {
    return(False);
  }
  integer_var(Fd);
  integer_var(DataType);
  writable_var(DataItem);
  writable_var(St);

  if (Int_Val(*DataType) == CONSTANTS) {
    return(read_constant(Int_Val(*Fd), DataItem, St));
  }
  else {
    return(read_characters(Int_Val(*Fd), Int_Val(*DataType), DataItem, St));
  }
}



int
read_characters(Fd, Eol, DataItem, Status)
     int	Fd, Eol;
     heapP	DataItem, Status;
{
  if ( (Eol >= 0)  && (Eol <= 255) ) 
    return(read_by_character(Fd, Eol, DataItem, Status));
  else
    return(read_entire_file(Fd, DataItem, Status));
}



int
read_entire_file(Fd, DataItem, Status)
     int	Fd;
     heapP	DataItem, Status;
{
  int	cc, len=0;
  char	*cHP = (char *) (HP+2);

  do {
    if (ended_heap(cHP+BUFSIZ)) {
      if ((off_t) 0 != lseek(Fd, (off_t) 0, SEEK_SET)) {
	cc = -1;
	break;
      }
      err_tbl_add(MACHINE, ErHPSPACE);
      return(False);
    }
    cc = read(Fd, cHP, BUFSIZ);
    cHP += cc;
    len += cc;
  }  while(cc > 0);
  asgn(*DataItem, DataItem, Ref_Word(HP));
  *HP++ = Str_Hdr1(CharType, 0);
  *HP   = Str_Hdr2(hash((char *) (HP + 1), len), len);
  HP   += 2 + (len + sizeof(heapT) - 1) / sizeof(heapT);
  if (cc < 0) {
    asgn(*Status, Status, Word(errno, IntTag));
  }
  else {
    asgn(*Status, Status, Word(-1, IntTag));
  }
  return(True);
}



int
read_by_character(Fd, Eol, DataItem, Status)
     int	Fd, Eol;
     heapP	DataItem, Status;
{
  int	len,
	eof = 0;
  off_t	current_offset;
  char	*cHP = (char *) (HP+2);

  if ( (current_offset = lseek(Fd, (off_t) 0, SEEK_CUR)) < 0 ) {
    asgn(*Status, Status, Word(errno, IntTag));
    return(True);
  }
  if (ended_heap(HP + BUFSIZ + 2)) {	/* check for minimum heap */
    err_tbl_add(MACHINE, ErHPSPACE);
    return(False);
  }
  do {
    if (ended_heap(cHP)) {
      if (current_offset != lseek(Fd, current_offset, SEEK_SET)) {
	asgn(*Status, Status, Word(errno, IntTag));
	break;
      }
      err_tbl_add(MACHINE, ErHPSPACE);
      return(False);
    }
    if (read(Fd, cHP, 1) != 1) {
      eof = 1;
      break;
    }
  } while (*cHP++ != (char) Eol);
  len = cHP - (char *) (HP+2);
  asgn(*DataItem, DataItem, Ref_Word(HP));
  *HP++ = Str_Hdr1(CharType, 0);
  *HP   = Str_Hdr2(hash((char *) (HP + 1), len), len);
  HP   += 2 + (len + sizeof(heapT) - 1) / sizeof(heapT);

  if (eof & (len == 0)) {
    asgn(*Status, Status, Word(-1, IntTag));
  }
  else {
    set_true(Status);
  }
  return(True);
}



int
read_constant(Fd, DataItem, Status)
     int	Fd;
     heapP	DataItem, Status;
{
  heapT	val_hdr[2];
  if (read(Fd, val_hdr, sizeof(heapT)) != sizeof(heapT)) {
    asgn(*Status, Status, Word(-1, IntTag));
    return(True);
  }
  switch(Tag_of(*val_hdr))
    {
    case IntTag:
      asgn(*DataItem, DataItem, Word(Int_Val(*val_hdr), IntTag));
      set_true(Status);
      break;
    case RealTag:
      asgn(*DataItem, DataItem, Ref_Word(HP));
      *HP++ = Word(0,RealTag);
      if (read(Fd, HP, sizeof(realT)) != sizeof(realT)) {
	asgn(*Status, Status, Word(-2, IntTag));
	return(True);
      }
      HP += realTbits/heapTbits;
      break;
    case StrTag:
      {
	int strlen;

	if (read(Fd, val_hdr+1, sizeof(heapT)) != sizeof(heapT)) {
	  asgn(*Status, Status, Word(-2, IntTag));
	  return(True);
	}
	strlen = (Str_Length(val_hdr) + sizeof(heapT) - 1) / sizeof(heapT);
	if (ended_heap(HP+strlen+2)) {
	  err_tbl_add(MACHINE, ErHPSPACE);
	  return(False);
	}
	if (read(Fd, HP+2, strlen * sizeof(heapT)) != strlen * sizeof(heapT)) {
	  asgn(*Status, Status, Word(-2, IntTag));
	  return(True);
	}
	asgn(*DataItem, DataItem, Ref_Word(HP));
	*HP++ = Str_Hdr1(Str_Type(val_hdr), Str_Offset(val_hdr));
	*HP   = Str_Hdr2(hash((char *) (HP+1), Str_Length(val_hdr)), 
			 Str_Length(val_hdr));
	HP   += strlen + 1;
	set_true(Status);
	break;
      }
    case NilTag:
      asgn(*DataItem, DataItem, Word(0, NilTag));
      set_true(Status);
      break;
    default:
      asgn(*Status, Status, Word(-3, IntTag));
    }
  return(True);
}


int
b_write(T, Arg_Cnt)
     heapP	T;
     int	Arg_Cnt;
{
  heapP	Fd = T,
	DataType = T+1,
	DataItem = T+2,
	Status  = T+3;

  if (Arg_Cnt != 4) {
    return(False);
  }
  integer_var(Fd);
  integer_var(DataType);
  deref_ptr(DataItem);
  writable_var(Status);
  if (IsVar(*DataItem)) {
    sus_tbl_add(DataItem);
    return(False);
  }
  writable_var(Status);

  if (Int_Val(*DataType) == CONSTANTS) {
    write_constant(Int_Val(*Fd), DataItem, Status);
  }
  else {
    char eol = Int_Val(*DataType);
    if (write_var(Int_Val(*Fd), DataItem) >= 0) {
      if ( (Int_Val(*DataType) >= 0)  &&  (Int_Val(*DataType) <= 255) ) {
	int i = write(Int_Val(*Fd), &eol, 1);
      }
      set_true(Status);
    }
    else {
      if (errno == 0)
	errno = -2;			/*** unexpected eof ***/
      asgn(*Status, Status, Word(errno, IntTag));
    }
  }
  return(True);
}


write_constant(Fd, DataItem, Status)
     int	Fd;
     heapP	DataItem,
		Status;
{
  int len=0;

  switch(Tag_of(*DataItem))
    {
    case IntTag:
      len = 1;
      break;
    case RealTag:
      len = 1;
      break;
    case StrTag:
      len = 2 + (Str_Length(DataItem) + sizeof(heapT) - 1) / sizeof(heapT);
      break;
    case NilTag:
      len = 1;
      break;
    default:
      asgn(*Status, Status, Word(-4, IntTag));
      return(True);
    }
  len = len * sizeof(heapT);
  if (len != write(Fd, DataItem, len)) {
    asgn(*Status, Status, Word(errno, IntTag));
  }
  else {
    if (Tag_of(*DataItem) == RealTag) {
      realT Real = real_val((DataItem+1));

      if (sizeof(realT) != write(Fd, &Real, sizeof(realT))) {
	/***
	  perror("Write Failed");
	/***/
	asgn(*Status, Status, Word(errno, IntTag));
      }
      else
	set_true(Status);
    }
    else
      set_true(Status);
  }
}



write_var(Fd, DataItem)
     int	Fd;
     heapP	DataItem;
{
  char cb[80];
  char *cp = cb;

  deref_ptr(DataItem);
  *cb = '\0';

#ifdef	INCLUDE_COMPOUND_TERMS
  if IsList(*DataItem) {
    write(Fd, "[", 1);
    write_list(Fd, DataItem);
    strcpy(cb,"]");
  }
  else
#endif

    switch (Tag_of(*DataItem))
      {
      case IntTag:
	sprintf(cb, "%d", Int_Val(*DataItem));
	break;
      case RealTag:
	sprintf(cb, "%f", real_val((DataItem+1)));
	break;
      case StrTag:
	cp = (char *) (DataItem+2);
	break;
      case NilTag:
	strcpy(cb, "[]");
	break;

#ifdef	INCLUDE_COMPOUND_TERMS
      case TplTag:
	write(Fd, "{", 1);
	strcpy(cb,",");
	{
	  int i;
	  heapP T = DataItem;
	  for(i=0; i < (Arity_of(*DataItem) - 1); i++) {
	    write_var(Fd, ++T);
	    write(Fd, cb, 1);
	  }
	  write_var(Fd, ++T);
	}
	strcpy(cb,"}");
	break;
#endif

      default:
	break;
      }
  {
    int  cc,clen;

    if (clen = strlen(cp))
      cc = write(Fd, cp, clen);
    return(cc);
  }
}



#ifdef	INCLUDE_COMPOUND_TERMS

write_list(Fd, DataItem)
     int	Fd;
     heapP	DataItem;
{
  int	First = 1;
  heapT	Element;

  while(IsList(*DataItem)) {
    Element = Off_List(*DataItem);
    if (First) {
      First = 0;
    }
    else {
      write(Fd, ",", 1);
    }
    write_var(Fd, Element);
    DataItem = Cdr(DataItem);
    deref_ptr(DataItem);
  }
}
#endif



#ifdef	STRING_ASGNS
asgn_string(Arg, CBuf)
     heapP	Arg;
     char	*CBuf;
{
  asgn(*Arg, Arg, Ref_Word(produce_string(CBuf)));
/***
{
  int	CBuf_Len,
        Move_Len;

  CBuf_Len = strlen(CBuf);
  Move_Len = 1 + (CBuf_Len + sizeof(heapT) - 1) / sizeof(heapT);
  if (ended_heap(HP+Move_Len+2)) {
    err_tbl_add(MACHINE, ErHPSPACE);
    return(False);
  }
  asgn(*Arg, Arg, Ref_Word(HP));
  strcpy((char *) (HP+2), CBuf);
  *HP++ = Str_Hdr1(CharType, 0);
  *HP   = Str_Hdr2(hash((char *) (HP+1), CBuf_Len), CBuf_Len);
  HP   += Move_Len+1;
/***/
}
#endif
