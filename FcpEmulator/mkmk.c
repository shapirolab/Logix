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
** Creates makefile and static_link.h according to the following flags:
	(Note that only the initials that distinguish the flag from other
	 flags are required).

   arch:
     architecture type
   concatenate:
     foreign function - string concatenation kernels
   ctl:
     foreign function - enables creating ctl .bin files
   cygwin:
     builds makefile for cywin kernel CYGWIN_NT-5.1
   dbg:
     sets DEBUG flag
   dbgl:
     sets DEBUG_LINK
   dbx:
     compile with -g
   doors:
     foreign function - doors api.
   file:
     foreign function - file interface
   freeze_term:
     foreign function - term freezing as used for screen displaying
   hppa1d1_hpux_9d05:
     build makefile for hpux 9.05
   interface:
     foreign function - operating system interface
   linux:
     builds makefile for linux redhat 6.1 or ubuntu
   long:
     build version for 64-bit word platform
   maci386:
     builds makefile for architecture i386 BSD kernel
   macosx:
     builds makefile for powerpc BSD kernel
   math:
     foreign function - mathematical functions
   opt:
     compiles with -O flag
   o2:
     compiles with -O2 flag for all files except for emulate.c
   o4:
     compiles with -O4 flag for all files except for emulate.c
   os4.1:
     links with -Bstatic as required by certain computers to enable dynamic
       linking
   posix:
     sets linkage flags, etc. for Posix (compatible) systems
   short:
     build version for 32-bit word platform (default)
   spi:
     foreign function - SpiFcp requests
   sgi_irix_5d2:
     builds makefile for Silicon Graphics Irix 5.2
   sun4_solaris_2d3:
     builds makefile for solaris 2.3
   sun4_sunos_4d1d3:
     builds makefile for sunos 4.1.3
   timer:
     foreign function - timer requests
   tty:
     foreign function - tty handler
   ultrix:
     sets ULTRIX flag, enables running on intel pcs
     add cnv.c to files.

**
**
*/

extern void exit(int);

#include	<stdio.h>
#include	<string.h>
#include	<sys/file.h>

#define	True	1
#define	False   0

#define LineLength	75
#define	TabSize		 8

#define MaxLinkFunc	20

static char *NullS = "";

static char *cnvS = "cnv";
static char *concatenateS = "concatenate";
static char *ctlS = "ctl";
static char *cygwin_nt_5d1S = "cygwin_nt-5d1";
static char *dbgS = "-DDEBUG";
static char *dbglS = "-DDEBUG_LINK";
static char *dbxS = "-g";
static char *doorsS = "doorsfcp";
static char *fileS = "file";
static char *freeze_termS = "freeze_term";
static char *interfaceS = "interface";
static char *hpux_9d05S = "hppa1d1_hpux_9d05";
static char *linux_variantD = "linux";
static char *longS = "-m64";
static char *longwordS = "-DLONGWORD";
static char *maci386S = "maci386_BSD";
static char *macosx_8d4S = "macosx_BSD_8d4";
static char *mathS = "math";
static char *noptS = " ";
static char *optS = "-O";
static char *o1S = "-O1";
static char *o2S = "-O2";
static char *o4S = "-O4";
static char *os41S = "-Bstatic";
static char *posixS = "POSIX";
static char *spiS = "spicomm";
static char *spiwS = "spiweight";
static char *sgi_5d2S = "sgi_irix_5d2";
static char *shortS = "-m32";
static char *shortwordS = "";
static char *solaris_2d3S = "sun4_solaris_2d3";
static char *sunos_4d1d3S = "sun4_sunos_4d1d3";
static char *timerS = "timer";
static char *ttyS = "tty";
static char *ultrixS = "-DULTRIX";

static char *archV = "";
static char *cnvV = "";
static char *concatenateV = "";
static char *ctlV = "";
static char *cygwin_nt_5d1V = "";
static char *dbgV = "";
static char *dbglV = "";
static char *dbvV = "";
static char *dbxV = "";
static char *doorsV = "";
static char *fileV = "";
static char *freeze_termV = "";
static char *hpux_9d05V = "";
static char *interfaceV = "";
static char *libsV = "";
static char *linux_variantV = "";
static char *longV = "";
static char *longwordV = "";
static char *maci386V = "";
static char *macosx_8d4V = "";
static char *mathV = "";
static char *optV = "";
static char *o4V = "";
static char *os41V = "";
static char *posixV = "";
static char *spiV = "";
static char *spiwV = "";
static char *sgi_5d2V = "";
static char *solaris_2d3V = "";
static char *sunos_4d1d3V = "";
static char *timerV = "";
static char *ttyV = "";
static char *ultrixV = "";

main(argc, argv)
     int	argc;
     char	*argv[];
{
  char *LinkFunc[MaxLinkFunc];

  int LinkFuncNum = 0;
  FILE *MakeFd = fopen("makefile", "w");
  FILE *LinkFd = fopen("link_static.h", "w");
  int Aborted = False;
  register char *S;
  register int I, Pos;
	
  if (MakeFd == NULL) {
    printf("Can not open \"makefile\" for writing\n");
    exit(1);
  }
  if (LinkFd == NULL) {
    printf("Can not open \"link_static.h\" for writing\n");
    exit(2);
  }
		
  while (--argc > 0 && !Aborted) {
    argv++;
    S = argv[0];
    switch (*S++) {
    case 'c':
      /* concatenate */
      /* ctl */
      switch (*S++) {
      case 'o':
	/* concatenate */
	concatenateV = LinkFunc[LinkFuncNum] = concatenateS;
	LinkFuncNum++;
	if (LinkFuncNum == MaxLinkFunc) {
	  printf("mkmk: Too many foreign functions\n");
	  exit(1);
	}
	continue;
      case 't':
	/* ctl */
	ctlV = LinkFunc[LinkFuncNum] = ctlS;
	LinkFuncNum++;
	if (LinkFuncNum == MaxLinkFunc) {
	  printf("mkmk: Too many foreign functions\n");
	  exit(1);
	}
	continue;
	case 'y':
	  /* CYGWIN_NT-5.1 */
	  cygwin_nt_5d1V = cygwin_nt_5d1S;
	  cnvV = cnvS;
	  continue;
      default:
	printf("mkmk: Unknown option %s\n", *argv);
	exit(1);
      }
      continue;
    case 'd':
      /* dbg */
      /* dbgl */
      /* dbv */
      /* dbx */
      /* doors */
      switch (*S++) {
      case 'b':
	/* dbg */
	/* dbgl */
	/* dbv */
	/* dbx */
	switch (*S++) {
	case 'g':
	  /* dbg */
	  /* dbgl */
	  /* dbv */
	  switch (*S++) {
	  case '\0':
	    /* dbg */
	    dbgV = dbgS;
	    continue;
	  case 'l':
	    /* dbgl */
	    dbglV = dbglS;
	    continue;
	  default:
	    printf("mkmk: Unknown option %s\n", *argv);
	    exit(1);
	  }
	  continue;
	case 'x':
	  /* dbx */
	  dbxV = dbxS;
	  continue;
	default:
	  printf("mkmk: Unknown option %s\n", *argv);
	  exit(1);
	}
	continue;
      case 'o':
	/* doors */
# if defined DOORSOK
	doorsV = LinkFunc[LinkFuncNum] = doorsS;
	LinkFuncNum++;
	if (LinkFuncNum == MaxLinkFunc) {
	  printf("mkmk: Too many foreign functions\n");
	  exit(1);
	}
#else
	printf("mkmk: doors is not currently implemented\n");
#endif
	continue;
      default:
	printf("mkmk: Unknown option %s\n", *argv);
	exit(1);
      }
      continue;
    case 'f':
      /* file */
      /* freeze_term */
      switch (*S++) {
      case 'i':
	fileV = LinkFunc[LinkFuncNum] = fileS;
	LinkFuncNum++;
	if (LinkFuncNum == MaxLinkFunc) {
	  printf("mkmk: Too many foreign functions\n");
	  exit(1);
	}
	continue;
      case 'r':
	freeze_termV = LinkFunc[LinkFuncNum] = freeze_termS;
	LinkFuncNum++;
	if (LinkFuncNum == MaxLinkFunc) {
	  printf("mkmk: Too many foreign functions\n");
	  exit(1);
	}
	continue;
      default:
	printf("mkmk: Unknown option %s\n", *argv);
	exit(1);
      }
      continue;
    case 'h':
      /* hppa1d1_hpux_9d05 */
      hpux_9d05V = hpux_9d05S;
      continue;
    case 'i':
      /* interface */
      interfaceV = LinkFunc[LinkFuncNum] = interfaceS;
      LinkFuncNum++;
      if (LinkFuncNum == MaxLinkFunc) {
	printf("mkmk: Too many foreign functions\n");
	exit(1);
      }
      continue;
    case 'l':
      switch (*S++) {
      case 'i':
	switch (*S++) {
	case 'b':
	  /* libnsl */
	  libsV = "-lnsl";
	  continue;
	case 'n':
	  /* linux */
	  linux_variantV = linux_variantD;
	  cnvV = cnvS;
	  continue;
	default:
	  printf("mkmk: Unknown option %s\n", *argv);
	  exit(1);
	}
	continue;
      case 'o':
	/* long */
	longV = longS;
	longwordV = longwordS;
	continue;
      default:
	printf("mkmk: Unknown option %s\n", *argv);
	exit(1);
      }
      continue;
    case 'm':
      if (*S++ == 'a') {
	switch (*S++) {
	case 'c': {
	  switch (*S++) {
	  case ('i'):
	    /* maci386 */
	    maci386V = maci386S;
	    archV = "-arch i386 ";
	    continue;
	  case ('o'):
	    /* macosx */
	    macosx_8d4V = macosx_8d4S;
	    archV = "-arch ppc ";
	    continue;
	  default:
	    printf("mkmk: Unknown option %s\n", *argv);
	    exit(1);
	  }
	  continue;
	}
	case 't':
	  /* math */
	  mathV = LinkFunc[LinkFuncNum] = mathS;
	  LinkFuncNum++;
	  if (LinkFuncNum == MaxLinkFunc) {
	    printf("mkmk: Too many foreign functions\n");
	    exit(1);
	  }
	  continue;
	default:
	  printf("mkmk: Unknown option %s\n", *argv);
	  exit(1);
	}
      }
      else {
	printf("mkmk: Unknown option %s\n", *argv);
	exit(1);
      }
      continue;
    case 'o':
      /* opt */
      /* os4.1 */
      /* o2 */
      /* o4 */
      switch (*S++) {
      case 'p':
	/* opt */
	optV = optS;
	o4V = optS;
	continue;
      case 's':
	/* os4.1 */
	os41V = os41S;
	continue;
      case '2':
	/* o2 */
	optV = o2S;
	o4V = noptS;
	continue;
      case '4':
	/* o4 */
	optV = o4S;
	o4V = o1S;
	continue;
      default:
	printf("mkmk: Unknown option %s\n", *argv);
	exit(1);
      }
      continue;
    case 'p':
      /* posix */
      posixV = posixS;
      continue;
    case 'r':
      /* redhat */
      linux_variantV = linux_variantD;
      cnvV = cnvS;
      continue;
    case 's':
      /* sgi_irix_5d2 */
      /* sun4_solaris_2d3 */
      /* sun4_sunos_4d1d3 */
      /* spi */
      switch (*S++) {
      case 'g':
	/* sgi_irix_5d2 */
	sgi_5d2V = sgi_5d2S;
	continue;
      case 'h':
	/* short */
	longV = shortS;
	longwordV = shortwordS;
	continue;
      case 'p':
	/* spi */
	spiV = LinkFunc[LinkFuncNum] = spiS;
	spiwV = spiwS;
	LinkFuncNum++;
	if (LinkFuncNum == MaxLinkFunc) {
	  printf("mkmk: Too many foreign functions\n");
	  exit(1);
	}
	continue;
      case 'u':
	/* sun4_solaris_2d3 */
	/* sun4_sunos_4d1d3 */
	switch (*(S+4)) {
	case 'o':
	  /* sun4_solaris_2d3 */
	  solaris_2d3V = solaris_2d3S;
	  continue;
	case 'u':
	  /* sun4_sunos_4d1d3 */
	  sunos_4d1d3V = sunos_4d1d3S;
	  continue;
	default:
	  printf("mkmk: Unknown option %s\n", *argv);
	  exit(1);
	}
	continue;
      default:
	printf("mkmk: Unknown option %s\n", *argv);
	exit(1);
      }
      continue;
    case 't':
      /* timer */
      /* tty */
      switch (*S++) {
      case 'i':
	/* timer */
	timerV = LinkFunc[LinkFuncNum] = timerS;
	LinkFuncNum++;
	if (LinkFuncNum == MaxLinkFunc) {
	  printf("mkmk: Too many foreign functions\n");
	  exit(1);
	}
	continue;
      case 't':
	/* tty */
	ttyV = LinkFunc[LinkFuncNum] = ttyS;
	LinkFuncNum++;
	if (LinkFuncNum == MaxLinkFunc) {
	  printf("mkmk: Too many foreign functions\n");
	  exit(1);
	}
	continue;
      default:
	printf("mkmk: Unknown option %s\n", *argv);
	exit(1);
      }
      continue;
    case 'u':
      switch (*S++) {
      case 'b':
	/* ubuntu */
	linux_variantV = linux_variantD;
	cnvV = cnvS;
	continue;
      case 'l':
	/* ultrix */
	ultrixV = ultrixS;
	cnvV = cnvS;
	continue;
      default:
	printf("mkmk: Unknown option %s\n", *argv);
	exit(1);
      }
      continue;
    default:
      printf("mkmk: Unknown option %s\n", *argv);
      exit(1);
    }
  }

  if (LinkFuncNum > 0) {
    fprintf(LinkFd, "#define	NUM_FUNCTIONS	%d\n", LinkFuncNum);
    fprintf(LinkFd, "\n");
    fprintf(LinkFd, "extern int\n");
    for (I = 0; I < LinkFuncNum; I++) {
      fprintf(LinkFd, "	%s()", LinkFunc[I]);
      if ((I+1) < LinkFuncNum) {
	fprintf(LinkFd, ",\n");
      }
    }
    fprintf(LinkFd, ";\n\n");
    fprintf(LinkFd, "static char *FuncName[NUM_FUNCTIONS] = {\n");
    for (I = 0; I < LinkFuncNum; I++) {
      fprintf(LinkFd, "	\"%s.o\"", LinkFunc[I]);
      if ((I+1) < LinkFuncNum) {
	fprintf(LinkFd, ",\n");
      }
    }
    fprintf(LinkFd, "};\n\n");
    fprintf(LinkFd, "static int (*Func[NUM_FUNCTIONS])() = {\n");
    for (I = 0; I < LinkFuncNum; I++) {
      fprintf(LinkFd, "	%s", LinkFunc[I]);
      if ((I+1) < LinkFuncNum) {
	fprintf(LinkFd, ",\n");
      }
    }
    fprintf(LinkFd, "};\n");
  }
  fclose(LinkFd);

  fprintf(MakeFd, "# fcp make\n");
  fprintf(MakeFd, "SHELL = /bin/sh\n");
  fprintf(MakeFd, "GCC = gcc %s%s\n", archV, longV);
  Pos = fprintf(MakeFd, "CFLAGS = -c");
  Pos = cond_print(MakeFd, longwordV, "", "", Pos);
  Pos = cond_print(MakeFd, dbgV, "", "", Pos);
  Pos = cond_print(MakeFd, dbglV, "", "", Pos);
  Pos = cond_print(MakeFd, dbvV, "", "", Pos);
  Pos = cond_print(MakeFd, dbxV, "", "", Pos);
  Pos = cond_print(MakeFd, ultrixV, "", "", Pos);

  if (strcmp(cygwin_nt_5d1V, NullS) != 0) {
    Pos = cond_print(MakeFd, "-DCYGWIN", "", "", Pos);
  }
  if (strcmp(hpux_9d05V, NullS) != 0) {
    Pos = cond_print(MakeFd, "-DHPUX", "", "", Pos);
  }
  if (strcmp(linux_variantV, NullS) != 0) {
    Pos = cond_print(MakeFd, "-DLINUX", "", "", Pos);
  }
  if (strcmp(maci386V, NullS) != 0) {
    Pos = cond_print(MakeFd, "-DMACINTOSH", "", "", Pos);
  }
  if (strcmp(macosx_8d4V, NullS) != 0) {
    Pos = cond_print(MakeFd, "-DMACINTOSH", "", "", Pos);
  }
  if (strcmp(sgi_5d2V, NullS) != 0) {
    Pos = cond_print(MakeFd, "-DSGI", "", "", Pos);
  }
  if (strcmp(solaris_2d3V, NullS) != 0) {
    Pos = cond_print(MakeFd, "-DSUNOS5d3", "", "", Pos);
  }
  if (strcmp(sunos_4d1d3V, NullS) != 0) {
    Pos = cond_print(MakeFd, "-DSUNOS4d1d3", "", "", Pos);
  }

  fprintf(MakeFd, "\n");
  if (strcmp(optV, NullS) != 0) {
    Pos = fprintf(MakeFd, "OPT =");
    Pos = cond_print(MakeFd, optV, "", "", Pos);
    fprintf(MakeFd, "\n");
    Pos = fprintf(MakeFd, "LOWOPT =");
    Pos = cond_print(MakeFd, o4V, "", "", Pos);
    fprintf(MakeFd, "\n");
  }
  fprintf(MakeFd, "\n");
  Pos = fprintf(MakeFd, "INCLUDES = -I.");
  if (strcmp(doorsV, NullS) != 0) {
    Pos = cond_print(MakeFd, "-I/zamir/users/alon/ubique/inc", "", "", Pos);
    Pos = cond_print(MakeFd, "-I/zamir/users/alon/ubique/lib/doors", "", "",
		     Pos);
  }
  fprintf(MakeFd, "\n\n");
  fprintf(MakeFd, "BASICH = fcp.h codes.h global.h macros.h\n");
  /* Basic O Files */
  Pos = fprintf(MakeFd, "OFILES =");
  Pos = cond_print(MakeFd, cnvV, "", ".o", Pos);
  Pos = cond_print(MakeFd, concatenateV, "", ".o", Pos);
  Pos = cond_print(MakeFd, ctlV, "", ".o", Pos);
  Pos = cond_print(MakeFd, "dist", "", ".o", Pos);
  if (strcmp(doorsV, NullS) != 0) {
    Pos = cond_print(MakeFd, "doorsfcp", "", ".o", Pos);
    Pos = cond_print(MakeFd, "drsfcptc", "", ".o", Pos);
    Pos = cond_print(MakeFd, "drsctfcp", "", ".o", Pos);
  }
  Pos = cond_print(MakeFd, "emulate", "", ".o", Pos);
  Pos = cond_print(MakeFd, "externs", "", ".o", Pos);
  Pos = cond_print(MakeFd, "fcp", "", ".o", Pos);
  Pos = cond_print(MakeFd, fileV, "", ".o", Pos);
  Pos = cond_print(MakeFd, freeze_termV, "", ".o", Pos);
  Pos = cond_print(MakeFd, "freezer", "", ".o", Pos);
  Pos = cond_print(MakeFd, "global", "", ".o", Pos);
  Pos = cond_print(MakeFd, "heap", "", ".o", Pos);
  Pos = cond_print(MakeFd, interfaceV, "", ".o", Pos);
  Pos = cond_print(MakeFd, "kernels", "", ".o", Pos);
  Pos = cond_print(MakeFd, "link_static", "", ".o", Pos);
  Pos = cond_print(MakeFd, "logix", "", ".o", Pos);
  Pos = cond_print(MakeFd, mathV, "", ".o", Pos);
  Pos = cond_print(MakeFd, "notify", "", ".o", Pos);
  Pos = cond_print(MakeFd, spiV, "", ".o", Pos);
  Pos = cond_print(MakeFd, spiwV, "", ".o", Pos);
  Pos = cond_print(MakeFd, "streams", "", ".o", Pos);
  Pos = cond_print(MakeFd, timerV, "", ".o", Pos);
  Pos = cond_print(MakeFd, ttyV, "", ".o", Pos);
  Pos = cond_print(MakeFd, "unify", "", ".o", Pos);
  Pos = cond_print(MakeFd, "utility", "", ".o", Pos);
  fprintf(MakeFd, "\n\n");
  
  fprintf(MakeFd, "\n");
  if (strcmp(doorsV, NullS) != 0) {
    Pos = fprintf(MakeFd, "UBIQUE_ARC =");
    if (strcmp(hpux_9d05V, NullS) != 0) {
      Pos = cond_print(MakeFd,
		       "/zamir/users/alon/ubique/hppa1d1_hpux_9d05/arc",
		       "", "", Pos);
    }
    if (strcmp(sgi_5d2V, NullS) != 0) {
      Pos = cond_print(MakeFd,
		       "/zamir/users/alon/ubique/sgi_irix_5d2/arc",
		       "", "", Pos);
    }
    if (strcmp(sunos_4d1d3V, NullS) != 0) {
      Pos = cond_print(MakeFd,
		       "/zamir/users/alon/ubique/sun4_sunos_4d1d3/arc",
		       "", "", Pos);
    }
    if (strcmp(solaris_2d3V, NullS) != 0) {
      Pos = cond_print(MakeFd,
		       "/zamir/users/alon/ubique/sun4_solaris_2d3/arc",
		       "", "", Pos);
    }
    fprintf(MakeFd, "\n\n");
  }
  Pos = fprintf(MakeFd, "LIBS   =");
  if (strcmp(doorsV, NullS) != 0) {
    Pos = cond_print(MakeFd, "${UBIQUE_ARC}/libdoors.a", "", "", Pos);
    Pos = cond_print(MakeFd, "${UBIQUE_ARC}/libdcm.a", "", "", Pos);
    Pos = cond_print(MakeFd, "${UBIQUE_ARC}/libtoken.a", "", "", Pos);
    Pos = cond_print(MakeFd, "${UBIQUE_ARC}/libutil.a", "", "", Pos);
    if (strcmp(solaris_2d3V, NullS) != 0) {
      Pos = cond_print(MakeFd, "-lsocket -lnsl -lgen -liberty", "", "", Pos);
    }
    if (strcmp(hpux_9d05V, NullS) != 0) {
      Pos = cond_print(MakeFd, "-liberty", "", "", Pos);
    }
  }
  if (strcmp(mathV, NullS) != 0) {
    if (strcmp(posixV, posixS) != 0) {
      Pos = cond_print(MakeFd, "-lm", "", "", Pos);
    }
    else {
      Pos = cond_print(MakeFd, "-l m", "", "", Pos);
    }
  }
  if (strcmp(libsV, NullS) != 0) {
    Pos = cond_print(MakeFd, libsV, "", "", Pos);
  }
  if (strcmp(posixV, posixS) != 0) {
    Pos = cond_print(MakeFd, "-lc", "", "", Pos);
  }
  else {
    Pos = cond_print(MakeFd, "-l c", "", "", Pos);
  }

  fprintf(MakeFd, "\n\n");

  fprintf(MakeFd, "fcp: \\\n");
  fprintf(MakeFd, "	$(OFILES)\n");
  fprintf(MakeFd, "	$(GCC)");
  cond_print(MakeFd, os41V, "", "", Pos);
  fprintf(MakeFd, " $(OFILES) $(LIBS) -o fcp\n");
  fprintf(MakeFd, "\n");

  if (strcmp(cnvV, NullS) != 0) {
    fprintf(MakeFd, "cnv.o: \\\n");
    fprintf(MakeFd, "	$(BASICH) emulate.h opcodes.h cnv.c \n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(LOWOPT) $(INCLUDES) cnv.c\n");
  }

  if (strcmp(concatenateV, NullS) != 0) {
    fprintf(MakeFd, "concatenate.o: \\\n");
    fprintf(MakeFd, "	$(BASICH) concatenate.c\n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) concatenate.c\n");
  }

  if (strcmp(ctlV, NullS) != 0) {
    fprintf(MakeFd, "ctl.o: \\\n");
    fprintf(MakeFd, "	$(BASICH) ctl.c\n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) ctl.c\n");
  }

  fprintf(MakeFd, "dist.o: \\\n");
  fprintf(MakeFd, "	$(BASICH) dist.c\n");
  fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) dist.c\n");

  if (strcmp(doorsV, NullS) != 0) {
    fprintf(MakeFd, "doorsfcp.o: \\\n");
    fprintf(MakeFd, "	$(BASICH) doorsfcp.h doorsfcp.c\n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) doorsfcp.c\n");

    fprintf(MakeFd, "drsfcptc.o: \\\n");
    fprintf(MakeFd, "	$(BASICH) doorsfcp.h doorsvar.h drsfcptc.c\n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) drsfcptc.c\n");

    fprintf(MakeFd, "drsctfcp.o: \\\n");
    fprintf(MakeFd, "	$(BASICH) doorsfcp.h doorsvar.h drsctfcp.c\n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) drsctfcp.c\n");
  }

  fprintf(MakeFd, "emulate.o : \\\n");
  fprintf(MakeFd, "	$(BASICH) opcodes.h emulate.h emulate.c\n");
  fprintf(MakeFd,
	  "	$(GCC) $(CFLAGS) $(LOWOPT) $(INCLUDES) emulate.c\n");

  fprintf(MakeFd, "externs.o : \\\n");
  fprintf(MakeFd, "	$(BASICH) opcodes.h externs.c\n");
  fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) externs.c\n");

  fprintf(MakeFd, "fcp.o : \\\n");
  fprintf(MakeFd, "	$(BASICH) emulate.h fcp.c\n");
  fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) fcp.c\n");

  if (strcmp(fileV, NullS) != 0) {
    fprintf(MakeFd, "file.o: \\\n");
    fprintf(MakeFd, "	$(BASICH) file.c\n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) file.c\n");
  }

  if (strcmp(freeze_termV, NullS) != 0) {
    fprintf(MakeFd, "freeze_term.o: \\\n");
    fprintf(MakeFd, "	$(BASICH) freeze_term.c\n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) freeze_term.c\n");
  }

  fprintf(MakeFd, "freezer.o : \\\n");
  fprintf(MakeFd, "	$(BASICH) emulate.h freezer.c\n");
  fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) freezer.c\n");

  fprintf(MakeFd, "global.o : \\\n");
  fprintf(MakeFd, "	$(BASICH) emulate.h global.c\n");
  fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) global.c\n");

  fprintf(MakeFd, "heap.o : \\\n");
  fprintf(MakeFd, "	$(BASICH) emulate.h heap.c\n");
  fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) heap.c\n");

  if (strcmp(interfaceV, NullS) != 0) {
    fprintf(MakeFd, "interface.o: \\\n");
    fprintf(MakeFd, "	$(BASICH) interface.c\n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) interface.c\n");
  }

  fprintf(MakeFd, "kernels.o : \\\n");
  fprintf(MakeFd, "	$(BASICH) emulate.h kernels.c\n");
  fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) kernels.c\n");

  fprintf(MakeFd, "link_static.o: \\\n");
  fprintf(MakeFd, "	$(BASICH) link_static.h link_static.c\n");
  fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) link_static.c\n");

  fprintf(MakeFd, "logix.o : \\\n");
  fprintf(MakeFd, "	$(BASICH) emulate.h logix.c\n");
  fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) logix.c\n");

  if (strcmp(mathV, NullS) != 0) {
    fprintf(MakeFd, "math.o: \\\n");
    fprintf(MakeFd, "	$(BASICH) math.c\n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) math.c\n");
  }

  fprintf(MakeFd, "notify.o : \\\n");
  fprintf(MakeFd, "	$(BASICH) notify.c\n");
  fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) notify.c\n");

  if (strcmp(spiV, NullS) != 0) {
    fprintf(MakeFd, "spicomm.o: \\\n");
    fprintf(MakeFd, "	$(BASICH) spicomm.c\n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) spicomm.c\n");
  }

  if (strcmp(spiwV, NullS) != 0) {
    fprintf(MakeFd, "spiweight.o: \\\n");
    fprintf(MakeFd, "	$(BASICH) spiweight.c\n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) spiweight.c\n");
  }

  fprintf(MakeFd, "streams.o : \\\n");
  fprintf(MakeFd, "	$(BASICH) emulate.h streams.c\n");
  fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) streams.c\n");

  if (strcmp(timerV, NullS) != 0) {
    fprintf(MakeFd, "timer.o: \\\n");
    fprintf(MakeFd, "	$(BASICH) timer.c\n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) timer.c\n");
  }

  if (strcmp(ttyV, NullS) != 0) {
    fprintf(MakeFd, "tty.o: \\\n");
    fprintf(MakeFd, "	$(BASICH) tty.c\n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) tty.c\n");
  }

  fprintf(MakeFd, "unify.o : \\\n");
  fprintf(MakeFd, "	$(BASICH) emulate.h unify.h unify.c\n");
  fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) unify.c\n");

  fprintf(MakeFd, "utility.o : \\\n");
  fprintf(MakeFd, "	$(BASICH) utility.c\n");
  fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) utility.c\n");

  fprintf(MakeFd, "\n");
  fclose(MakeFd);
}

int
cond_print(FileFd, String, Prefix, Suffix, Position)
     FILE *FileFd;
     char *String, *Prefix, *Suffix;
     int Position;
{
  if (strcmp(String, NullS) != 0) {
    if ((Position + strlen(Prefix) + strlen(String) + strlen(Suffix) + 3) >
	LineLength) {
      fprintf(FileFd, " \\\n");
      Position = TabSize * fprintf(FileFd, "	");
    }
    Position += fprintf(FileFd, " %s%s%s", Prefix, String, Suffix);
    if (Position >= LineLength) {
      fprintf(FileFd, "\\\n");
      Position = TabSize * fprintf(FileFd, "	");
    }
  }
  return(Position);
}

      
