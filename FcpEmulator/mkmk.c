/* $Header: /home/qiana/Repository/FcpEmulator/mkmk.c,v 1.6 2002/10/09 07:09:07 bill Exp $ */

/*
** Creates makefile and static_link.h according to the following flags:
	(Note that only the initials that distinguish the flag from other
	 flags are required).

   concatenate:
     foreign function - string concatenation kernels
   ctl:
     foreign function - enables creating ctl .bin files
   dbg:
     sets DEBUG flag
   dbgl:
     sets DEBUG_LINK
   dbx:
     compile with -g
   dec:
     sets ULTRIX flag, enables running on dec workstations or intel pcs
     add cnv.c to files.
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
     builds makefile for linux kernel 2.2.12, redhat 6.1
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
       linkning
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

**
**
*/


#include	<stdio.h>
#include	<string.h>
#include	<ctype.h>
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
static char *dbgS = "-DDEBUG";
static char *dbglS = "-DDEBUG_LINK";
static char *dbxS = "-g";
static char *decS = "-DULTRIX";
static char *doorsS = "doorsfcp";
static char *fileS = "file";
static char *freeze_termS = "freeze_term";
static char *interfaceS = "interface";
static char *hpux_9d05S = "hppa1d1_hpux_9d05";
static char *linux_6d1 = "linux_2d2d12_redhat_6d1";
static char *mathS = "math";
static char *noptS = " ";
static char *optS = "-O";
static char *o1S = "-O1";
static char *o2S = "-O2";
static char *o4S = "-O4";
static char *os41S = "-Bstatic";
static char *spiS = "spicomm";
static char *spiwS = "spiweight";
static char *sgi_5d2S = "sgi_irix_5d2";
static char *solaris_2d3S = "sun4_solaris_2d3";
static char *sunos_4d1d3S = "sun4_sunos_4d1d3";
static char *timerS = "timer";
static char *ttyS = "tty";

static char *cnvV = "";
static char *concatenateV = "";
static char *ctlV = "";
static char *dbgV = "";
static char *dbglV = "";
static char *dbvV = "";
static char *dbxV = "";
static char *decV = "";
static char *doorsV = "";
static char *fileV = "";
static char *freeze_termV = "";
static char *hpux_9d05V = "";
static char *interfaceV = "";
static char *libsV = "";
static char *linux_6d1V = "";
static char *mathV = "";
static char *optV = "";
static char *o4V = "";
static char *os41V = "";
static char *spiV = "";
static char *spiwV = "";
static char *sgi_5d2V = "";
static char *solaris_2d3V = "";
static char *sunos_4d1d3V = "";
static char *timerV = "";
static char *ttyV = "";

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
	  exit();
	}
	continue;
      case 't':
	/* ctl */
	ctlV = LinkFunc[LinkFuncNum] = ctlS;
	LinkFuncNum++;
	if (LinkFuncNum == MaxLinkFunc) {
	  printf("mkmk: Too many foreign functions\n");
	  exit();
	}
	continue;
      default:
	printf("mkmk: Unknown option %s\n", *argv);
	exit();
      }
    case 'd':
      /* dbg */
      /* dbgl */
      /* dbv */
      /* dbx */
      /* dec */
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
	    exit();
	  }
	case 'x':
	  /* dbx */
	  dbxV = dbxS;
	  continue;
	default:
	  printf("mkmk: Unknown option %s\n", *argv);
	  exit();
	}
      case 'e':
	/* dec */
	decV = decS;
	cnvV = cnvS;
	continue;
      case 'o':
	/* doors */
	doorsV = LinkFunc[LinkFuncNum] = doorsS;
	LinkFuncNum++;
	if (LinkFuncNum == MaxLinkFunc) {
	  printf("mkmk: Too many foreign functions\n");
	  exit();
	}
	continue;
      default:
	printf("mkmk: Unknown option %s\n", *argv);
	exit();
      }
    case 'f':
      /* file */
      /* freeze_term */
      switch (*S++) {
      case 'i':
	fileV = LinkFunc[LinkFuncNum] = fileS;
	LinkFuncNum++;
	if (LinkFuncNum == MaxLinkFunc) {
	  printf("mkmk: Too many foreign functions\n");
	  exit();
	}
	continue;
      case 'r':
	freeze_termV = LinkFunc[LinkFuncNum] = freeze_termS;
	LinkFuncNum++;
	if (LinkFuncNum == MaxLinkFunc) {
	  printf("mkmk: Too many foreign functions\n");
	  exit();
	}
	continue;
      default:
	printf("mkmk: Unknown option %s\n", *argv);
	exit();
      }
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
	exit();
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
	  /* linux_2d2d12_redhat_6d1 */
	  linux_6d1V = linux_6d1;
	  cnvV = cnvS;
	  continue;
	default:
	  printf("mkmk: Unknown option %s\n", *argv);
	  exit();
	}
      default:
	printf("mkmk: Unknown option %s\n", *argv);
	exit();
      }
    case 'm':
      /* math */
      mathV = LinkFunc[LinkFuncNum] = mathS;
      LinkFuncNum++;
      if (LinkFuncNum == MaxLinkFunc) {
	printf("mkmk: Too many foreign functions\n");
	exit();
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
	exit();
      }
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
      case 'p':
	/* spi */
	spiV = LinkFunc[LinkFuncNum] = spiS;
	spiwV = spiwS;
	LinkFuncNum++;
	if (LinkFuncNum == MaxLinkFunc) {
	  printf("mkmk: Too many foreign functions\n");
	  exit();
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
	  exit();
	}
      default:
	printf("mkmk: Unknown option %s\n", *argv);
	exit();
      }
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
	  exit();
	}
	continue;
      case 't':
	/* tty */
	ttyV = LinkFunc[LinkFuncNum] = ttyS;
	LinkFuncNum++;
	if (LinkFuncNum == MaxLinkFunc) {
	  printf("mkmk: Too many foreign functions\n");
	  exit();
	}
	continue;
      default:
	printf("mkmk: Unknown option %s\n", *argv);
	exit();
      }
    default:
      printf("mkmk: Unknown option %c\n", *argv);
      exit();
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
  fprintf(MakeFd, "SHELL = /bin/csh\n");
  fprintf(MakeFd, "GCC = gcc\n");
  Pos = fprintf(MakeFd, "CFLAGS = -c");
  Pos = cond_print(MakeFd, dbgV, "", "", Pos);
  Pos = cond_print(MakeFd, dbglV, "", "", Pos);
  Pos = cond_print(MakeFd, dbvV, "", "", Pos);
  Pos = cond_print(MakeFd, dbxV, "", "", Pos);
  Pos = cond_print(MakeFd, decV, "", "", Pos);

  if (strcmp(hpux_9d05V, NullS) != 0) {
    Pos = cond_print(MakeFd, "-DHPUX", "", "", Pos);
  }
  if (strcmp(linux_6d1V, NullS) != 0) {
    Pos = cond_print(MakeFd, "-DLINUX", "", "", Pos);
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
    Pos = cond_print(MakeFd, "-lm", "", "", Pos);
  }
  if (strcmp(libsV, NullS) != 0) {
    Pos = cond_print(MakeFd, libsV, "", "", Pos);
  }
  Pos = cond_print(MakeFd, "-lc", "", "", Pos);

  fprintf(MakeFd, "\n\n");

  fprintf(MakeFd, "fcp: \\\n");
  fprintf(MakeFd, "	$(OFILES)\n");
  fprintf(MakeFd, "	$(GCC)");
  cond_print(MakeFd, os41V, "", "", Pos);
  fprintf(MakeFd, " $(OFILES) $(LIBS) -o fcp\n");
  fprintf(MakeFd, "\n");

  if (strcmp(cnvV, NullS) != 0) {
    fprintf(MakeFd, "cnv.o: \\\n");
    fprintf(MakeFd, "	$(BASICH) emulate.h opcodes.h\n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(LOWOPT) $(INCLUDES) cnv.c\n");
  }

  if (strcmp(concatenateV, NullS) != 0) {
    fprintf(MakeFd, "concatenate.o: \\\n");
    fprintf(MakeFd, "	$(BASICH)\n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) concatenate.c\n");
  }

  if (strcmp(ctlV, NullS) != 0) {
    fprintf(MakeFd, "ctl.o: \\\n");
    fprintf(MakeFd, "	$(BASICH)\n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) ctl.c\n");
  }

  fprintf(MakeFd, "dist.o: \\\n");
  fprintf(MakeFd, "	$(BASICH)\n");
  fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) dist.c\n");

  if (strcmp(doorsV, NullS) != 0) {
    fprintf(MakeFd, "doorsfcp.o: \\\n");
    fprintf(MakeFd, "	$(BASICH) doorsfcp.h\n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) doorsfcp.c\n");

    fprintf(MakeFd, "drsfcptc.o: \\\n");
    fprintf(MakeFd, "	$(BASICH) doorsfcp.h doorsvar.h\n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) drsfcptc.c\n");

    fprintf(MakeFd, "drsctfcp.o: \\\n");
    fprintf(MakeFd, "	$(BASICH) doorsfcp.h doorsvar.h\n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) drsctfcp.c\n");
  }

  fprintf(MakeFd, "emulate.o : \\\n");
  fprintf(MakeFd, "	$(BASICH) opcodes.h emulate.h\n");
  fprintf(MakeFd,
	  "	$(GCC) $(CFLAGS) $(LOWOPT) $(INCLUDES) emulate.c\n");

  fprintf(MakeFd, "externs.o : \\\n");
  fprintf(MakeFd, "	$(BASICH) opcodes.h\n");
  fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) externs.c\n");

  fprintf(MakeFd, "fcp.o : \\\n");
  fprintf(MakeFd, "	$(BASICH) emulate.h\n");
  fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) fcp.c\n");

  if (strcmp(fileV, NullS) != 0) {
    fprintf(MakeFd, "file.o: \\\n");
    fprintf(MakeFd, "	$(BASICH)\n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) file.c\n");
  }

  if (strcmp(freeze_termV, NullS) != 0) {
    fprintf(MakeFd, "freeze_term.o: \\\n");
    fprintf(MakeFd, "	$(BASICH)\n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) freeze_term.c\n");
  }

  fprintf(MakeFd, "freezer.o : \\\n");
  fprintf(MakeFd, "	$(BASICH) emulate.h\n");
  fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) freezer.c\n");

  fprintf(MakeFd, "global.o : \\\n");
  fprintf(MakeFd, "	$(BASICH) emulate.h\n");
  fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) global.c\n");

  fprintf(MakeFd, "heap.o : \\\n");
  fprintf(MakeFd, "	$(BASICH) emulate.h\n");
  fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) heap.c\n");

  if (strcmp(interfaceV, NullS) != 0) {
    fprintf(MakeFd, "interface.o: \\\n");
    fprintf(MakeFd, "	$(BASICH)\n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) interface.c\n");
  }

  fprintf(MakeFd, "kernels.o : \\\n");
  fprintf(MakeFd, "	$(BASICH) emulate.h\n");
  fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) kernels.c\n");

  fprintf(MakeFd, "link_static.o: \\\n");
  fprintf(MakeFd, "	$(BASICH) link_static.h\n");
  fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) link_static.c\n");

  fprintf(MakeFd, "logix.o : \\\n");
  fprintf(MakeFd, "	$(BASICH) emulate.h\n");
  fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) logix.c\n");

  if (strcmp(mathV, NullS) != 0) {
    fprintf(MakeFd, "math.o: \\\n");
    fprintf(MakeFd, "	$(BASICH)\n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) math.c\n");
  }

  fprintf(MakeFd, "notify.o : \\\n");
  fprintf(MakeFd, "	$(BASICH)\n");
  fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) notify.c\n");

  if (strcmp(spiV, NullS) != 0) {
    fprintf(MakeFd, "spicomm.o: \\\n");
    fprintf(MakeFd, "	$(BASICH)\n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) spicomm.c\n");
  }

  if (strcmp(spiwV, NullS) != 0) {
    fprintf(MakeFd, "spiweight.o: \\\n");
    fprintf(MakeFd, "	$(BASICH)\n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) spiweight.c\n");
  }

  fprintf(MakeFd, "streams.o : \\\n");
  fprintf(MakeFd, "	$(BASICH) emulate.h\n");
  fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) streams.c\n");

  if (strcmp(timerV, NullS) != 0) {
    fprintf(MakeFd, "timer.o: \\\n");
    fprintf(MakeFd, "	$(BASICH)\n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) timer.c\n");
  }

  if (strcmp(ttyV, NullS) != 0) {
    fprintf(MakeFd, "tty.o: \\\n");
    fprintf(MakeFd, "	$(BASICH)\n");
    fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) tty.c\n");
  }

  fprintf(MakeFd, "unify.o : \\\n");
  fprintf(MakeFd, "	$(BASICH) emulate.h unify.h\n");
  fprintf(MakeFd, "	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) unify.c\n");

  fprintf(MakeFd, "utility.o : \\\n");
  fprintf(MakeFd, "	$(BASICH)\n");
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

      
