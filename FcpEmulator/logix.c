/* $Header: /home/qiana/Repository/FcpEmulator/logix.c,v 1.1.1.1 1999/07/01 07:15:10 bill Exp $ */
/*
**  logix.c  -   logix fcp emulator main program.
*/
 
#include        "fcp.h"
#include        "codes.h"
#include        "global.h"
#include        "macros.h"
 
main(argc, argv)
     int        argc;
     char       *argv[];
{
  fcp(argc, argv);
}
