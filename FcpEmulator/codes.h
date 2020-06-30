/*
** This module is part of EFCP.
**

   Copyright 2007 Avraham Houri
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
**	codes.h	 -  operation codes
*/

/*
** Events Codes
*/

#define	PauseC		0
#define	ResumeC		1
#define	ExitC		2

/*
** Constants Numbers
*/

/*
** Machine Output Codes.
** (Must be first since they are used for masking)
*/

#define FailedC	 	 0
#define GCC	 	 1
#define IdleC	 	 2
#define BootC	 	 3

/*
** Message Constants
*/

#define UnknownC	 4
#define DoneC		 5
#define FalseC		 6
#define UnboundC	 7
#define ErrorC		 8

/*
** Types.
*/

#define NORMAL	  	 0
				/* No string corresponding to NORMAL! */
#define MACHINE		 9
#define SIGNAL	  	10
#define SYSTEM	  	11

/*
** Machine Error Codes.
*/

/* Heap Overflow */
#define ErHPOVFL	12

/* Trails Overflow */
#define ErTRLSOVFL	13

/* Tabels Overflow */
#define ErTBLSOVFL	14

/* No Heap Space */
#define ErHPSPACE	15

/* No Link Space */
#define ErLINKSPACE	16

/* Undefined Code */
#define ErUDFDCD	17

/* Invalid Data */
#define ErINVLDOBJ	18

/* Bad Argument */
#define ErBADARG	19

/* Exit Requested */
#define ErEXITREQ	20

/* Not Open */
#define ErNOTOPN	21

/* No Device Space*/
#define ErNODEVSPC	22

/* Not Found */
#define ErNOTFND	23

/*
** Distributed Implementation Messages
*/

#define WeC		24
#define RoC		25
#define BothC		26
#define	BindingC	27
#define ValueC		28
#define CommitC		29
#define LockC		30
#define Id_QueryC	31
#define	IdentifyC	32
#define ReferenceC	33
#define KnownC		34
#define SharedC		35
#define ActiveC		36
#define InActiveC	37
#define EmptyC		38
#define Non_EmptyC	39
#define LocalC		40
#define RemoteC		41
#define RequestedC	42
#define UnRequestedC	43
#define ReferencedC	44
#define UnReferencedC	45
#define AtomicC		46
#define NonAtomicC	47
#define	IncrementalC	48
#define ExportedC	49
#define GarbageC	50
#define MultipleC	55
#define ExcessC		56
#define DoorsErrorC	57
#define DoorsEventC	58
#define DoorsRequestC	59
#define DoorsResponseC	60

/*
** Devices Messages/Codes
*/

#define ReadC		51
#define WriteC		52
#define ExceptC		53
#define SignalC		54

/*
** Number of Constants
*/

#define	ConstSize	61
