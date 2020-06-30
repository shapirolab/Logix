/*
** This module is part of EFCP.
**

     Copyright 2007 Margaret Purtill, Michael Hirsch
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
	Module Name   math.c	arithmetic for real numbers

	January 1987
*/


/*
   REAL NUMBER MATH
*/


#include	<stdio.h>
#include	<errno.h>
#include	<math.h>

#include	"fcp.h"
#include	"codes.h"
#include	"global.h"
#include	"macros.h"


extern	long	random();

extern  FILE	*DbgFile;

/* Single operand functions */
#define SIN  1		/* sine function */
#define COS  2		/* cosine function */
#define TAN  3		/* tangent function */
#define RND  4		/* random seed or function */
#define ASIN 5		/* arcsine function	*/
#define	ACOS 6		/* arccosine function	*/
#define	ATAN 7		/* arctangent function	*/
#define	EXP  8		/* natural exponent	*/
#define	LN  9		/* natural logarithm	*/
#define SQRT 10		/* square-root function */

#define	DUAL 11		/* First dual operand function */

/* Dual operand functions: */

#define POW  11		/* power function       */
#define LOG 12      /* find logN(x)         */

/*
#define DEBUG_ME
*/

math(T)
heapP T;
{
  realT	Value, Value2, Result;

  heapT Operation = *(T+1);
  heapP	Argument = T+2;
  heapP	Ptr, Argument2;

  deref_val(Operation);
  deref_ptr(Argument);
  if IsReal(*Argument) {
    Value = real_val((Argument+1));
  } else {
    Value = (realT) (Int_Val(*Argument));
  }
  if(Int_Val(Operation) < DUAL) {
    Ptr = T+3;
    deref_ptr(Ptr);
#ifdef	DEBUG_ME
  fprintf(DbgFile,
	  "math: value = %f, operation = %d\n", Value, Int_Val(Operation));
  fflush(DbgFile);
#endif
  } else {
    Argument2 = T+3;
    Ptr = T+4;
    deref_ptr(Argument2);
    deref_ptr(Ptr);
    if IsReal(*Argument2) {
      Value2 = real_val((Argument2+1));
    } else {
      Value2 = (realT) (Int_Val(*Argument2));
    }
#ifdef	DEBUG_ME
  printf(DbgFile, "math: values = %f, %f, operation = %d\n",
	 Value, Value2, Int_Val(Operation));
  fflush(DbgFile);
#endif
  }
  switch(Int_Val(Operation)) { 
  case SIN:			/* sine */
    Result = sin(Value);
    break;
  case COS:			/* cosine */
    Result = cos(Value);
    break;
  case TAN:			/* tangent */
    Result = tan(Value);
    break;
  case RND:			/* random */
    if (Value == 0.0) {
      Result = (realT)((realT)random()/2147483647.0);
    } else {
      srandom((int) Value);
      Result = 0;
    }
    break;
  case SQRT:			/* square root function*/
    Result = sqrt(Value); 
    break;
  case ASIN:			/* arcsine function	*/
    Result = asin(Value);
    break;
  case ACOS:			/* arccosine function	*/
    Result = acos(Value);
    break;
  case ATAN:			/* arctangent function	*/
    Result = atan(Value);
    break;
  case EXP:			/* natural exponent	*/
    Result = exp(Value);
    break;
  case LN:			/* natural logarithm	*/
    Result = log(Value);
    break;
  case POW:			/* power operator	*/
    Result = pow(Value, Value2);
    break;
  case LOG:			/* find logN(x)         */
    Result = (log(Value) / log(Value2));
    break;
  default:
#ifdef	DEBUG_ME
    printf(DbgFile, "math illegal operation %d\x",*T);
    fflush(DbgFile);
#endif
    return(False);

  } /* switch */
  
#ifdef	DEBUG_ME
  printf(DbgFile, "math: result = %f\n",Result);
    fflush(DbgFile);
#endif
  /* Pow is unique - int, int returns an int */
  if(Int_Val(Operation) == POW
  && !IsReal(*Argument)
  && !IsReal(*Argument2)) {
    asgn(*Ptr, Ptr, Word((int)Result, IntTag));
  } else {
    asgn(*Ptr, Ptr, Ref_Word(HP));
    *HP++ = Word(0, RealTag);
    real_copy_val(Result, HP);
    HP += realTbits/heapTbits;
  }
  return(True);
}
