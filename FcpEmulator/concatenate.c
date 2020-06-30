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


** 
**     Free Software Foundation, Inc.
**     51 Franklin Street, Fifth Floor
**     Boston, MA 02110-1301 USA

       contact: bill@wisdom.weizmann.ac.il
*/

/*

1. Arguments :

  1.1 L is a list of strings, integers and nils. 
  1.2 S is a variable. This program concatenates all the strings (and the
      ascii representation of integers) from L and assigns S to reference 
      this string - nils are ignored.
  1.3 Blanks is the (minimum) number of blanks by which each line (segment)
      is indented.
  1.4 Line_Length is the maximum number of characters (including indentation)
      in a line. 

2. Methods

  2.1 We traverse the list term by term, and put the strings directly
      on the heap.
  2.2 We do not permit a line (segment) to be longer than the specified
      max_line_length.
      If the next token to be concatenated makes the line_length too big,
      we put a new-line, and indent the continuation line.
  2.3 In continuation lines we add extra indentation to distinguish a
      continuation line from all others.

*/


#include	<stdio.h>
#include	<errno.h>

#include	"fcp.h"
#include	"codes.h"
#include	"global.h"
#include	"macros.h"

#define indent_later 4

/**************/
char	*sp;
heapP	L;
int	line_length, max_line_length, indent, new_line;
/**************/

concatenate(T)
heapP T;
{
  heapP S, Blanks, Line_Length, PStr;
  char	*send;
  int	slen;

  L = ++T;
  deref_ptr(L);
  S = ++T;
  deref_ptr(S);
  Blanks = ++T;
  deref_ptr(Blanks);
  Line_Length = ++T;
  deref_ptr(Line_Length);
  PStr = HP;
  indent = Int_Val(*Blanks);
  max_line_length = Int_Val(*Line_Length);

/*****************************************/
  sp = (char *) (PStr + 2);
  line_length = 0;
  new_line = True;
  slen = strip_list();
  send = (char *) sp;
/****************************************/

  if (ended_heap(send))	{
    err_tbl_add(MACHINE, ErHPSPACE);
    return(False);
  }
  if (slen > MaxStr) {
    err_tbl_add(MACHINE, ErBADARG);
    return(False);
  }
  if (!IsNil(*L)) {
    if (IsInvld(*L)) {
      asgn(*S, S, Ref_Word(L));
      return(True);
    }
    else {
      err_tbl_add(MACHINE, ErBADARG);
      return(False);
    }
  }
  *HP++ = Str_Hdr1(CharType, 0);
  *HP++ = Str_Hdr2(hash((char *) (PStr+2), slen), slen);
  HP += Str_Words(PStr);
  while (send < (char *)HP) {
    *send++ = '\0';
  }
  asgn(*S, S, Ref_Word(PStr));
  return(True);
}

/*****************************************************************************/

strip_list()
{
  heapT	head;
  heapP	Term;
  int	slen=0;

  while (IsList(*L) && (!ended_heap(sp))) {
    if (new_line) {
      slen += add_blanks(indent);
    }
    head = Off_List(*L);
    deref(head, Term);
    switch Tag_of(head) {
    case IntTag:
      slen += convert_integer(Int_Val(head)); 
      break;
    case StrTag:
      slen += convert_string(Term); 
      break;
    case NilTag:
      break;
    case InvldTag:
      L = Term;
      return(0);
    default:
      return(0);
    } /* switch */
    L = Cdr(L);
    deref_ptr(L);
  }
  return(slen);
}  /* strip_list */

/*****************************************************************************/

convert_string(Term)
     heapP Term;
{
  int cs,lfin,seg1,segn,total;
  int start_line = new_line;
  char *p,*c,*sp0;

  new_line = False;
  total = Str_Length(Term);
  c = p = (char *)(Term+2);
  cs = lfin = seg1 = segn = 0;
  while ( (total-- > 0) && (*p != '\0') ) {
    if (*p++ == '\n') {
      if (segn == 0) {
	seg1 = cs;
      }
      segn = ++cs;
      if ((total > 0) && (*p != '\0') && (*p != '\n')) {
	lfin += indent;
      }
    }
    else {
      cs++;
    }
  }
  total = cs + lfin;
  if ( segn != 0 ) {
    segn = cs - segn;
  }
  else {
    seg1 = segn = cs;
  }
  sp0 = sp;
  if (((line_length + seg1) > max_line_length) && (seg1 > 0) && !start_line) { 
    *sp++ = '\n';
    line_length = add_blanks(indent+indent_later);
    total += line_length + 1; 
  }
  sp0 += total;
  if (ended_heap(sp0)) {
    sp = sp0;
    return(0);
  }
  while (cs-- > 0) {
    *sp = *c++ ;
    if (*sp++ == '\n') {
      if (cs == 0) {
	new_line = True;
      }
      else {
	if (*c != '\n') {
	  line_length = add_blanks(indent);
	}
      }
    }
  } 
  line_length += segn;
  return(total);
} /* convert_string */

/*****************************************************************************/

convert_integer(n)
int n;
{
  int i,sign,length;
  char s[15];

  if ((sign = n) < 0) {
    n = -n;
  }
  i = 0;
  do {
    s[i++] = n % 10 + '0';
  }
  while ((n /= 10) > 0);
  if (sign < 0) {
    s[i++] = '-';
  }
  line_length += length = i;
  if ((line_length > max_line_length) && !new_line) {
    *sp++ = '\n';
    line_length = add_blanks(indent+indent_later) + length;
    length = line_length + 1;
  }
  do {
    *sp++ = s[--i];
  }
  while (i > 0);
  new_line = False;
  return(length);
} /* convert_integer */

add_blanks(indentation)
     int indentation;
{
  int i;

  if (ended_heap(sp+indentation)) {
    sp += indentation;
  }
  else {
    for (i=1; i <= indentation; i++) {
      *sp++ = ' ';
    }
  }
  return(indentation);
} /* add_blanks */
