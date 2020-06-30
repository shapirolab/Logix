/*
** This module is part of EFCP.
**

     Copyright 2007 Yossie Lichenstein
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

   freeze_term.c - 1986

1. Arguments:

 1.1	Input is a term which may include variables, and may be infinite
	(i.e. self-referent).
 1.2	Output is a term which is a frozen (sometimes finite) view of
	the Input. Each variable is replaced by its name from the dictionary,
	or, if it is a numbered variable, by a constructed string or by the
	tuple {string,number}, depending on the argument var_form.
	A shared variable is represented by its identifier; when the
	identifier is a string, it is prefixed by an underscore ("_").
	A self-referencing term may be truncated, depending on the argument
	Switch.
 1.3 	Depth is the maximum number of levels to be traversed in Input
	(i.e. the nesting depth of tuples).
 1.4 	Length is the maximum length of list to be traversed in Input.
 1.5 	Switch determines processing of self-referent terms.
	Switch == IGNORE == 0 
	 No action is taken to eliminate infinite terms. Only the Depth
	 bound will limit traversing an infinite term.
	Switch == TRUNCATE == 1
	 Self-referent terms are truncated. A tuple indicating the self-
	 reference depth (i.e. the depth of the reoccuring father) replaces
	 the self refernce.
	Switch == ISOMORPHIC == 2
	 A self-referent Output is produced if the Input is self-referent.
	 That is, the Output structure is isomorphic to the Input structure.
 1.6	var_form determines the format of numbered variables names.
	var_form == ANONYMOUS == 0
	 All variables are represented by String_for_VAR or String_for_Ro,
	 without regard to the dictionary, depending on whether they are
	 writable or read-only.
	var_form == NAMED == 1
	 The name is a string.  This is the dictionary ident for named
	 variables, while for variables identified by a number it is
	 constructed from the String_for_VAR, followed by the digits of
	 the number.  The string is followed by '?' if the variable is
	 referenced read-only.
	var_form == PARSED == 2
	 The name is a tuple whose functor is String_for_VAR or String_for_RO,
	 according to whether the variable reference is writable or read-only.
	 The second argument of the tuple is the ident assigned to the variable
	 in the dictionary.
 1.7	Dictionary is a var or a list of tuples of this form {var,name}.
	Name can be an integer or a string.
	The dictionary is represented in the FCP level as a difference-list,
	so Dictionary is actually the head of this list.
	Freeze is done by searching Dictionary.
 1.8	Tail is the tail of the dictionary (i.e. it is a var which is the 
	last list-cell on the Dictionary. When updateing the dictionary
	we append new entries to this Tail.
 1.9	New_Tail is the tail of the updated dictionary.
 1.10	Added is a variable which is instanced to an integer representing 
	the number of new elements added to the dictionary, while this call
	to freeze_term.
 1.11	Next is the number which will be given to the next variable to be 
	freezed.
 1.12
	String_for_VAR
	String_for_RO
	String_for_DEPTH
	String_for_INFINITE
	These four arguments are the string that the kernel uses to announce
	a variable, a read-only variable, truncated at depth/length limits and
	truncated infinite terms.
	This strings ought to be defined by the FCP user.

2. Methods

 2.1	Traversing a term is done by the convert functions.
	Input is accessed as a parameter to this functions, while the output
	term is built directly on the heap (*HP).
 2.2	Freezing is done in three stages:
     1.	The existing dictionary is read and put into the hash-table
	(by initialize_dictionary and install_var_from_dictionary).
	In this manner we go to the dictionary only once, for a term.
	In later stages we will use the hash table to access quickly
	the dictionary entries.
	If a variable in the dictionary is instantiated (tag is not VarTag)
	we do not put it in the hash table.
     2.	While traversing the Input we may encounter a variable; then we
	search for it in the dictionary's hash table. If not found we add
	the variable. In both cases we return its identifier, which rep-
	resents the variable in the output.  If this identifier is a number -
	the output may be either a string or a tuple, depending on var_form.
     3.	When finished (traversing Input) we add new entries (in the hash-
	table dictionary) to the FCP dictionary-list.
	We 'remember' the place where new entries were allocated, and
	we use this pointer to go on the new entries and add them to 
	Dictionary's tail. (in update_dictionary).
 2.3	Truncating self reference:
	We keep current path on the term tree in a hash-table. If we
	encounter a tuple or list which is on the kept path, we conclude
	that we are in a circular term. Then we behave as was requested
	by the Switch argument.
 2.4	Hashing:
	We handle two hash-tables, one for freezing variables, the other
	for recognizing self-referent terms. These tables are disjoint 
	(two different rows in a matrix). The functions for hashing are
	the same and a parameter differentiates between the two tables.   

*/

#include	<stdio.h>
#include	<errno.h>

#include	"fcp.h"
#include	"codes.h"
#include	"global.h"
#include	"macros.h"

extern FILE *DbgFile;

#define TERMS	0
#define VARS	1

#define buf_size	500

#define HASHSIZE	50

#define OVERFLOW	Word(0, NilTag)

#define	IGNORE		0
#define	TRUNCATE	1

#define	ANONYMOUS	0
#define	NAMED		1
#define	PARSED		2

#define CONTINUE	0
#define QUIT		1

struct hash_entry {
  int	 id;
  int	 data;
  int	 data2;
  struct hash_entry *next;
};

struct	hash_entry buf[VARS+1][buf_size];
struct	hash_entry *hashtab[VARS+1][HASHSIZE];
int	Hash_buf_ptr[VARS+1];

int	Next;
int	New_Elements_in_Dictionary;
int	Path_ptr;
int	MaxTupl, MaxList, depth, length;
int	Truncate_self_ref;
int	var_form;

heapP	String_for_INFINITE, String_for_VAR, String_for_RO, String_for_DEPTH;

/****************************************************************************/

freeze_term(T)
     heapP T;
{
  int	new_entries;
  heapP	original_HP;
  heapP	Input  = ++T;
  heapP	Output = ++T;
  heapP	Depth  = ++T;
  heapP	Length = ++T;
  heapP	Switch	= ++T;
  heapP	Dictionary = ++T;
  heapP	Tail = ++T;
  heapP	New_Tail = ++T;
  heapP	Added = ++T;
  heapP	NEXT  = ++T;
  heapP	variable_format	= ++T;

  String_for_VAR	=	++T; 
  String_for_RO		=	++T; 
  String_for_DEPTH	=	++T;
  String_for_INFINITE	=	++T;

  deref_ptr(Input);
  deref_ptr(Output);
  deref_ptr(Depth);
  deref_ptr(Length);
  deref_ptr(Switch);
  deref_ptr(Dictionary);
  deref_ptr(Tail);
  deref_ptr(New_Tail);
  deref_ptr(Added);
  deref_ptr(NEXT);
  deref_ptr(variable_format);
  deref_ptr(String_for_VAR);
  deref_ptr(String_for_RO);
  deref_ptr(String_for_INFINITE);

  MaxTupl = Int_Val(*Depth);
  MaxList = Int_Val(*Length);
  Next = Int_Val(*NEXT);
  Truncate_self_ref = Int_Val(*Switch);
  var_form = Int_Val(*variable_format);
  depth = 0;
  length = 0;
  Path_ptr = 0;

  initialize_lists();
  initialize_dictionary(Dictionary);
  original_HP = HP;
  if (convert(Input, False) == QUIT) {
    return(False);
  }
  asgn(*Output, Output, Ref_Word(original_HP));

  asgn(*Tail, Tail, Ref_Word(HP));
  update_dictionary(New_Tail,&new_entries);
  asgn(*Added, Added, Word(new_entries, IntTag));
  return(True);

} /* freeze_term */

/****************************************************************************/

convert(Input, OUTPORT)
heapP	Input;
int	OUTPORT;
{
  register heapP P = Input;
  int	f,reply;
  heapP shared_info();
  int shared;

  reply = CONTINUE;
  deref_ptr(P);
  switch Tag_of(*P) { 
  case WrtTag:
    if (var_form != ANONYMOUS) {
      if ((shared = is_shared(P))) {
	f = *Id_SVR(shared_info(P));
      }
      else {
	if (!IsZeroed(*P)) {

	  P = Var_Val(*P);
	}
	f = freeze_var(P);
      }
      if (f != OVERFLOW) {
	if (var_form == NAMED || OUTPORT) {
	  named_var((heapT) f, False, shared, OUTPORT);
	}
	else { /* var_form = PARSED or variable is shared */
	  *HP++ = Word(2,TplTag);
	  *HP++ = Ref_Word(String_for_VAR);
	  *HP++ = (heapT) f;
	}
	break;
      }	
      /* variable table overflow */
      var_form = ANONYMOUS;
    }
    *HP++ = Ref_Word(String_for_VAR);
    break;
  case RoTag:
    if (var_form != ANONYMOUS) {
      if ((shared = is_shared_ro(P))) {
	f = *Id_SVR(shared_info(P));
      }
      else {
	f = freeze_var(P);
      }
      if (f != OVERFLOW) {
	if (var_form == NAMED) {
	  named_var((heapT) f, True, shared, False);
	}
	else { /* var_form = PARSED */
	  *HP++ = Word(2,TplTag);
	  *HP++ = Ref_Word(String_for_RO );
	  *HP++ = (heapT) f;
	}
	break;
      }	
      /* variable table overflow */
      var_form = ANONYMOUS;
    }
    *HP++ = Ref_Word(String_for_RO );
    break;
  case IntTag:   
    *HP++ = *P;
    break;
  case RealTag:
  case StrTag:  
    *HP++ = Ref_Word(P); 
    break;
  case VctrTag:
    if (var_form == NAMED) {
      reply = convert_tuple(P, True);
      break;
    }
  case NilTag:
    *HP++ = *P;
    break;
  case TplTag:
    reply = convert_tuple(P, False);
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
    reply = convert_list(P);
    break;
  case L_NilTag:
  case L_IntTag:
    if ((length < MaxList)) {
      length++;
      *HP++ = *P;
      reply = convert(++P, False);
    }
    else {
      *HP++ = Ref_Word(String_for_DEPTH);
    }
    break;
  case InvldTag:
    *HP++ = *P;
    reply = convert(++P, False);
    break;
  default:
    *HP++ = Ref_Word(P);
    break;
  }  /* switch */

  if (ended_heap(HP) && (reply == CONTINUE)) {
    err_tbl_add(MACHINE, ErHPSPACE);
    reply = QUIT;
  }
  return(reply);

} /* convert */

/****************************************************************************/

named_var(ft, RO, SHARED, OUTPORT)
heapT	ft;
int	RO;
int	SHARED;
int	OUTPORT;
{
  int	plen,slen,snx,int_val,convert;
  char	*sp,*pp;
  char	sn[15];
  heapP	fp;

  if ((convert = IsInt(ft)) && !SHARED) {
    int_val = Int_Val(ft);
    plen = Str_Length(String_for_VAR);
    pp = (char *)(String_for_VAR + 2);
  }
  else { /* Tag_of(ft) == RefTag ; ft should be a reference to a string */
    if (RO || SHARED || OUTPORT) {
      fp = NULL;
      deref(ft, fp);
      if (IsStr(ft)) {	/* Necessarily, for NAMED */
	plen = Str_Length(fp);
	pp = (char *)(fp + 2);
      }
      else {		/* Unusual shared variable id */
	if (RO) {
	  pp = "SHARED_RO";
	  RO = False;
	}
	else {
	  pp = "SHARED_WE";
	}
	plen = 9;
	*HP++ = Word(2,TplTag);
	*HP++ = Ref_Word(HP+2);
	if (fp == NULL) {
	  *HP++ = ft;
	}
	else {
	  *HP++ = Ref_Word(fp);
	}
      }
    }
    else {
      *HP++ = ft;
      return;
    }
  }
  fp = HP;
  *HP++ = Ref_Word(++fp);
  sp = (char *)(fp + 2);

  slen = 0;
  if (SHARED) {
    (*sp++) = '_';
    slen++;
  }
  while (plen-- > 0 && *pp != '\0' && (!ended_heap(sp))) {
    (*sp++) = (*pp++);
    slen++;
  }

  snx = 0;
  if (RO) {
    sn[snx++] = '?';
  }
  else if (OUTPORT) {
    sn[snx++] = '!';
  }
  if (convert) {
    do {
      sn[snx++] = (int_val % 10) + '0';
    }
    while ((int_val /= 10) > 0);
  }
  slen += snx;

  do {
    (*sp++) = sn[--snx];
  }
  while (snx > 0);
  
  *HP++ = Str_Hdr1(CharType, 0);
  *HP++ = Str_Hdr2(hash((char *) (fp+2), slen), slen);
  HP += Str_Words(fp);
  while (sp < (char *) HP) {
    *sp++ = '\0';
  }

} /* named_var */

/****************************************************************************/

convert_list(Input)
heapP	Input;
{
  int	already_in_list, distance, reply;
  heapP	father;

  if (Truncate_self_ref == IGNORE) {
    reply = convert_list_args(Input);
  }
  else {
    already_in_list = term_in_list(Input,&distance,&father);
    if (already_in_list) {
      if (Truncate_self_ref == TRUNCATE) {
	*HP++ = Word(2,TplTag);
	*HP++ = Ref_Word(String_for_INFINITE);
	*HP++ = Word(distance, IntTag);
      }
      else {
	*HP++ = Ref_Word(father);
      }
      reply = CONTINUE;
    }
    else {
      reply = convert_list_args(Input);
      remove_term_from_list(Input);
    }
  } /* Don't IGNORE Truncate_self_ref */
  return(reply);

} /* convert_list */

/****************************************************************************/

convert_list_args(Input)
heapP	Input;
{
  heapP	tp;
  int	reply;

  if ((length < MaxList)) {
    length++;
    tp = HP+2;
    *HP++ = L_Ref_Word(tp);
    tp = HP++;
    reply = convert(L_Ref_Val(*Input), False);
    if (reply != QUIT) {
      *tp = Ref_Word(HP);
      reply = convert(++Input, False);
      length--;
    }
  }
  else {
    *HP++ = Ref_Word(String_for_DEPTH);
    reply = CONTINUE;
  }
  return(reply);

} /* convert_list_args */

/****************************************************************************/

convert_tuple(Input, OUTPORT)
heapP Input;
int   OUTPORT;
{
  int	distance, reply;
  heapP	father;

  if (Truncate_self_ref == IGNORE) {
    reply = convert_tuple_args(Input, OUTPORT);
  }
  else {
    if (term_in_list(Input,&distance,&father)) {
      if (Truncate_self_ref == TRUNCATE) {
	*HP++ = Word(2,TplTag);
	*HP++ = Ref_Word(String_for_INFINITE);
	*HP++ = Word(distance,IntTag);
      }
      else {
	*HP++ = Ref_Word(father);
      }
      reply = CONTINUE;
    }
    else {
      reply = convert_tuple_args(Input, OUTPORT);
      remove_term_from_list(Input);
    }
  } /* Don't IGNORE Truncate_self_ref */
  return(reply);

} /* convert_tuple */ 

convert_tuple_args(Input, OUTPORT)
     heapP Input;
{
  heapP	PTuple;
  heapP	tp;
  int	N, reply;

  N = Val_of(*Input);
  if (N > MaxTpl) {
    err_tbl_add(MACHINE, ErBADARG);
    return(QUIT);
  }
  if (heap_space((2*sizeof(heapT))) < N) {
    err_tbl_add(MACHINE, ErHPSPACE);
    return(QUIT);
  }
  if ((depth < MaxTupl)) {
    depth++;
    *HP++ = Word(N,TplTag);
    tp = HP;	
    HP += N;
    while (N-- > 0) {
      *tp++ = Ref_Word(HP);
      if (convert(++Input, OUTPORT) == QUIT) {
	return(QUIT);
      }
    }
    depth--;
  }
  else	{
    *HP++ = Ref_Word(String_for_DEPTH);
  }
  return(CONTINUE);

} /* convert_tuple_args */


/****************************************************************************/

term_in_list(ref,distance,father)
     heapP	ref;
     int	*distance;
     heapP	*father;
{
  struct hash_entry *np, *hash_lookup(), *hash_install();

  if ((np = hash_lookup(TERMS,(int) ref)) == NULL) {
    np = hash_install(TERMS,(int) ref);
    Path_ptr++;
    np->data = Path_ptr;
    np->data2 = (int) HP;
    return(False);
  }
  else { 
    *distance = Path_ptr - np->data;
    *father = (heapP) np->data2;
    return(True);
  }

} /* term_in_list(ref) */


remove_term_from_list(ref)
     heapP ref;
{
  struct hash_entry *np, *p;
  int N, h;

  N = (int) ref;
  h = int_hash(N);
  for (p = np = hashtab[TERMS][h] ; np != NULL ; p = np, np = np->next) {
    if (N == np->id) {
      if (np == hashtab[TERMS][h]) {
	hashtab[TERMS][h] = hashtab[TERMS][h]->next;
      }
      else {
	p->next = np->next;
      }
      Path_ptr--;
      return(True);
    }
  }
  fprintf(DbgFile,
	  "Problems in truncating infinite terms :A term that was not\n");
  fprintf(DbgFile, " on list, was subject to remove command (freeze)\n");
  return(False);
} /* remove_term_from_list */

/****************************************************************************/

initialize_dictionary(Dictionary)
     heapP	Dictionary;
{
  int	finish = False;
  heapP	D = Dictionary;

  while (!finish) {
    deref_ptr(D);
    switch Tag_of(*D)	{
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
      install_var_from_dictionary(L_Ref_Val(*D++));
      break;
    case WrtTag:  
    case RoTag:
      /* End_of_dictionary = D;	*/
      finish = True;
      break;
    default:
      fprintf(DbgFile, " Illegal dictionary : Tag is not expected (freeze)\n");
      finish = True;
      break;
    }
  }
  New_Elements_in_Dictionary = Hash_buf_ptr[VARS];
} /* initialize_dictionary */

/****************************************************************************/

update_dictionary(New_Tail,new_entries)
heapP	New_Tail;
int	*new_entries;
{
  register int i;

  if (New_Elements_in_Dictionary == Hash_buf_ptr[VARS]) {
    *new_entries = 0;
    *HP++ = Ref_Word(New_Tail);
    return(False);
  } /* if no new elements */

  *new_entries = Hash_buf_ptr[VARS]-New_Elements_in_Dictionary;
  for (i = New_Elements_in_Dictionary ; i < Hash_buf_ptr[VARS] ; i++ ) {
    *HP = L_Ref_Word((HP+2));
    HP++;
    *HP = Ref_Word((HP+4));
    HP++;
    *HP++ = Word(2, TplTag);
    *HP++ = Ref_Word(((heapP) (buf[VARS][i].id)));
    *HP++ = (heapT) (buf[VARS][i].data);
  } /* for */
  *HP++ = Ref_Word(New_Tail);
  return(True);

} /* update_dictionary */

/****************************************************************************/

install_var_from_dictionary(Tuple)
heapP	Tuple;
{
  heapP	Var, Name;
  struct hash_entry *np, *hash_install();

  deref_ptr(Tuple);
  if (!IsTpl(*Tuple)) {
    fprintf(DbgFile, " Illegal dictionary Tag (freeze_term) \n ");
    return(False);
  }
  if (Val_of(*Tuple) != 2) {
    fprintf(DbgFile,
	    " Illegal dictionary tuple argument count (freeze_term) \n ");
    return(False);
  }
  Var = Tuple + 1;
  deref_ptr(Var);
  if (IsWrt(*Var)) {
    if (!IsZeroed(*Var)) {
      Var = Var_Val(*Var);
    }
  }
  else {
    if (!IsRo(*Var)) {
      return(False);
    }
  }
  Name = Tuple + 2;
  deref_ptr(Name);
  if (!IsInt(*Name) && !IsStr(*Name)) {
    fprintf(DbgFile, " Illegal dictionary Name (freeze_term) \n ");
    return(False);
  }
  np = hash_install(VARS,(int) Var);
  if (np == NULL) {
    return(False);
  }
  if (IsInt(*Name)) {
    np->data = *Name;
  }
  else {
    np->data = Ref_Word(Name);
  }
  return(True);

} /* install_var_from_dictionary */

/****************************************************************************/

freeze_var(var)
heapP var;
{
  struct hash_entry *np,*hash_lookup(), *hash_install();

  if ((np = hash_lookup(VARS,(int) var)) == NULL) {
    np = hash_install(VARS,(int) var);
    if (np == NULL) {
      return(OVERFLOW);
    }
    np->data = Word(Next++, IntTag);
  }
  return(np->data);

} /* freeze_var */



/****************************************************************************/


int_hash(N)
int N;
{
  return(N % HASHSIZE);
}

struct hash_entry *hash_lookup(table,N)
int table, N;
{
  struct hash_entry *np;

  for (np = hashtab[table][int_hash(N)]; np != NULL; np = np->next) {
    if (N == np->id) {
      return(np);
    }
  }
  return(NULL);
}

struct hash_entry *hash_install(table,N)
int table, N;
{
  struct hash_entry *np;
  int hashval, ptr;

  if (Hash_buf_ptr[table] >= buf_size) { 
    if (table == VARS) {
      return(NULL);
    }
    /*  table == TERMS  */
    fprintf(DbgFile,
	    " *** Overflow. Infinite terms may not be removed (freeze) \n");
    Hash_buf_ptr[table] = 0;
  }
  ptr = Hash_buf_ptr[table]++;
  np = &buf[table][ptr];
  np->id = N;
  hashval = int_hash(np->id);
  np->next = hashtab[table][hashval];
  hashtab[table][hashval] = np;
  return(np);

} /* hash_install */

initialize_lists()
{
  register int i;

  for (i = 0; i < HASHSIZE; i++) {
    hashtab[TERMS][i] = NULL;
    hashtab[VARS][i] = NULL;
  }
  Hash_buf_ptr[TERMS] = 0;
  Hash_buf_ptr[VARS] = 0;

} /* initialize_lists */

/****************************************************************************/
