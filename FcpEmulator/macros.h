/* $Header: /home/qiana/Repository/FcpEmulator/macros.h,v 1.1 1999/07/01 07:15:09 bill Exp $ */
/*
**  macros.h  -  macros
*/

/* Derefrencing */

#define deref(V, P) \
{ \
  while (IsRef(V)) { \
    P = Ref_Val(V); \
    V = *P; \
  } \
}

#define deref_val(V) \
{ \
  while (IsRef(V)) { \
    V = *(Ref_Val(V)); \
  } \
}

#define deref_ptr(P) \
{ \
  while (IsRef(*P)) { \
    P = Ref_Val(*P); \
  } \
}

#define deref_ref(V, P) \
{ \
  do { \
    P = Ref_Val(V); \
    V = *P; \
  } \
  while (IsRef(V)); \
}

/* Trails */

#define trls_overflow_check(Ptr0, Ptr1) \
{ \
  if ((Ptr0 + Trls_TH) >= Ptr1) { \
    do_exit("", MACHINE, ErTRLSOVFL, False); \
  } \
}

#define	asgn_trl_reset() \
{ \
  AsgnTRP = AsgnTrl; \
}

#define	chng_trl_reset() \
{ \
  ChngTRP = ChngTrl; \
}

#define asgn_trl_add(OldVal, Ptr) \
{ \
  trls_overflow_check(AsgnTRP, ChngTRP); \
  (++AsgnTRP)->Value = OldVal; \
  AsgnTRP->Address = Ptr; \
}

#define chng_trl_add(OldVal, Ptr) \
{ \
  trls_overflow_check(AsgnTRP, ChngTRP); \
  (--ChngTRP)->Value = OldVal; \
  ChngTRP->Address = Ptr; \
}

#define	asgn(OldVal, Ptr, NewVal) \
{ \
  trls_overflow_check(AsgnTRP, ChngTRP); \
  (++AsgnTRP)->Value = OldVal; \
  AsgnTRP->Address = Ptr; \
  *Ptr = NewVal; \
}

#define	chng(OldVal, Ptr, NewVal) \
{ \
  trls_overflow_check(AsgnTRP, ChngTRP); \
  (--ChngTRP)->Value = OldVal; \
  ChngTRP->Address = Ptr; \
  *Ptr = NewVal; \
}

#define	asgn_trl_undo(OldAsgnTRP) \
{ \
  while (AsgnTRP != OldAsgnTRP) { \
    *((AsgnTRP)->Address) = AsgnTRP->Value; \
    AsgnTRP--; \
  } \
}

#define	chng_trl_undo(OldChngTRP) \
{ \
  while (ChngTRP != OldChngTRP) { \
    *((ChngTRP)->Address) = ChngTRP->Value; \
    ChngTRP++; \
  } \
}

/* Tables */

#define tbls_overflow_check(Ptr0, Ptr1) \
{ \
  if ((Ptr0 + Tbls_TH) >= Ptr1) { \
    do_exit("", MACHINE, ErTBLSOVFL, False); \
  } \
}

/* Suspension Table */

#ifdef	DEBUG
#define sus_tbl_reset() \
{ \
  PrevSTP = STP = SusTbl; \
}
#else
#define sus_tbl_reset() \
{ \
  STP = SusTbl; \
}
#endif
    
#define sus_tbl_add(Address) \
{ \
  if (Address < HB) { \
    *(++STP) = (heapT) Address; \
  } \
  else { \
    if (IsRo(*(Address)) && (Var_Val(*(Address)) < HB)) { \
      *(++STP) = (heapT) Var_Val(*(Address)); \
    } \
    else { \
      TempSus = True; \
    } \
  } \
}

/* Errors Table */

#define err_tbl_reset() \
{ \
  ERRP = ErrTbl; \
}
    
#define err_tbl_add(Type, Value) \
{ \
  *(--ERRP) = (heapT) Value; \
  *(--ERRP) = (heapT) Type; \
}


#define	char_offset(X, O)	(((char *) (X)) + (O))

#define to_word_boundry(CodeP) \
{ \
  CodeP = (opcodeP) char_offset(CodeP, (((int) CodeP) & 0x2)); \
}

/* Convertor macros */

/* switch a double */
#define cnv_d(P) \
{ \
  register char	*P_Chars = (char *) (P); \
  register char	Char; \
\
  Char = P_Chars[0]; \
  P_Chars[0] = P_Chars[7]; \
  P_Chars[7] = Char; \
  Char = P_Chars[1]; \
  P_Chars[1] = P_Chars[6]; \
  P_Chars[6] = Char; \
  Char = P_Chars[2]; \
  P_Chars[2] = P_Chars[5]; \
  P_Chars[5] = Char; \
  Char = P_Chars[3]; \
  P_Chars[3] = P_Chars[4]; \
  P_Chars[4] = Char; \
}

/* switch a word */
#define cnv_w(P) \
{ \
  register char	*P_Chars = (char *) (P); \
  register char	Char; \
\
  Char = P_Chars[0]; \
  P_Chars[0] = P_Chars[3]; \
  P_Chars[3] = Char; \
  Char = P_Chars[1]; \
  P_Chars[1] = P_Chars[2]; \
  P_Chars[2] = Char; \
}

/* switch a short */
#define cnv_s(P) \
{ \
  register char	*P_Chars = (char *) (P); \
  register char	Char; \
\
  Char = P_Chars[0]; \
  P_Chars[0] = P_Chars[1]; \
  P_Chars[1] = Char; \
}
