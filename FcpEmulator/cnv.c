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

#include	"codes.h"
#include  "opcodes.h"
#include	"fcp.h"
#include	"global.h"
#include	"macros.h"
#include	"emulate.h"

/* Import convertor */

int
imp_convert(PString)                    /* main, convert module */
     register heapP PString;
{
  register heapP PStrEnd;

  cnv_w(PString);
  cnv_w((PString+1));
  if (!IsStr(*PString)) {
    err_tbl_add(MACHINE, ErBADARG);
    return(False);
  }
  PStrEnd = PString + 2 + Str_Words(PString);
  return(imp_cnv(PString, PStrEnd));
}

int
imp_cnv(PString, PEnd)
     heapP PString, PEnd;
{
  if (!IsStr(*PString) || ((PString + 2 + Str_Words(PString)) > PEnd)) {
    err_tbl_add(MACHINE, ErBADARG);
    return(False);
  }
  switch (Str_Type(PString)) {
  case CharType:
    return(True);
  case PrcdrType:
    return(imp_cnv_prcd(PString));
  case MdlType:
    return(imp_cnv_mdl(PString));
  case FrznType:
    return(imp_cnv_frzn(PString));
  case CtlType:
    return(imp_cnv_ctl(PString));
  default:
    err_tbl_add(MACHINE, ErBADARG);
    return(False);
  }
}

imp_cnv_prcd(PString)            /* convert procedure */
     heapP PString;
{
  register heapP P;
  register int StrWords = Str_Words(PString);
  heapP POffset = PString+2, PLimit;

  cnv_w(POffset);
  P = POffset + (int) *POffset; 
  POffset++;
  cnv_w(POffset);
  if (((int) *POffset) != 0) {
    PLimit = POffset + (int) *POffset;
    cnv_w(PLimit); /* arity */
  }
  else {
    PLimit = PString + 2 + StrWords;
  }
  for (; P < PLimit;) { /* data area */
    cnv_w(P++);
  }
  return(True);
}

imp_cnv_mdl(PString)              /* module */
     heapP PString;
{
  register heapP P = PString+2;
  register int StrWords = Str_Words(PString);
  heapP POffset, PLimit;

  POffset = P++;
  cnv_w(POffset)
  if (((int) *POffset) != 0) { /* Info Offset */
    register heapP P0 = POffset + (int) *POffset;
    cnv_w(P0);
    cnv_w(P0+1);
    if (IsStr((*P0))) { /* Ctl */
      PLimit = PString + 2 + StrWords - 1 /* Null Word */;
    }
    else {
      PLimit = POffset + (int) *POffset;
    }
    cnv_w(P0);
    cnv_w(P0+1);
  }
  else {
    PLimit = PString + 2 + StrWords;
  }
  for (; P < PLimit; P += 2 + StrWords) {
    cnv_w(P);
    cnv_w(P+1);
    StrWords = Str_Words(P);
    if (!imp_cnv(P, PLimit)) {
      return(False);
    }
  }
  return(True);
}

imp_cnv_frzn(PString)           /* frozen */
     heapP PString;
{
  register heapP P = PString+2;
  heapP PLimit = P + Str_Words(PString);

  while (P < PLimit) {
    cnv_w(P);
    switch (Tag_of(*P++)) {
    case Tag(0x00, RefFlag):
    case Tag(0x01, RefFlag):
    case Tag(0x02, RefFlag):
    case Tag(0x03, RefFlag):
    case Tag(0x04, RefFlag):
    case Tag(0x05, RefFlag):
    case Tag(0x06, RefFlag):
    case Tag(0x07, RefFlag):
    case Tag(0x08, RefFlag):
    case Tag(0x09, RefFlag):
    case Tag(0x0a, RefFlag):
    case Tag(0x0b, RefFlag):
    case Tag(0x0c, RefFlag):
    case Tag(0x0d, RefFlag):
    case Tag(0x0e, RefFlag):
    case Tag(0x0f, RefFlag):
    case WrtTag:
    case RoTag:
    case IntTag:
      continue;
    case RealTag:
      {
	register heapT rtmp = *P;

	*P = *(P+1);
	*(P+1) = rtmp;
      }
      cnv_w(P++);
      cnv_w(P++);
      continue;
    case StrTag:
      {
	register int StrWords;

	P--;
	cnv_w(P+1);
	StrWords = Str_Words(P);
	if (!imp_cnv(P, PLimit)) {
	  return(False);
	}
	P += 2 + StrWords;
      }
      continue;
    case NilTag:
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
    case L_IntTag: 
    case L_NilTag: 
    case TplTag:
      continue;
    case VctrTag:
    case InvldTag:
    default:
      err_tbl_add(MACHINE, ErBADARG);
      return(False);
    }
  }
  return(True);
}

/* New Emulator */
/*@@@*/

imp_cnv_ctl(PString)                       /* control */
     heapP PString;
{
  register heapP P;
  register opcodeP PC;	/* Program Counter */
  register heapP PLimit, POffset = PString+2;
  
  register int Ia;

  PC = (opcodeP) (POffset+1);
  cnv_w(POffset);
  P = POffset + (int) *POffset;
  PLimit = P; /* End of opcodes */
  cnv_w(P++); /* Prcdr-Name-Offset */
  cnv_w(P++); /* Prcdr-Arity */
  cnv_w(P);   /* Prcdr-Index */
  
 next_instr_label:
  if (PC == (opcodeP) PLimit) {
    return(True);
  }
  cnv_s(PC);
  switch (*PC++) {
  case 0:
    if (PC != (opcodeP) PLimit) {
      err_tbl_add(MACHINE, ErBADARG);
      return(False);
    }
    next_instr();

  case deref_2:                /* 0x001 */
    cnv_pc_reg_offset();
    cnv_pc_reg();
    next_instr();

  case deref_3:                /* 0x002 */
  case deref_car_3:            /* 0x005 */
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_reg();
    next_instr();

  case deref_subarg_3:         /* 0x003 */ 
    cnv_pc_subarg_addr();
    cnv_pc_reg();
    cnv_pc_reg();
    next_instr();

  case deref_subarg_2:         /* 0x004 */
    cnv_pc_subarg_addr();
    cnv_pc_reg();
    next_instr();

  case deref_car_2:            /* 0x006 */
  case deref_2_addr:           /* 0x00c */ 
  case load_car:               /* 0x00e */
    cnv_pc_reg();
    cnv_pc_reg();
    next_instr();

  case deref_list:             /* 0x007 */
    cnv_pc_reg_offset();
    cnv_pc_reg();
    cnv_pc_intra_offset();
    next_instr();
 
  case deref_car_list:         /* 0x008 */
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_intra_offset();
    next_instr();

  case deref_sub_list:         /* 0x009 */
    cnv_pc_subarg_addr();
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_intra_offset();
    next_instr();

  case deref_vars:             /* 0x00a */
    cnv_s(PC);
    Ia = *PC++;
    for(; Ia > 0; Ia-- )
      cnv_pc_reg_offset();
    cnv_pc_intra_offset();
    next_instr();

  case deref_var1:             /* 0x00b */
    cnv_pc_reg_offset();
    cnv_pc_intra_offset();
    next_instr();

  case deref_integer1:         /* 0x00d */
    cnv_pc_reg_offset();
    cnv_pc_intra_offset();
    cnv_pc_intra_offset();
    next_instr();

  case deref_list3:            /* 0x00f */
    cnv_pc_reg();
    cnv_pc_reg_offset();
    cnv_pc_reg();
    cnv_pc_intra_offset();
    next_instr();
 
  case load_we_var:             /* 0x010 */
  case load_ref_to_we_var:      /* 0x011 */
  case load_nil:                /* 0x01c */ 
    next_instr();

  case load_ro_of_reg:          /* 0x012 */
  case load_ref_to_ro_of_reg:   /* 0x013 */
  case load_reg:                /* 0x016 */
  case load_reg_indirect:       /* 0x017 */
  case load_car_of_reg:         /* 0x018 */
    cnv_pc_reg();
    next_instr();

  case load_ro_of_subarg:       /* 0x014 */
  case load_ref_to_ro_of_subarg:/* 0x015 */
  case load_ref_to_subarg:      /* 0x01b */
    cnv_pc_subarg_addr();
    next_instr();

  case load_pr_arg:             /* 0x019 */
    cnv_pc_cp_arg();
    next_instr();

  case load_subarg:             /* 0x01a */
    cnv_pc_subarg();
    next_instr();

  case load_word:               /* 0x01d */
    cnv_pc_word();
    next_instr();

  case load_real:               /* 0x01e */
  case load_ref_to_real:        /* 0x01f */
    cnv_pc_double();
    next_instr();

  case load_ref_to_string:      /* 0x020 */
    cnv_pc_ref();
    next_instr();

  case allocate_var:            /* 0x021 */ 
  case allocate_list_cell:      /* 0x024 */
    cnv_pc_reg();
    next_instr();
    
  case allocate_vars:           /* 0x022 */
    cnv_s(PC);
    Ia = *PC++;
    for(; Ia > 0; Ia--) 
	cnv_pc_reg();
    next_instr();
 
  case allocate_tuple:          /* 0x023 */
  case allocate_listN:          /* 0x026 */
    cnv_pc_reg();
    cnv_pc_nibble();
    next_instr();

  case allocate_list_we:        /* 0x025 */
    cnv_pc_reg();
    cnv_pc_reg();
    next_instr();

  case allocate_pr:             /* 0x027 */
    cnv_pc_nibble();
    cnv_pc_reg();
    next_instr();

  case list_assign_with_check:  /* 0x028 */
  case list_assign:             /* 0x029 */
    cnv_pc_reg_offset();
    cnv_pc_reg_offset();
    next_instr();

  case fetch:                   /* 0x02a */
    cnv_pc_reg();
    cnv_pc_inter_addr();
    next_instr();

  case copy_Rs_Rd:              /* 0x02b */
  case copy_Rs_SRd:             /* 0x02d */
  case copy_SRs_Rd:             /* 0x031 */
  case copy_SRs_SRd:            /* 0x033 */
  case copy_RRs_Rd:             /* 0x037 */
  case copy_RRs_SRd:            /* 0x039 */
  case copy_CRs_Rd:             /* 0x03a */
  case copy_CRs_SRd:            /* 0x03c */
    cnv_pc_reg();
    cnv_pc_reg();
    next_instr();

  case copy_Rs_CpId:            /* 0x02c */
  case copy_SRs_CpId:           /* 0x032 */
  case copy_RRs_CpId:           /* 0x038 */  
  case copy_CRs_CpId:           /* 0x03b */
    cnv_pc_reg();
    cnv_pc_cp_arg();
    next_instr();

  case copy_CpIs_Rd:            /* 0x02e */
  case copy_CpIs_SRd:           /* 0x030 */
    cnv_pc_cp_arg();
    cnv_pc_reg();
    next_instr();

  case copy_CpIs_CpId:          /* 0x02f */
    cnv_pc_cp_arg();
    cnv_pc_cp_arg();
    next_instr();

  case copy_RsIs_Rd:            /* 0x034 */
  case copy_RsIs_SRd:           /* 0x036 */
    cnv_pc_subarg();
    cnv_pc_reg();
    next_instr();

  case copy_RsIs_CpId:          /* 0x035 */
    cnv_pc_subarg();
    cnv_pc_cp_arg();
    next_instr();

  case copy_ARsIs_Rd:           /* 0x03d */        
  case copy_ARsIs_SRd:          /* 0x03f */
  case copy_RARsIs_Rd:          /* 0x040 */
  case copy_RARsIs_SRd:         /* 0x042 */      
    cnv_pc_subarg_addr();
    cnv_pc_reg();
    next_instr();

  case copy_ARsIs_CpId:         /* 0x03e */
  case copy_RARsIs_CpId:        /* 0x041 */
    cnv_pc_subarg_addr();
    cnv_pc_cp_arg();
    next_instr();

  case copy_Nil_Rd:             /* 0x043 */
  case copy_Nil_SRd:            /* 0x045 */
  case copy_WeVar_Rd:           /* 0x04f */
  case copy_WeVar_SRd:          /* 0x051 */
    cnv_pc_reg();
    next_instr();

  case copy_Nil_CpId:           /* 0x044 */
  case copy_WeVar_CpId:         /* 0x050 */
    cnv_pc_cp_arg();
    next_instr();

  case copy_Word_Rd:            /* 0x046 */
  case copy_Word_SRd:           /* 0x048 */
    cnv_pc_word();
    cnv_pc_reg();
    next_instr();

  case copy_Word_CpId:          /* 0x047 */
    cnv_pc_word();
    cnv_pc_cp_arg();
    next_instr();

  case copy_Real_Rd:            /* 0x049 */
  case copy_Real_SRd:           /* 0x04b */
    cnv_pc_double();
    cnv_pc_reg();
    next_instr();

  case copy_Real_CpId:          /* 0x04a */
    cnv_pc_double();
    cnv_pc_cp_arg();
    next_instr();

  case copy_String_Rd:          /* 0x04c */
  case copy_String_SRd:         /* 0x04e */
    cnv_pc_ref();
    cnv_pc_reg();
    next_instr();

  case copy_String_CpId:        /* 0x04d */  
    cnv_pc_ref();
    cnv_pc_cp_arg();
    next_instr();

  case asgn_Rs_SRd:             /* 0x052 */        
  case asgn_SRs_SRd:            /* 0x054 */
  case asgn_RRs_SRd:            /* 0x056 */
  case asgn_CRs_SRd:            /* 0x057 */
  case assign_Rs_SRd:           /* 0x05f */
  case assign_SRs_SRd:          /* 0x061 */
  case assign_RRs_SRd:          /* 0x063 */
  case assign_CRs_SRd:          /* 0x064 */          
  case assign_com_Rs_SRd:       /* 0x06c */
  case assign_com_SRs_SRd:      /* 0x06e */
  case assign_com_RRs_SRd:      /* 0x070 */
  case assign_com_CRs_SRd:      /* 0x071 */          
  case assign_com_tr_Rs_SRd:    /* 0x079 */
  case assign_com_tr_SRs_SRd:   /* 0x07b */
  case assign_com_tr_RRs_SRd:   /* 0x07d */
  case assign_com_tr_CRs_SRd:   /* 0x07e */          
    cnv_pc_reg();
    cnv_pc_reg();
    next_instr();

  case asgn_CpIs_SRd:           /* 0x053 */
  case assign_CpIs_SRd:         /* 0x060 */
  case assign_com_CpIs_SRd:     /* 0x06d */
  case assign_com_tr_CpIs_SRd:  /* 0x07a */
    cnv_pc_cp_arg();
    cnv_pc_reg();
    next_instr();

  case asgn_RsIs_SRd:           /* 0x055 */
  case assign_RsIs_SRd:         /* 0x062 */
  case assign_com_RsIs_SRd:     /* 0x06f */
  case assign_com_tr_RsIs_SRd:  /* 0x07c */
    cnv_pc_subarg();
    cnv_pc_reg();
    next_instr();

  case asgn_ARsIs_SRd:          /* 0x058 */
  case asgn_RARsIs_SRd:         /* 0x059 */
  case assign_ARsIs_SRd:        /* 0x065 */
  case assign_RARsIs_SRd:       /* 0x066 */
  case assign_com_ARsIs_SRd:    /* 0x072 */
  case assign_com_RARsIs_SRd:   /* 0x073 */
  case assign_com_tr_ARsIs_SRd: /* 0x07f */
  case assign_com_tr_RARsIs_SRd:/* 0x080 */
    cnv_pc_subarg_addr();
    cnv_pc_reg();
    next_instr();

  case asgn_Nil_SRd:            /* 0x05a */
  case asgn_WeVar_SRd:          /* 0x05e */
  case assign_Nil_SRd:          /* 0x067 */
  case assign_WeVar_SRd:        /* 0x06b */
  case assign_com_Nil_SRd:      /* 0x074 */
  case assign_com_WeVar_SRd:    /* 0x081 */
  case assign_com_tr_Nil_SRd:   /* 0x078 */
  case assign_com_tr_WeVar_SRd: /* 0x085 */
    cnv_pc_reg();
    next_instr();

  case asgn_Word_SRd:           /* 0x05b */
  case assign_Word_SRd:         /* 0x068 */
  case assign_com_Word_SRd:     /* 0x075 */
  case assign_com_tr_Word_SRd:  /* 0x082 */
    cnv_pc_word();
    cnv_pc_reg();
    next_instr();

  case asgn_Real_SRd:           /* 0x05c */
  case assign_Real_SRd:         /* 0x069 */
  case assign_com_Real_SRd:     /* 0x076 */
  case assign_com_tr_Real_SRd:  /* 0x083 */
    cnv_pc_double();
    cnv_pc_reg();
    next_instr();

  case asgn_String_SRd:         /* 0x05d */
  case assign_String_SRd:       /* 0x06a */
  case assign_com_String_SRd:   /* 0x077 */
  case assign_com_tr_String_SRd:/* 0x084 */
    cnv_pc_ref();
    cnv_pc_reg();
    next_instr();

  case assign_and_inc:          /* 0x086 */
  case assign_inc_com:          /* 0x087 */
  case assign_inc_com_trail:    /* 0x088 */
    cnv_pc_reg_offset();
    cnv_pc_reg_offset();
    next_instr();

  case goto_there:              /* 0x089 */
    cnv_pc_intra_offset();
    next_instr();

  case if_not_reference:        /* 0x08a - 0x094*/
  case if_not_variable:
  case if_not_writable:
  case if_not_read_only:
  case if_not_integer:
  case if_not_real:
  case if_not_string:
  case if_not_nil:
  case if_not_list:
  case if_not_tuple:
  case if_not_vector:
    cnv_pc_reg();
    cnv_pc_intra_offset();
    next_instr();

  case if_int_lt:               /* 0x095 - 0x0ac */ 
  case if_int_bt:
  case if_int_le:
  case if_int_be:
  case if_int_eq:
  case if_int_neq:
  case if_real_lt:
  case if_real_bt:
  case if_real_le:
  case if_real_be:
  case if_real_eq:
  case if_real_neq:
  case if_str_lt:
  case if_str_bt:
  case if_str_le:
  case if_str_be:
  case if_str_eq:
  case if_str_neq:
  case if_tuple_lt:
  case if_tuple_bt:
  case if_tuple_le:
  case if_tuple_be:
  case if_tuple_eq:
  case if_tuple_neq:
  case cmp_int_bt:              /* 0xc8 - 0xcd */
  case cmp_int_le:
  case cmp_int_be:
  case cmp_int_eq:
  case cmp_int_ne:
  case cmp_int_lt:
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_intra_offset();
    next_instr();

  case switch_on_tag:           /* 0x0ad */
    cnv_pc_reg();
    cnv_s(PC);
    Ia = (*PC++) + 1;
    cnv_pc_word();
    cnv_pc_word();
    for (; Ia > 0; Ia--) 
      cnv_pc_intra_offset();
    next_instr();

  case unify_args:               /* 0x0ae - 0x0c2 */
    cnv_s(PC);
    Ia = *PC++;
    cnv_pc_intra_offset();
    for(; Ia > 0; Ia--) {
      cnv_s(PC);
      switch(*PC++) {
      case unify_reg_reg:        /* 0x0af */
      case unify_reg_roreg:      /* 0x0b2 */
      case unify_reg_carreg:     /* 0x0b4 */
	cnv_pc_reg();
	cnv_pc_reg();
	break;
      case unify_reg_xreg:       /* 0x0b0 */
      case unify_reg_axreg:      /* 0x0b1 */
      case unify_reg_roaxreg:    /* 0x0b3 */
	cnv_pc_reg();
	cnv_pc_subarg_addr();
	break;
      case unify_reg_word:       /* 0x0b5 */
	cnv_pc_reg();
	cnv_pc_word();
	break;
      case unify_reg_string:     /* 0x0b6 */
	cnv_pc_reg();
	cnv_pc_ref();
	break;
      case unify_reg_real:       /* 0x0b7 */
	cnv_pc_reg();
	cnv_pc_double();
	break;
      case unify_reg_nil:        /* 0x0b8 */
	cnv_pc_reg();
	break;
      case unify_xreg_reg:       /* 0x0b9 */
      case unify_xreg_roreg:     /* 0x0bc */
      case unify_xreg_carreg:    /* 0x0be */
	cnv_pc_subarg_addr();
	cnv_pc_reg();
	break;
     case unify_xreg_xreg:       /* 0x0ba */
     case unify_xreg_axreg:      /* 0x0bb */
     case unify_xreg_roaxreg:    /* 0x0bd */
	cnv_pc_subarg_addr(); 
	cnv_pc_subarg_addr();
	break;
     case unify_xreg_word:       /* 0x0bf */
	cnv_pc_subarg_addr();
	cnv_pc_word();
	break;
     case unify_xreg_string:     /* 0x0c0 */
	cnv_pc_subarg_addr();
	cnv_pc_ref();
	break;
     case unify_xreg_real:       /* 0x0c1 */
	cnv_pc_subarg_addr();
	cnv_pc_double();
	break;
     case unify_xreg_nil:        /* 0x0c2 */
	cnv_pc_subarg_addr();
	break;
     default:
        err_tbl_add(MACHINE, ErUDFDCD);
        return( False );
        break;
     }
    }
    next_instr();      /* end of unify */

  case branch_integer:          /* 0x0c3 */
  case branch_tuple:            /* 0x0c5 */
    cnv_pc_reg();
    cnv_w(PC);
    Ia = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    cnv_w(PC);
    Ia = Val_of(((heapT) (pc_word() - Ia))) + 2;
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    for (; Ia > 0; Ia--) {
      cnv_pc_intra_offset();
    }
    next_instr();

  case branch_real:             /* 0x0c4 */
    /* Not defined yet */
    err_tbl_add(MACHINE, ErBADARG);
    return(False);

  case case_hash_integer:       /* 0x0c6 */
  case case_hash_string:        /* 0x0c7 */
    cnv_pc_reg();
    cnv_s(PC);
    Ia = (*PC++) + 1;
    for (; Ia > 0; Ia--) 
      cnv_pc_intra_offset();
    next_instr();

  case decrement_2_reg:           /* 0x0ce */
  case decrement:                 /* 0x0d0 */
  case decrement_2_pointer:       /* 0x0d2 */
  case increment_2_reg:           /* 0x0d3 */
  case increment:                 /* 0x0d5 */
  case increment_2_pointer:       /* 0x0d7 */
  case increment_and_commit:      /* 0x0d8 */
  case decrement_and_commit:      /* 0x0d9 */
    cnv_pc_reg();
    cnv_pc_reg();
    next_instr();

  case decrement_2_xreg:           /* 0x0cf */
  case increment_2_xreg:           /* 0x0d4 */
    cnv_pc_reg();
    cnv_pc_subarg_addr();
    cnv_pc_subarg_addr();
    next_instr();

  case decrement_pointer:          /* 0x0d1 */
  case increment_pointer:          /* 0x0d6 */
    cnv_pc_reg();
    next_instr();

  case plus_reg_reg_reg:           /* 0x0da */
  case plusnum_reg_reg:            /* 0x0e2 */
  case diff_reg_reg_reg:           /* 0x0eb */
  case diffnum_reg_reg:            /* 0x0f3 */
  case cmt_plus_reg_reg_reg:       /* 0x0fc */
  case cmt_plusnum_reg_reg:        /* 0x104 */
  case cmt_diff_reg_reg_reg:       /* 0x10d */
  case cmt_diffnum_reg_reg:        /* 0x115 */
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_reg();
    next_instr();

  case plus_reg_reg_xreg:          /* 0x0db */
  case diff_reg_reg_xreg:          /* 0x0ec */
  case cmt_plus_reg_reg_xreg:      /* 0x0fd */
  case cmt_diff_reg_reg_xreg:      /* 0x10e */
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_subarg_addr();
    next_instr();

  case plus_reg_int_reg:           /* 0x0dc */
  case plusnum_reg_int:            /* 0x0e3 */
  case diff_reg_int_reg:           /* 0x0ed */
  case diffnum_reg_int:            /* 0x0f4 */
  case cmt_plus_reg_int_reg:       /* 0x0fe */
  case cmt_plusnum_reg_int:        /* 0x105 */
  case cmt_diff_reg_int_reg:       /* 0x10f */
  case cmt_diffnum_reg_int:        /* 0x116 */
    cnv_pc_reg();
    cnv_pc_word();
    cnv_pc_reg();
    next_instr();

  case plus_reg_int_xreg:          /* 0x0dd */
  case diff_reg_int_xreg:          /* 0x0ee */
  case cmt_plus_reg_int_xreg:      /* 0x0ff */
  case cmt_diff_reg_int_xreg:      /* 0x110 */
    cnv_pc_reg();
    cnv_pc_word();
    cnv_pc_subarg_addr();
    next_instr();

  case plus_int_reg_reg:           /* 0x0de */
  case plusnum_int_reg:            /* 0x0e5 */
  case diff_int_reg_reg:           /* 0x0ef */
  case diffnum_int_reg:            /* 0x0f6 */
  case cmt_plus_int_reg_reg:       /* 0x100 */
  case cmt_plusnum_int_reg:        /* 0x107 */
  case cmt_diff_int_reg_reg:       /* 0x111 */
  case cmt_diffnum_int_reg:        /* 0x118 */
    cnv_pc_word();
    cnv_pc_reg();
    cnv_pc_reg();
    next_instr();

  case plus_int_reg_xreg:          /* 0x0df */
  case diff_int_reg_xreg:          /* 0x0f0 */
  case cmt_plus_int_reg_xreg:      /* 0x101 */
  case cmt_diff_int_reg_xreg:      /* 0x112 */
    cnv_pc_word();
    cnv_pc_reg();
    cnv_pc_subarg_addr();
    next_instr();

  case plus_int_int_reg:           /* 0x0e0 */
  case plusnum_int_int:            /* 0x0e6 */
  case diff_int_int_reg:           /* 0x0f1 */
  case diffnum_int_int:            /* 0x0f7 */
  case cmt_plus_int_int_reg:       /* 0x102 */
  case cmt_plusnum_int_int:        /* 0x108 */
  case cmt_diff_int_int_reg:       /* 0x113 */
  case cmt_diffnum_int_int:        /* 0x119 */
    cnv_pc_word();
    cnv_pc_word();
    cnv_pc_reg();
    next_instr();

  case plus_int_int_xreg:          /* 0x0e1 */
  case diff_int_int_xreg:          /* 0x0f2 */
  case cmt_plus_int_int_xreg:      /* 0x103 */
  case cmt_diff_int_int_xreg:      /* 0x114 */
    cnv_pc_word();
    cnv_pc_word();
    cnv_pc_subarg_addr();
    next_instr();

  case plusnum_reg_real:           /* 0x0e4 */
  case diffnum_reg_real:           /* 0x0f5 */
  case cmt_plusnum_reg_real:       /* 0x106 */
  case cmt_diffnum_reg_real:       /* 0x117 */
    cnv_pc_reg();
    cnv_pc_double();
    cnv_pc_reg();
    next_instr();

  case plusnum_int_real:           /* 0x0e7 */
  case diffnum_int_real:           /* 0x0f8 */
  case cmt_plusnum_int_real:       /* 0x109 */
  case cmt_diffnum_int_real:       /* 0x11a */
    cnv_pc_word();
    cnv_pc_double();
    cnv_pc_reg();
    next_instr();

  case plusnum_real_reg:           /* 0x0e8 */
  case diffnum_real_reg:           /* 0x0f9 */
  case cmt_plusnum_real_reg:       /* 0x10a */
  case cmt_diffnum_real_reg:       /* 0x11b */
    cnv_pc_double();
    cnv_pc_reg();
    cnv_pc_reg();
    next_instr();

  case plusnum_real_int:           /* 0x0e9 */         
  case diffnum_real_int:           /* 0x0fa */
  case cmt_plusnum_real_int:       /* 0x10b */         
  case cmt_diffnum_real_int:       /* 0x11c */
    cnv_pc_double();
    cnv_pc_word();
    cnv_pc_reg();
    next_instr();

  case plusnum_real_real:          /* 0xea */
  case diffnum_real_real:          /* 0xfb */
  case cmt_plusnum_real_real:      /* 010c */
  case cmt_diffnum_real_real:      /* 011d */
    cnv_pc_double();
    cnv_pc_double();
    cnv_pc_reg();
    next_instr();

  case enqueue:                   /* 0x11e */
    cnv_pc_reg();
    cnv_pc_inter_addr();
    next_instr();
    
  case iterate1:                  /* 0x11f */
    cnv_pc_inter_addr();
    next_instr();

  case iterate:                   /* 0x120 */
    cnv_pc_intra_offset();
    next_instr();

  case execute:                   /* 0x121 */
    cnv_pc_inter_addr();
    cnv_pc_inter_offset();
    next_instr();

  case execute2:                  /* 0x122 */
    cnv_pc_inter_addr();
    cnv_pc_inter_addr();
    next_instr();

  case execute1:                 /* 0x123 */
    cnv_pc_inter_addr();
    next_instr();

  case halt:                     /* 0x124 */
  case commit_nolabel:           /* 0x126 */
  case commit0:                  /* 0x127 */
  case suspend0:                 /* 0x12b */
  case set_HBT:                  /* 0x12d */
  case undo:                     /* 0x12e */
    next_instr();

  case commit1:                  /* 0x125 */ 
    /* ???? temporary - until ctl adjusts itself to new vars */
    cnv_pc_intra_offset();
    next_instr();

  case set_cp_arity:              /* 0x128 */
    cnv_pc_nibble();
    next_instr();

  case suspend2:                  /* 0x129 */
    cnv_pc_inter_offset();
    cnv_pc_inter_offset();
    next_instr();

  case suspend1:                  /* 0x12a */
    cnv_pc_inter_offset();
    next_instr();

  case suspend_on:                /* 0x12c */
    cnv_pc_reg();
    next_instr();

  case drf2_if_not_nil:           /* 0x12f */
  case drf2_if_not_list:          /* 0x130 */
    cnv_pc_reg_offset();
    cnv_pc_reg();
    cnv_pc_intra_offset();
    next_instr();

  case drf2_if_int_neq:           /* 0x131 */
  case drf2_if_tuple_neq:         /* 0x134 */
    cnv_pc_reg_offset();
    cnv_pc_reg();
    cnv_pc_word();
    cnv_pc_intra_offset();
    next_instr();

  case drf2_if_real_neq:          /* 0x132 */
    cnv_pc_reg_offset();
    cnv_pc_reg();
    cnv_pc_double();
    cnv_pc_intra_offset();
    next_instr();

  case drf2_if_str_neq:           /* 0x133 */
    cnv_pc_reg_offset();
    cnv_pc_reg();
    cnv_pc_inter_addr();
    cnv_pc_intra_offset();
    next_instr();

  case drf2_switch_on_tag:        /* 0x135 */
    cnv_pc_reg_offset();
    cnv_s(PC);
    Ia = *PC++ + 1;
    cnv_pc_word();
    for (; Ia > 0; Ia-- )
       cnv_pc_intra_offset();
    next_instr();

  case drf2_branch_integer:       /* 0x136 */
  case drf2_branch_tuple:         /* 0x138 */
    cnv_pc_reg_offset();
    cnv_pc_reg();
    cnv_w(PC);
    Ia = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    cnv_w(PC);
    Ia = Val_of(((heapT) (pc_word() - Ia))) + 2;
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    for (; Ia > 0; Ia--) {
      cnv_pc_intra_offset();
    }
    next_instr();

  case drf2_branch_real:          /* 0x137 */
    /* Not defined yet */
    err_tbl_add(MACHINE, ErUDFDCD);
    return(False);

  case drf2_hash_integer:         /* 0x139 */
  case drf2_hash_string:          /* 0x13a */
    cnv_pc_reg_offset();
    cnv_pc_reg();
    cnv_s(PC);
    Ia = *PC++ + 1;
    for (; Ia > 0; Ia--) {
      cnv_pc_intra_offset();
    }
    next_instr();

  case if_var_suspend:            /* 0x13b */
    cnv_pc_reg();
    cnv_pc_reg();
    next_instr();

  case drf1_if_var_suspend:       /* 0x13c */
    cnv_pc_reg_offset();
    cnv_pc_intra_offset();
    next_instr();

  case alloc_pr_enqu:             /* 0x13d */
    cnv_pc_nibble();
    cnv_pc_inter_addr();
    next_instr();

  case alloc_pr_regs_enqu:        /* 0x13e */
    cnv_s(PC);
    Ia = *PC++;
    cnv_pc_inter_addr();
    for (; Ia > 0; Ia--) {
      cnv_pc_reg();
    }
    next_instr();

  case mult_operation:            /* 0x13f */
    cnv_s(PC);
    Ia = *PC++;
    cnv_s(PC);
    switch (*PC++) {
    case copy_Rs_Rd:             /* 0x02b sub-op */
    case copy_Rs_SRd:            /* 0x02d sub-op */
    case copy_SRs_Rd:            /* 0x031 sub-op */
    case copy_SRs_SRd:           /* 0x033 sub-op */
    case copy_RRs_Rd:            /* 0x037 sub-op */
    case copy_RRs_SRd:           /* 0x039 sub-op */
    case copy_CRs_Rd:            /* 0x03a sub-op */
    case copy_CRs_SRd:           /* 0x03c sub-op */
    case asgn_Rs_SRd:            /* 0x052 sub-op */
    case asgn_SRs_SRd:           /* 0x054 sub-op */
    case asgn_RRs_SRd:           /* 0x056 sub-op */
    case asgn_CRs_SRd:           /* 0x057 sub-op */
    case assign_Rs_SRd:          /* 0x05f sub-op */
    case assign_SRs_SRd:         /* 0x061 sub-op */
    case assign_RRs_SRd:         /* 0x063 sub-op */
    case assign_CRs_SRd:         /* 0x064 sub-op */
    case assign_com_Rs_SRd:      /* 0x06c sub-op */
    case assign_com_SRs_SRd:     /* 0x06e sub-op */
    case assign_com_RRs_SRd:     /* 0x070 sub-op */
    case assign_com_CRs_SRd:     /* 0x071 sub-op */
    case assign_com_tr_Rs_SRd:   /* 0x079 sub-op */
    case assign_com_tr_SRs_SRd:  /* 0x07b sub-op */
    case assign_com_tr_RRs_SRd:  /* 0x07d sub-op */
    case assign_com_tr_CRs_SRd:  /* 0x07e sub-op */
       for (; Ia > 0; Ia--) {
        cnv_pc_reg();
        cnv_pc_reg();
      }
      next_instr();
    case copy_Rs_CpId:           /* 0x02c sub-op */
    case copy_SRs_CpId:          /* 0x032 sub-op */
    case copy_RRs_CpId:          /* 0x038 sub-op */
    case copy_CRs_CpId:          /* 0x03b sub-op */ 
      for (; Ia > 0; Ia--) {
        cnv_pc_reg();
        cnv_pc_cp_arg();
      }
      next_instr();
    case copy_CpIs_Rd:           /* 0x02e sub-op */
    case copy_CpIs_SRd:          /* 0x030 sub-op */
    case asgn_CpIs_SRd:          /* 0x053 sub-op */
    case assign_CpIs_SRd:        /* 0x060 sub-op */
    case assign_com_CpIs_SRd:    /* 0x06d sub-op */
    case assign_com_tr_CpIs_SRd: /* 0x07a sub-op */
      for (; Ia > 0; Ia--) {
	cnv_pc_cp_arg();
	cnv_pc_reg();
      }
      next_instr();
    case copy_CpIs_CpId:         /* 0x02f sub-op */      
     for (; Ia > 0; Ia--) {
	cnv_pc_cp_arg();
	cnv_pc_cp_arg();
      }
      next_instr();
    case copy_RsIs_Rd:           /* 0x034 sub-op */                 
    case copy_RsIs_SRd:          /* 0x036 sub-op */
    case asgn_RsIs_SRd:          /* 0x055 sub-op */
    case assign_RsIs_SRd:        /* 0x062 sub-op */
    case assign_com_RsIs_SRd:    /* 0x06f sub-op */
    case assign_com_tr_RsIs_SRd: /* 0x07c sub-op */
      for (; Ia > 0; Ia--) {
	cnv_pc_subarg();
	cnv_pc_reg();
      }
      next_instr();
    case copy_RsIs_CpId:         /* 0x038 sub-op */
      for (; Ia > 0; Ia--) {
	cnv_pc_subarg();
	cnv_pc_cp_arg();
      }
      next_instr();
    case copy_ARsIs_Rd:          /* 0x03d sub-op */ 
    case copy_ARsIs_SRd:         /* 0x03f sub-op */
    case copy_RARsIs_Rd:         /* 0x040 sub-op */
    case copy_RARsIs_SRd:        /* 0x042 sub-op */
    case asgn_ARsIs_SRd:         /* 0x058 sub-op */
    case asgn_RARsIs_SRd:        /* 0x059 sub-op */
    case assign_ARsIs_SRd:       /* 0x065 sub-op */
    case assign_RARsIs_SRd:      /* 0x066 sub-op */
    case assign_com_ARsIs_SRd:   /* 0x072 sub-op */
    case assign_com_RARsIs_SRd:  /* 0x073 sub-op */
    case assign_com_tr_ARsIs_SRd:/* 0x07f sub-op */
    case assign_com_tr_RARsIs_SRd:/* 0x080 sub-op */
      for (; Ia > 0; Ia--) {
	cnv_pc_subarg_addr();
	cnv_pc_reg();
      }
      next_instr();
    case copy_ARsIs_CpId:        /* 0x03e sub-op */
    case copy_RARsIs_CpId:       /* 0x041 sub-op */
      for (; Ia > 0; Ia--) {
	cnv_pc_subarg_addr();
	cnv_pc_cp_arg();
      }
      next_instr();
    case copy_Nil_Rd:            /* 0x043 sub-op */
    case copy_Nil_SRd:           /* 0x045 sub-op */
    case copy_WeVar_Rd:          /* 0x04f sub-op */
    case copy_WeVar_SRd:         /* 0x051 sub-op */
    case asgn_Nil_SRd:           /* 0x05a sub-op */
    case asgn_WeVar_SRd:         /* 0x05e sub-op */
    case assign_Nil_SRd:         /* 0x067 sub-op */
    case assign_WeVar_SRd:       /* 0x06b sub-op */
    case assign_com_Nil_SRd:     /* 0x074 sub-op */
    case assign_com_WeVar_SRd:   /* 0x078 sub-op */
    case assign_com_tr_Nil_SRd:  /* 0x081 sub-op */
    case assign_com_tr_WeVar_SRd:/* 0x085 sub-op */
      for (; Ia > 0; Ia--) 
	cnv_pc_reg();
      next_instr();
    case copy_Nil_CpId:          /* 0x044 sub-op */
    case copy_WeVar_CpId:        /* 0x050 sub-op */
      for (; Ia > 0; Ia--) 
	cnv_pc_subarg();
       next_instr();
    case copy_Word_Rd:           /* 0x046 sub-op */
    case copy_Word_SRd:          /* 0x048 sub-op */
    case asgn_Word_SRd:          /* 0x05b sub-op */
    case assign_Word_SRd:        /* 0x068 sub-op */
    case assign_com_Word_SRd:    /* 0x075 sub-op */
    case assign_com_tr_Word_SRd: /* 0x082 sub-op */
      for (; Ia > 0; Ia--) {
	cnv_pc_word();
	cnv_pc_reg();
      }
      next_instr();
    case copy_Word_CpId:         /* 0x047 sub-op */
      for (; Ia > 0; Ia--) {
	cnv_pc_word();
 	cnv_pc_subarg();
      }
      next_instr();
    case copy_Real_Rd:           /* 0x049 sub-op */
    case copy_Real_SRd:          /* 0x04b sub-op */
    case asgn_Real_SRd:          /* 0x05c sub-op */
    case assign_Real_SRd:        /* 0x069 sub-op */
    case assign_com_Real_SRd:    /* 0x076 sub-op */
    case assign_com_tr_Real_SRd: /* 0x083 sub-op */
      for (; Ia > 0; Ia--) {
	cnv_pc_double();
	cnv_pc_reg();
      }
      next_instr();
    case copy_Real_CpId:         /* 0x04a sub-op */
      for (; Ia > 0; Ia--) {
 	cnv_pc_double();
 	cnv_pc_subarg();
      }
      next_instr();
    case copy_String_Rd:         /* 0x04c sub-op */
    case copy_String_SRd:        /* 0x04e sub-op */
    case asgn_String_SRd:        /* 0x05d sub-op */
    case assign_String_SRd:      /* 0x06a sub-op */
    case assign_com_String_SRd:  /* 0x077 sub-op */
    case assign_com_tr_String_SRd:/* 0x084 sub-op */
      for (; Ia > 0; Ia--) {
	cnv_pc_ref();
	cnv_pc_reg();
      }
      next_instr();
    case copy_String_CpId:       /* 0x04d sub-op */
      for (; Ia > 0; Ia--) {
	cnv_pc_ref();
 	cnv_pc_subarg();
      }
      next_instr();
    }                    

    /* Ask kernels */

    /* Ask/0 */

  case otherwise:		/* "otherwise", 0 */  /* 0x140 */
    cnv_pc_intra_offset();
    cnv_pc_intra_offset();  
    next_instr();

    /* Ask/1 */

  case is_nonvar:	 	/* "nonvar", 1 */ /* 0x141 - 0x152 */
  case ask_unknown:		/* "ask_unknown", 1 */
  case is_known:	  	/* "known", 1 */
  case is_var:			/* "var", 1 */
  case is_we:                   /* "we", 1 */
  case is_not_we:               /* "not_we", 1 */
  case is_ro:			/* "ro", 1 */
  case is_integer:  		/* "integer", 1 */
  case is_real:			/* "real", 1 */
  case is_string:	  	/* "string",1 */
  case is_list:			/* "list", 1 */
  case is_tuple:	  	/* "tuple", 1 */
  case is_vector:	  	/* "vector", 1 */
  case is_module:		/* "module", 1 */
  case is_constant:		/* "constant", 1 */
  case is_compound:		/* "compound", 1 */
  case is_number:		/* "number", 1 */
  case grounded:		/* "grounded", 1 */
  case exceptions:		/* "exceptions", 1 */  /* 0x188 */
    cnv_pc_reg();
    cnv_pc_intra_offset();  
    cnv_pc_intra_offset();  
    next_instr();

    /* Ask/2 */
    
  case wait_equals:		/* "=?=", 2 */      /* 0x153 - 0x164 */
  case wait_not_equals:		/* "=\=", 2 */
  case is_less:			/* "@<", 2 */
  case not_identical:		/* "\=", 2 */
  case lt:			/* "<", 2 */
  case le:			/* "=<", 2 */
  case bitwise_not:		/* "bitwise_not", 2 */
  case string_length:		/* "string_length", 2 */
  case string_hash:		/* "string_hash", 2 */
  case cnv_to_integer:		/* "cnvrt_to_integer", 2 */
  case cnv_to_real:		/* "cnvrt_to_real", 2 */
  case cnv_to_string:		/* "cnvrt_to_string", 2 */
  case arity:			/* "arity", 2 */
  case make_tuple:		/* "make_tuple", 2 */	
  case info:			/* "info", 2 */
  case code_info:		/* "code_info", 2 */
  case invalid_g:		/* "invalid", 2 */
  case var_info_g:		/* "var_info", 2 */ 
  case identical:               /* "==", 2 */        /* 0x187 */
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_intra_offset();  
    cnv_pc_intra_offset();  
    next_instr();
   
    /* Ask/3 */
    
  case plus:			/* "+", 3 */   /* 0x165 - 0x16f */
  case diff:			/* "-", 3 */
  case times:			/* "*", 3 */
  case div:			/* "/", 3 */
  case mod:			/* "mod", 3 */
  case bitwise_and:		/* "bitwise_and", 3 */
  case bitwise_or:		/* "bitwise_or", 3 */
  case list_to_string:		/* "list_to_string", 3 */
  case string_to_dlist:		/* "string_to_dlist", 3 */
  case arg:			/* "arg", 3 */
  case read_vector:		/* "read_vector", 3 */ 
  case nth_char:		/* "nth_char", 3 */	/* 0x189 */
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_intra_offset();  
    cnv_pc_intra_offset();  
    next_instr();
   
    /* Ask/4 */

  case freeze:			/* "freeze", 4 */  /* 0x170 */
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_intra_offset();  
    cnv_pc_intra_offset();  
    next_instr();

    /* Tell/0 Kernels */
    
  case deschedule:		/* "deschedule", 0 */  /* 0x172 */
    cnv_pc_intra_offset();  
    cnv_pc_intra_offset();  
    next_instr();
    
    /* Tell/1 */
    
  case machine_output:	   /* "machine_output", 1 */ /* 0x173 - 0x177 */
  case ttyput_byte:		/* "ttyput_byte", 1 */
  case ttyput_integer:		/* "ttyput_integer", 1 */
  case ttyput_string:		/* "ttyput_string", 1 */
  case make_unshared_g:		/* "make_unshared", 1 */
    cnv_pc_reg();
    cnv_pc_intra_offset();  
    cnv_pc_intra_offset();  
    next_instr();

    /* Tell/2 */
    
  case debug:			/* "debug", 2 */ /* 0x178 - 0x180 */
  case equals:			/* "=", 2 */
  case do_execute:		/* "execute", 2 */
  case close_vector:		/* "close_vector", 2 */
  case priority:		/* "priority", 2 */
  case request:			/* "request", 2 */
  case link:			/* "link", 2 */
  case bind_ro_g:	        /* "bind_ro", 2 */
  case make_invalid_g:          /* "make_invalid", 2 */
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_intra_offset();  
    cnv_pc_intra_offset();  
    next_instr();

    /* Tell/3 */
    
  case activate:		/* "activate", 3 */ /* 0x181 - 0x184 */
  case melt:			/* "melt", 3 */
  case make_vector:		/* "make_vector", 3 */
  case make_shared_g:	        /* "make_shared", 2 */
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_intra_offset();  
    cnv_pc_intra_offset();  
    next_instr();

    /* Tell/4 */
    
  case store_vector:	  /* "store_vector", 4 */ /* 0x185 - 0x186 */
  case write_vector:		/* "write_vector", 4 */
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_intra_offset();  
    cnv_pc_intra_offset();  
    next_instr();
    
  default:
    err_tbl_add(MACHINE, ErBADARG);
    return(False);
  }
}

/* Export convertor */

exp_convert(PString)
     register heapP PString;
{
  register heapP PStrEnd;

  if (!IsStr(*PString)) {
    err_tbl_add(MACHINE, ErBADARG);
    return(False);
  }
  PStrEnd = PString + 2 + Str_Words(PString);
  return(exp_cnv(PString, PStrEnd));
}

exp_cnv(PString, PEnd)
     heapP PString, PEnd;
{
  if (!IsStr(*PString) || ((PString + 2 + Str_Words(PString)) > PEnd)) {
    err_tbl_add(MACHINE, ErBADARG);
    return(False);
  }
  switch (Str_Type(PString)) {
  case CharType:
    return(exp_cnv_char(PString));
  case PrcdrType:
    return(exp_cnv_prcd(PString));
  case MdlType:
    return(exp_cnv_mdl(PString));
  case FrznType:
    return(exp_cnv_frzn(PString));
  case CtlType:
    return(exp_cnv_ctl(PString));
  default:
    err_tbl_add(MACHINE, ErBADARG);
    return(False);
  }
}

exp_cnv_char(PString)
  heapP PString;
{
  register heapP P = PString;

  cnv_w(P++); 
  cnv_w(P);
  return(True);
}

exp_cnv_prcd(PString)
     heapP PString;
{
  register heapP P = PString;
  register int StrWords = Str_Words(PString);
  heapP POffset, PLimit;

  cnv_w(P++); 
  cnv_w(P++);
  POffset = P;
  P = POffset + (int) *POffset; 
  cnv_w(POffset++);
  if (((int) *POffset) != 0) {
    PLimit = POffset + (int) *POffset;
    cnv_w(POffset);
    cnv_w(PLimit); /* arity */
  }
  else {
    PLimit = PString + 2 + StrWords;
  }
  for (; P < PLimit;) { /* data area */
    cnv_w(P++);
  }
  return(True);
}

exp_cnv_mdl(PString)
     heapP PString;
{
  register heapP P = PString;
  register int StrWords = Str_Words(PString);
  heapP POffset, PLimit;

  cnv_w(P++); 
  cnv_w(P++);
  POffset = P++;
  if (((int) *POffset) != 0) { /* Info Offset */
    if (IsStr((*(POffset + (int) *POffset)))) { /* Ctl */
      PLimit = PString + 2 + StrWords - 1 /* Null Word */;
    }
    else {
      PLimit = POffset + (int) *POffset;
    }
    cnv_w(POffset);
  }
  else {
    PLimit = PString + 2 + StrWords;
  }
  for (; P < PLimit; P += 2 + StrWords) {
    StrWords = Str_Words(P);
    if (!exp_cnv(P, PLimit)) {
      return(False);
    }
  }
  return(True);
}

exp_cnv_frzn(PString)
     heapP PString;
{
  register heapP P = PString;
  heapP PLimit = P + 2 + Str_Words(PString);

  cnv_w(P++); 
  cnv_w(P++);
  while (P < PLimit) {
    switch (Tag_of(*P)) {
    case Tag(0x00, RefFlag):
    case Tag(0x01, RefFlag):
    case Tag(0x02, RefFlag):
    case Tag(0x03, RefFlag):
    case Tag(0x04, RefFlag):
    case Tag(0x05, RefFlag):
    case Tag(0x06, RefFlag):
    case Tag(0x07, RefFlag):
    case Tag(0x08, RefFlag):
    case Tag(0x09, RefFlag):
    case Tag(0x0a, RefFlag):
    case Tag(0x0b, RefFlag):
    case Tag(0x0c, RefFlag):
    case Tag(0x0d, RefFlag):
    case Tag(0x0e, RefFlag):
    case Tag(0x0f, RefFlag):
    case WrtTag:
    case RoTag:
    case IntTag:
      cnv_w(P++);
      continue;
    case RealTag:
      cnv_w(P++);
      {
	register heapT rtmp = *P;

	*P = *(P+1);
	*(P+1) = rtmp;
      }
      cnv_w(P++);
      cnv_w(P++);
      continue;
    case StrTag:
      {
	register int StrWords = Str_Words(P);

	if (!exp_cnv(P, PLimit)) {
	  return(False);
	}
	P += 2 + StrWords;
      }
      continue;
    case NilTag:
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
    case L_IntTag: 
    case L_NilTag: 
    case TplTag:
      cnv_w(P++);
      continue;
    case VctrTag:
    case InvldTag:
    default:
      err_tbl_add(MACHINE, ErBADARG);
      return(False);
    }
  }
  return(True);
}

/* New Emulator */
/*@@@*/

exp_cnv_ctl(PString)
     heapP PString;
{
  register heapP P = PString;
  register opcodeP PC;	/* Program Counter */
  register opcodeT PCV;	/* Program Counter Value */
  register heapP PLimit, POffset;
  
  register int Ia;

  cnv_w(P++); 
  cnv_w(P++);
  POffset = P;
  PC = (opcodeP) (P+1);
  P = POffset + (int) *POffset;
  PLimit = P; /* End of opcodes */
  cnv_w(POffset);
  cnv_w(P++); /* Prcdr-Name-Offset */
  cnv_w(P++); /* Prcdr-Arity */
  cnv_w(P); /* Prcdr-Index */
  
 next_instr_label:
  if (PC == (opcodeP) PLimit) {
    return(True);
  }
  PCV = *PC;
  cnv_pc_nibble();
  switch (PCV) {
  case 0:
    if (PC != (opcodeP) PLimit) {
      err_tbl_add(MACHINE, ErBADARG);
      return(False);
    }
    next_instr();

  case deref_2:                  /* 0x001 */
    cnv_pc_reg_offset();
    cnv_pc_reg();
    next_instr();

  case deref_3:                  /* 0x002 */
  case deref_car_3:              /* 0x005 */
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_reg();
    next_instr();

  case deref_subarg_3:           /* 0x003 */
    cnv_pc_subarg_addr();
    cnv_pc_reg();
    cnv_pc_reg();
    next_instr();

  case deref_subarg_2:           /* 0x004 */
    cnv_pc_subarg_addr();
    cnv_pc_reg();
    next_instr();

  case deref_car_2:              /* 0x006 */
  case deref_2_addr:             /* 0x00c */   
  case load_car:                 /* 0x00e */
    cnv_pc_reg();
    cnv_pc_reg();
    next_instr();

  case deref_list:               /* 0x007 */
    cnv_pc_reg_offset();
    cnv_pc_reg();
    cnv_pc_intra_offset();
    next_instr();

  case deref_car_list:           /* 0x008 */
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_intra_offset();
    next_instr();

  case deref_sub_list:           /* 0x009 */
    cnv_pc_subarg_addr();
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_intra_offset();
    next_instr();

  case deref_vars:               /* 0x00a */
    Ia = *PC;
    cnv_pc_nibble();
    for( ; Ia > 0; Ia-- )
      cnv_pc_reg_offset();
    cnv_pc_intra_offset();
    next_instr();

  case deref_var1:               /* 0x00b */
    cnv_pc_reg_offset();
    cnv_pc_intra_offset();
    next_instr();

  case deref_integer1:           /* 0x00d */
    cnv_pc_reg_offset();
    cnv_pc_intra_offset();
    cnv_pc_intra_offset();
    next_instr();

  case deref_list3:              /* 0x00f */
    cnv_pc_reg();
    cnv_pc_reg_offset();
    cnv_pc_reg();
    cnv_pc_intra_offset();
    next_instr();

  case load_we_var:              /* 0x010 */
  case load_ref_to_we_var:       /* 0x011 */
  case load_nil:                 /* 0x01c */        
    next_instr();

  case load_ro_of_reg:           /* 0x012 */
  case load_ref_to_ro_of_reg:    /* 0x013 */
  case load_reg:                 /* 0x016 */
  case load_reg_indirect:        /* 0x017 */
  case load_car_of_reg:          /* 0x018 */
    cnv_pc_reg();
    next_instr();

  case load_ro_of_subarg:        /* 0x014 */
  case load_ref_to_ro_of_subarg: /* 0x015 */
  case load_ref_to_subarg:       /* 0x01b */
    cnv_pc_subarg_addr();
    next_instr();

  case load_pr_arg:              /* 0x019 */
    cnv_pc_cp_arg();
    next_instr();

  case load_subarg:              /* 0x01a */
    cnv_pc_subarg();
    next_instr();

  case load_word:                /* 0x01d */
    cnv_pc_word();
    next_instr();

  case load_real:                /* 0x01e */
  case load_ref_to_real:         /* 0x01f */
    cnv_pc_double();
    next_instr();
    
  case load_ref_to_string:       /* 0x020 */
    cnv_pc_ref();
    next_instr();

  case allocate_var:             /* 0x021 */
  case allocate_list_cell:       /* 0x024 */
    cnv_pc_reg();
    next_instr();

 case allocate_vars:             /* 0x022 */
    Ia = *PC;
    cnv_pc_nibble();
    for(; Ia > 0; Ia--) 
	cnv_pc_reg();
    next_instr();
 
  case allocate_tuple:           /* 0x023 */
  case allocate_listN:           /* 0x026 */
    cnv_pc_reg();
    cnv_pc_nibble();
    next_instr();

  case allocate_list_we:         /* 0x025 */
    cnv_pc_reg();
    cnv_pc_reg();
    next_instr();

  case allocate_pr:              /* 0x027 */
    cnv_pc_nibble();
    cnv_pc_reg();
    next_instr();

  case list_assign_with_check:   /* 0x028 */
  case list_assign:              /* 0x029 */
    cnv_pc_reg_offset();
    cnv_pc_reg_offset();
    next_instr();

  case fetch:                    /* 0x02a */
    cnv_pc_reg();
    cnv_pc_inter_addr();
    next_instr();

  case copy_Rs_Rd:               /* 0x02b */
  case copy_Rs_SRd:              /* 0x02d */
  case copy_SRs_Rd:              /* 0x031 */
  case copy_SRs_SRd:             /* 0x033 */
  case copy_RRs_Rd:              /* 0x037 */
  case copy_RRs_SRd:             /* 0x039 */
  case copy_CRs_Rd:              /* 0x03a */
  case copy_CRs_SRd:             /* 0x03c */
    cnv_pc_reg();
    cnv_pc_reg();
    next_instr();

  case copy_Rs_CpId:             /* 0x02c */
  case copy_SRs_CpId:            /* 0x032 */
  case copy_RRs_CpId:            /* 0x038 */
  case copy_CRs_CpId:            /* 0x03b */
    cnv_pc_reg();
    cnv_pc_cp_arg();
    next_instr();

  case copy_CpIs_Rd:             /* 0x02e */
  case copy_CpIs_SRd:            /* 0x030 */
    cnv_pc_cp_arg();
    cnv_pc_reg();
    next_instr();

  case copy_CpIs_CpId:           /* 0x02f */
    cnv_pc_cp_arg();
    cnv_pc_cp_arg();
    next_instr();

  case copy_RsIs_Rd:             /* 0x034 */
  case copy_RsIs_SRd:            /* 0x036 */
    cnv_pc_subarg();
    cnv_pc_reg();
    next_instr();

  case copy_RsIs_CpId:           /* 0x035 */
    cnv_pc_subarg();
    cnv_pc_cp_arg();
    next_instr();

  case copy_ARsIs_Rd:            /* 0x03c */
  case copy_ARsIs_SRd:           /* 0x03e */ 
  case copy_RARsIs_Rd:           /* 0x040 */
  case copy_RARsIs_SRd:          /* 0x042 */
    cnv_pc_subarg_addr();
    cnv_pc_reg();
    next_instr();

  case copy_ARsIs_CpId:          /* 0x03e */
  case copy_RARsIs_CpId:         /* 0x041 */
    cnv_pc_subarg_addr();
    cnv_pc_cp_arg();
    next_instr();

  case copy_Nil_Rd:              /* 0x043 */
  case copy_Nil_SRd:             /* 0x045 */
  case copy_WeVar_Rd:            /* 0x04f */
  case copy_WeVar_SRd:           /* 0x051 */
    cnv_pc_reg();
    next_instr();

  case copy_Nil_CpId:            /* 0x044 */
  case copy_WeVar_CpId:          /* 0x050 */
    cnv_pc_cp_arg();
    next_instr();

  case copy_Word_Rd:             /* 0x046 */
  case copy_Word_SRd:            /* 0x048 */
    cnv_pc_word();
    cnv_pc_reg();
    next_instr();

  case copy_Word_CpId:           /* 0x047 */
    cnv_pc_word();
    cnv_pc_cp_arg();
    next_instr();

  case copy_Real_Rd:             /* 0x049 */
  case copy_Real_SRd:            /* 0x04b */
    cnv_pc_double();
    cnv_pc_reg();
    next_instr();

  case copy_Real_CpId:           /* 0x04a */
    cnv_pc_double();
    cnv_pc_cp_arg();
    next_instr();

  case copy_String_Rd:           /* 0x04c */
  case copy_String_SRd:          /* 0x04e */
    cnv_pc_ref();
    cnv_pc_reg();
    next_instr();

  case copy_String_CpId:         /* 0x04d */   
    cnv_pc_ref();
    cnv_pc_cp_arg();
    next_instr();

  case asgn_Rs_SRd:              /* 0x052 */        
  case asgn_SRs_SRd:             /* 0x054 */
  case asgn_RRs_SRd:             /* 0x056 */
  case asgn_CRs_SRd:             /* 0x057 */
  case assign_Rs_SRd:            /* 0x05f */
  case assign_SRs_SRd:           /* 0x061 */
  case assign_RRs_SRd:           /* 0x063 */
  case assign_CRs_SRd:           /* 0x064 */             
  case assign_com_Rs_SRd:        /* 0x06c */
  case assign_com_SRs_SRd:       /* 0x06e */
  case assign_com_RRs_SRd:       /* 0x070 */
  case assign_com_CRs_SRd:       /* 0x071 */             
  case assign_com_tr_Rs_SRd:     /* 0x079 */
  case assign_com_tr_SRs_SRd:    /* 0x07b */
  case assign_com_tr_RRs_SRd:    /* 0x07d */
  case assign_com_tr_CRs_SRd:    /* 0x07e */             
    cnv_pc_reg();
    cnv_pc_reg();
    next_instr();

  case asgn_CpIs_SRd:            /* 0x053 */
  case assign_CpIs_SRd:          /* 0x060 */
  case assign_com_CpIs_SRd:      /* 0x06d */
  case assign_com_tr_CpIs_SRd:   /* 0x07a */
    cnv_pc_cp_arg();
    cnv_pc_reg();
    next_instr();

  case asgn_RsIs_SRd:            /* 0x055 */
  case assign_RsIs_SRd:          /* 0x062 */
  case assign_com_RsIs_SRd:      /* 0x06f */
  case assign_com_tr_RsIs_SRd:   /* 0x07c */
    cnv_pc_subarg();
    cnv_pc_reg();
    next_instr();

  case asgn_ARsIs_SRd:           /* 0x058 */
  case asgn_RARsIs_SRd:          /* 0x059 */
  case assign_ARsIs_SRd:         /* 0x065 */
  case assign_RARsIs_SRd:        /* 0x066 */
  case assign_com_ARsIs_SRd:     /* 0x072 */
  case assign_com_RARsIs_SRd:    /* 0x073 */
  case assign_com_tr_ARsIs_SRd:  /* 0x07f */
  case assign_com_tr_RARsIs_SRd: /* 0x080 */
    cnv_pc_subarg_addr();
    cnv_pc_reg();
    next_instr();

  case asgn_Nil_SRd:             /* 0x05a */
  case asgn_WeVar_SRd:           /* 0x05e */
  case assign_Nil_SRd:           /* 0x067 */
  case assign_WeVar_SRd:         /* 0x06b */
  case assign_com_Nil_SRd:       /* 0x074 */
  case assign_com_WeVar_SRd:     /* 0x078 */
  case assign_com_tr_Nil_SRd:    /* 0x081 */
  case assign_com_tr_WeVar_SRd:  /* 0x085 */
    cnv_pc_reg();
    next_instr();

  case asgn_Word_SRd:            /* 0x05b */
  case assign_Word_SRd:          /* 0x068 */    
  case assign_com_Word_SRd:      /* 0x075 */     
  case assign_com_tr_Word_SRd:   /* 0x082 */    
    cnv_pc_word();
    cnv_pc_reg();
    next_instr();

  case asgn_Real_SRd:            /* 0x05c */
  case assign_Real_SRd:          /* 0x069 */
  case assign_com_Real_SRd:      /* 0x076 */
  case assign_com_tr_Real_SRd:   /* 0x083 */
    cnv_pc_double();
    cnv_pc_reg();
    next_instr();

  case asgn_String_SRd:          /* 0x05d */
  case assign_String_SRd:        /* 0x06a */
  case assign_com_String_SRd:    /* 0x077 */
  case assign_com_tr_String_SRd: /* 0x084 */
    cnv_pc_ref();
    cnv_pc_reg();
    next_instr();

  case assign_and_inc:             /* 0x086 */
  case assign_inc_com:             /* 0x087 */
  case assign_inc_com_trail:       /* 0x088 */
    cnv_pc_reg_offset();
    cnv_pc_reg_offset();
    next_instr();

  case goto_there:                 /* 0x089 */
    cnv_pc_intra_offset();
    next_instr();

  case if_not_reference:           /* 0x08a - 0x094 */
  case if_not_variable:
  case if_not_writable:
  case if_not_read_only:
  case if_not_integer:
  case if_not_real:
  case if_not_string:
  case if_not_nil:
  case if_not_list:
  case if_not_tuple:
  case if_not_vector:
    cnv_pc_reg();
    cnv_pc_intra_offset();
    next_instr();

  case if_int_lt:                  /* 0x095 - 0x0ac */
  case if_int_bt:
  case if_int_le:
  case if_int_be:
  case if_int_eq:
  case if_int_neq:
  case if_real_lt:
  case if_real_bt:
  case if_real_le:
  case if_real_be:
  case if_real_eq:
  case if_real_neq:
  case if_str_lt:
  case if_str_bt:
  case if_str_le:
  case if_str_be:
  case if_str_eq:
  case if_str_neq:
  case if_tuple_lt:
  case if_tuple_bt:
  case if_tuple_le:
  case if_tuple_be:
  case if_tuple_eq:
  case if_tuple_neq:
  case cmp_int_lt:	        /* 0x0c8 - 0x0cd */
  case cmp_int_bt:
  case cmp_int_le:
  case cmp_int_be:
  case cmp_int_eq:
  case cmp_int_ne:
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_intra_offset();
    next_instr();

  case switch_on_tag:      /* 0x0ad */
    cnv_pc_reg();
    Ia = (*PC) + 1;
    cnv_pc_nibble();
    cnv_pc_word();
    cnv_pc_word();
    for (; Ia > 0; Ia--) 
      cnv_pc_intra_offset();
    next_instr();

  case unify_args:              /* 0x0ae - 0x0c2 */	
    Ia = *PC;
    cnv_pc_nibble();
    cnv_pc_intra_offset();
    for(; Ia > 0; Ia--) {
      PCV = *PC;
      cnv_pc_nibble();
      switch(PCV) {
      case unify_reg_reg:      /* 0x0af */
      case unify_reg_roreg:    /* 0x0b2 */
      case unify_reg_carreg:   /* 0x0b4 */
	cnv_pc_reg();
	cnv_pc_reg();
	break;
      case unify_reg_xreg:     /* 0x0b0 */
      case unify_reg_axreg:    /* 0x0b1 */
      case unify_reg_roaxreg:  /* 0x0b3 */
	cnv_pc_reg();
	cnv_pc_subarg_addr();
	break;
      case unify_reg_word:     /* 0x0b5 */
	cnv_pc_reg();
	cnv_pc_word();
	break;
      case unify_reg_string:   /* 0x0b6 */
	cnv_pc_reg();
	cnv_pc_ref();
	break;
      case unify_reg_real:     /* 0x0b7 */
	cnv_pc_reg();
	cnv_pc_double();
	break;
      case unify_reg_nil:      /* 0x0b8 */
	cnv_pc_reg();
	break;
      case unify_xreg_reg:     /* 0x0b9 */
      case unify_xreg_roreg:   /* 0x0bb */
      case unify_xreg_carreg:  /* 0x0be */
	cnv_pc_subarg_addr();
	cnv_pc_reg();
	break;
      case unify_xreg_xreg:    /* 0x0ba */
      case unify_xreg_axreg:   /* 0x0bb */
      case unify_xreg_roaxreg: /* 0xbd */
	cnv_pc_subarg_addr();  
	cnv_pc_subarg_addr();
	break;
      case unify_xreg_word:    /* 0x0bf */
	cnv_pc_subarg_addr();
	cnv_pc_word();
	break;
      case unify_xreg_string:  /* 0x0c0 */
	cnv_pc_subarg_addr();
	cnv_pc_ref();
	break;
      case unify_xreg_real:    /* 0x0c1 */
	cnv_pc_subarg_addr();
	cnv_pc_double();
	break;
      case unify_xreg_nil:     /* 0x0c2 */
	cnv_pc_subarg_addr();
	break;
      default:
        err_tbl_add(MACHINE, ErUDFDCD);
        return( False );
        break;
      }
    }
    next_instr();           /* end of unify */

  case branch_integer:          /* 0x0c3 */
  case branch_tuple:            /* 0x0c5 */
    cnv_pc_reg();
    Ia = pc_word();
    cnv_pc_word();
    Ia = Val_of(((heapT) (pc_word() - Ia))) + 2;
    cnv_pc_word();
    for (; Ia > 0; Ia--) {
      cnv_pc_intra_offset();
    }
    next_instr();

  case branch_real:             /* 0x0c4 */
    /* Not defined yet */
    err_tbl_add(MACHINE, ErBADARG);
    return(False);

  case case_hash_integer:       /* 0x0c6 */
  case case_hash_string:        /* 0x0c7 */
    cnv_pc_reg();
    Ia = (*PC) + 1;
    cnv_pc_nibble();
    for (; Ia > 0; Ia--) {
      cnv_pc_intra_offset();
    }
    next_instr();

  case decrement_2_reg:          /* 0x0cc */
  case decrement:                /* 0x0d0 */
  case decrement_2_pointer:      /* 0x0d2 */
  case increment_2_reg:          /* 0x0d3 */
  case increment:                /* 0x0d5 */
  case increment_2_pointer:      /* 0x0d7 */
  case increment_and_commit:     /* 0x0d8 */
  case decrement_and_commit:     /* 0x0d9 */
    cnv_pc_reg();
    cnv_pc_reg();
    next_instr();

  case decrement_2_xreg:         /* 0x0d8 */
  case increment_2_xreg:         /* 0x0d9 */
    cnv_pc_reg();
    cnv_pc_subarg_addr();
    cnv_pc_subarg_addr();
    next_instr();

  case decrement_pointer:        /* 0x0d1 */
  case increment_pointer:        /* 0x0d6 */
    cnv_pc_reg();
    next_instr();

  case plus_reg_reg_reg:             /* 0x0da */
  case plusnum_reg_reg:              /* 0x0e2 */
  case diff_reg_reg_reg:             /* 0x0eb */
  case diffnum_reg_reg:              /* 0x0f3 */
  case cmt_plus_reg_reg_reg:         /* 0x0fc */
  case cmt_plusnum_reg_reg:          /* 0x104 */
  case cmt_diff_reg_reg_reg:         /* 0x10d */
  case cmt_diffnum_reg_reg:          /* 0x115 */
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_reg();
    next_instr();

  case plus_reg_reg_xreg:            /* 0x0db */
  case diff_reg_reg_xreg:            /* 0x0ec */
  case cmt_plus_reg_reg_xreg:        /* 0x0fd */
  case cmt_diff_reg_reg_xreg:        /* 0x10e */
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_subarg_addr();
    next_instr();

  case plus_reg_int_reg:             /* 0x0dc */
  case plusnum_reg_int:              /* 0x0e3 */
  case diff_reg_int_reg:             /* 0x0ed */
  case diffnum_reg_int:              /* 0x0f4 */
  case cmt_plus_reg_int_reg:         /* 0x0fe */
  case cmt_plusnum_reg_int:          /* 0x105 */
  case cmt_diff_reg_int_reg:         /* 0x10f */
  case cmt_diffnum_reg_int:          /* 0x116 */
    cnv_pc_reg();
    cnv_pc_word();
    cnv_pc_reg();
    next_instr();

  case plus_reg_int_xreg:            /* 0x0dd */
  case diff_reg_int_xreg:            /* 0x0ee */
  case cmt_plus_reg_int_xreg:        /* 0x0ff */
  case cmt_diff_reg_int_xreg:        /* 0x110 */
    cnv_pc_reg();
    cnv_pc_word();
    cnv_pc_subarg_addr();
    next_instr();

  case plus_int_reg_reg:             /* 0x0de */
  case plusnum_int_reg:              /* 0x0e5 */
  case diff_int_reg_reg:             /* 0x0ef */
  case diffnum_int_reg:              /* 0x0f6 */
  case cmt_plus_int_reg_reg:         /* 0x100 */
  case cmt_plusnum_int_reg:          /* 0x107 */
  case cmt_diff_int_reg_reg:         /* 0x111 */
  case cmt_diffnum_int_reg:          /* 0x118 */
    cnv_pc_word();
    cnv_pc_reg();
    cnv_pc_reg();
    next_instr();

  case plus_int_reg_xreg:            /* 0x0df */
  case diff_int_reg_xreg:            /* 0x0f0 */
  case cmt_plus_int_reg_xreg:        /* 0x101 */
  case cmt_diff_int_reg_xreg:        /* 0x112 */
    cnv_pc_word();
    cnv_pc_reg();
    cnv_pc_subarg_addr();
    next_instr();
      
  case plus_int_int_reg:             /* 0x0e0 */
  case plusnum_int_int:              /* 0x0e6 */
  case diff_int_int_reg:             /* 0x0f1 */
  case diffnum_int_int:              /* 0x0f7 */
  case cmt_plus_int_int_reg:         /* 0x102 */
  case cmt_plusnum_int_int:          /* 0x108 */
  case cmt_diff_int_int_reg:         /* 0x113 */
  case cmt_diffnum_int_int:          /* 0x119 */
    cnv_pc_word();
    cnv_pc_word();
    cnv_pc_reg();
    next_instr();

  case plus_int_int_xreg:            /* 0x0e1 */
  case diff_int_int_xreg:            /* 0x0f2 */
  case cmt_plus_int_int_xreg:        /* 0x103 */
  case cmt_diff_int_int_xreg:        /* 0x114 */
    cnv_pc_word();
    cnv_pc_word();
    cnv_pc_subarg_addr();
    next_instr();

  case plusnum_reg_real:             /* 0x0e4 */
  case diffnum_reg_real:             /* 0x0f5 */
  case cmt_plusnum_reg_real:         /* 0x106 */
  case cmt_diffnum_reg_real:         /* 0x117 */
    cnv_pc_reg();
    cnv_pc_double();
    cnv_pc_reg();
    next_instr();

  case plusnum_int_real:             /* 0x0e7 */
  case diffnum_int_real:             /* 0x0f8 */
  case cmt_plusnum_int_real:         /* 0x109 */
  case cmt_diffnum_int_real:         /* 0x11a */
    cnv_pc_word();
    cnv_pc_double();
    cnv_pc_reg();
    next_instr();

  case plusnum_real_reg:             /* 0x0e8 */
  case diffnum_real_reg:             /* 0x0f9 */
  case cmt_plusnum_real_reg:         /* 0x10a */
  case cmt_diffnum_real_reg:         /* 0x11b */
    cnv_pc_double();
    cnv_pc_reg();
    cnv_pc_reg();
    next_instr();

  case plusnum_real_int:             /* 0x0e9 */
  case diffnum_real_int:             /* 0x0fa */
  case cmt_plusnum_real_int:         /* 0x10b */
  case cmt_diffnum_real_int:         /* 0x11c */
    cnv_pc_double();
    cnv_pc_word();
    cnv_pc_reg();
    next_instr();

  case plusnum_real_real:            /* 0x0ea */
  case diffnum_real_real:            /* 0x0fb */
  case cmt_plusnum_real_real:        /* 0x10c */
  case cmt_diffnum_real_real:        /* 0x11d */
    cnv_pc_double();
    cnv_pc_double();
    cnv_pc_reg();
    next_instr();

  case enqueue:               /* 0x11e */
    cnv_pc_reg();
    cnv_pc_inter_addr();
    next_instr();
    
  case iterate1:              /* 0x11f */
    cnv_pc_inter_addr();
    next_instr();

  case iterate:               /* 0x120 */
    cnv_pc_intra_offset();
    next_instr();

  case execute:               /* 0x121 */
    cnv_pc_inter_addr();
    cnv_pc_inter_offset();
    next_instr();

  case execute2:              /* 0x122 */
    cnv_pc_inter_addr();
    cnv_pc_inter_addr();
    next_instr();

  case execute1:              /* 0x123 */
    cnv_pc_inter_addr();
    next_instr();

  case halt:                 /* 0x124 */
  case commit_nolabel:       /* 0x126 */
  case commit0:              /* 0x127 */
  case suspend0:             /* 0x12b */
  case set_HBT:              /* 0x12d */
  case undo:                 /* 0x12e */
    next_instr();

  case commit1:              /* 0x125 */
    /* ???? temporary - until ctl adjusts itself to new vars */
    cnv_pc_intra_offset();
    next_instr();

  case set_cp_arity:         /* 0x128 */
    cnv_pc_nibble();
    next_instr();

  case suspend2:             /* 0x129 */
    cnv_pc_inter_offset();
    cnv_pc_inter_offset();
    next_instr();

  case suspend1:             /* 0x12a */
    cnv_pc_inter_offset();
    next_instr();

  case suspend_on:           /* 0x12c */
    cnv_pc_reg();
    next_instr();
    
  case drf2_if_not_nil:      /* 0x12f */
  case drf2_if_not_list:     /* 0x130 */
    cnv_pc_reg_offset();
    cnv_pc_reg();
    cnv_pc_intra_offset();
    next_instr();

  case drf2_if_int_neq:      /* 0x131 */
  case drf2_if_tuple_neq:    /* 0x134 */
    cnv_pc_reg_offset();
    cnv_pc_reg();
    cnv_pc_word();
    cnv_pc_intra_offset();
    next_instr();

  case drf2_if_real_neq:     /* 0x132 */
    cnv_pc_reg_offset();
    cnv_pc_reg();
    cnv_pc_double();
    cnv_pc_intra_offset();
    next_instr();

  case drf2_if_str_neq:      /* 0x133 */
    cnv_pc_reg_offset();
    cnv_pc_reg();
    cnv_pc_inter_addr();
    cnv_pc_intra_offset();
    next_instr();


  case drf2_switch_on_tag:   /* 0x135 */
    cnv_pc_reg_offset();
    cnv_pc_reg();
    Ia = *PC + 1;
    cnv_pc_nibble();
    cnv_pc_word();
    for (; Ia > 0; Ia--) 
      cnv_pc_intra_offset();
    next_instr();

  case drf2_branch_integer:  /* 0x136 */
  case drf2_branch_tuple:    /* 0x138 */
    cnv_pc_reg_offset();
    cnv_pc_reg();
    Ia = pc_word();
    cnv_pc_word();
    Ia = Val_of(((heapT) (pc_word() - Ia))) + 2;
    cnv_pc_word();
    for (; Ia > 0; Ia--) {
      cnv_pc_intra_offset();
    }
    next_instr();

  case drf2_branch_real:     /* 0x137 */      
    /* Not defined yet */
    err_tbl_add(MACHINE, ErUDFDCD);
    return(False);

  case drf2_hash_integer:    /* 0x139 */
  case drf2_hash_string:     /* 0x13a */
    cnv_pc_reg_offset();
    cnv_pc_reg();
    Ia = *PC + 1;
    cnv_pc_nibble();
    for (; Ia > 0; Ia--) {
      cnv_pc_intra_offset();
    }
    next_instr();

  case if_var_suspend:       /* 0x13b */
    cnv_pc_reg();
    cnv_pc_reg();
    next_instr();

  case drf1_if_var_suspend:  /* 0x13c */
    cnv_pc_reg_offset();
    cnv_pc_intra_offset();
    next_instr();

  case alloc_pr_enqu:        /* 0x13d */
    cnv_pc_nibble();
    cnv_pc_inter_addr();
    next_instr();

  case alloc_pr_regs_enqu:   /* 0x13e */
    Ia = *PC;
    cnv_pc_nibble();
    cnv_pc_inter_addr();
    for (; Ia > 0; Ia--) {
      cnv_pc_reg();
    }
    next_instr();

  case mult_operation:       /* 0x13f */
    Ia = *PC;
    cnv_pc_nibble();
    PCV = *PC;
    cnv_pc_nibble();
    switch (PCV) {
    case copy_Rs_Rd:             /* 0x02b sub-op */
    case copy_Rs_SRd:            /* 0x02d sub-op */
    case copy_SRs_Rd:            /* 0x031 sub-op */
    case copy_SRs_SRd:           /* 0x033 sub-op */
    case copy_RRs_Rd:            /* 0x037 sub-op */
    case copy_RRs_SRd:           /* 0x039 sub-op */
    case copy_CRs_Rd:            /* 0x03a sub-op */
    case copy_CRs_SRd:           /* 0x03c sub-op */
    case asgn_Rs_SRd:            /* 0x052 sub-op */
    case asgn_SRs_SRd:           /* 0x054 sub-op */
    case asgn_RRs_SRd:           /* 0x056 sub-op */
    case asgn_CRs_SRd:           /* 0x057 sub-op */
    case assign_Rs_SRd:          /* 0x05f sub-op */
    case assign_SRs_SRd:         /* 0x061 sub-op */
    case assign_RRs_SRd:         /* 0x063 sub-op */
    case assign_CRs_SRd:         /* 0x064 sub-op */
    case assign_com_Rs_SRd:      /* 0x06c sub-op */
    case assign_com_SRs_SRd:     /* 0x06e sub-op */
    case assign_com_RRs_SRd:     /* 0x070 sub-op */
    case assign_com_CRs_SRd:     /* 0x071 sub-op */
    case assign_com_tr_Rs_SRd:   /* 0x079 sub-op */
    case assign_com_tr_SRs_SRd:  /* 0x07b sub-op */
    case assign_com_tr_RRs_SRd:  /* 0x07d sub-op */
    case assign_com_tr_CRs_SRd:  /* 0x07e sub-op */
       for (; Ia > 0; Ia--) {
        cnv_pc_reg();
        cnv_pc_reg();
      }
      next_instr();
    case copy_Rs_CpId:           /* 0x02c sub-op */
    case copy_SRs_CpId:          /* 0x032 sub-op */
    case copy_RRs_CpId:          /* 0x038 sub-op */
    case copy_CRs_CpId:          /* 0x03b sub-op */ 
      for (; Ia > 0; Ia--) {
        cnv_pc_reg();
        cnv_pc_cp_arg();
      }
      next_instr();
    case copy_CpIs_Rd:           /* 0x02e sub-op */
    case copy_CpIs_SRd:          /* 0x030 sub-op */
    case asgn_CpIs_SRd:          /* 0x053 sub-op */
    case assign_CpIs_SRd:        /* 0x060 sub-op */
    case assign_com_CpIs_SRd:    /* 0x06d sub-op */
    case assign_com_tr_CpIs_SRd: /* 0x07a sub-op */
      for (; Ia > 0; Ia--) {
	cnv_pc_cp_arg();
	cnv_pc_reg();
      }
      next_instr();
    case copy_CpIs_CpId:         /* 0x02f sub-op */      
     for (; Ia > 0; Ia--) {
	cnv_pc_cp_arg();
	cnv_pc_cp_arg();
      }
      next_instr();
    case copy_RsIs_Rd:           /* 0x034 sub-op */                 
    case copy_RsIs_SRd:          /* 0x036 sub-op */
    case asgn_RsIs_SRd:          /* 0x055 sub-op */
    case assign_RsIs_SRd:        /* 0x062 sub-op */
    case assign_com_RsIs_SRd:    /* 0x06f sub-op */
    case assign_com_tr_RsIs_SRd: /* 0x07c sub-op */
      for (; Ia > 0; Ia--) {
	cnv_pc_subarg();
	cnv_pc_reg();
      }
      next_instr();
    case copy_RsIs_CpId:         /* 0x038 sub-op */
      for (; Ia > 0; Ia--) {
	cnv_pc_subarg();
	cnv_pc_cp_arg();
      }
      next_instr();
    case copy_ARsIs_Rd:          /* 0x03d sub-op */ 
    case copy_ARsIs_SRd:         /* 0x03f sub-op */
    case copy_RARsIs_Rd:         /* 0x040 sub-op */
    case copy_RARsIs_SRd:        /* 0x042 sub-op */
    case asgn_ARsIs_SRd:         /* 0x058 sub-op */
    case asgn_RARsIs_SRd:        /* 0x059 sub-op */
    case assign_ARsIs_SRd:       /* 0x065 sub-op */
    case assign_RARsIs_SRd:      /* 0x066 sub-op */
    case assign_com_ARsIs_SRd:   /* 0x072 sub-op */
    case assign_com_RARsIs_SRd:  /* 0x073 sub-op */
    case assign_com_tr_ARsIs_SRd:/* 0x07f sub-op */
    case assign_com_tr_RARsIs_SRd:/* 0x080 sub-op */
      for (; Ia > 0; Ia--) {
	cnv_pc_subarg_addr();
	cnv_pc_reg();
      }
      next_instr();
    case copy_ARsIs_CpId:        /* 0x03e sub-op */
    case copy_RARsIs_CpId:       /* 0x041 sub-op */
      for (; Ia > 0; Ia--) {
	cnv_pc_subarg_addr();
	cnv_pc_cp_arg();
      }
      next_instr();
    case copy_Nil_Rd:            /* 0x043 sub-op */
    case copy_Nil_SRd:           /* 0x045 sub-op */
    case copy_WeVar_Rd:          /* 0x04f sub-op */
    case copy_WeVar_SRd:         /* 0x051 sub-op */
    case asgn_Nil_SRd:           /* 0x05a sub-op */
    case asgn_WeVar_SRd:         /* 0x05e sub-op */
    case assign_Nil_SRd:         /* 0x067 sub-op */
    case assign_WeVar_SRd:       /* 0x06b sub-op */
    case assign_com_Nil_SRd:     /* 0x074 sub-op */
    case assign_com_WeVar_SRd:   /* 0x078 sub-op */
    case assign_com_tr_Nil_SRd:  /* 0x081 sub-op */
    case assign_com_tr_WeVar_SRd:/* 0x085 sub-op */
      for (; Ia > 0; Ia--) 
	cnv_pc_reg();
      next_instr();
    case copy_Nil_CpId:          /* 0x044 sub-op */
    case copy_WeVar_CpId:        /* 0x050 sub-op */
      for (; Ia > 0; Ia--) 
	cnv_pc_subarg();
       next_instr();
    case copy_Word_Rd:           /* 0x046 sub-op */
    case copy_Word_SRd:          /* 0x048 sub-op */
    case asgn_Word_SRd:          /* 0x05b sub-op */
    case assign_Word_SRd:        /* 0x068 sub-op */
    case assign_com_Word_SRd:    /* 0x075 sub-op */
    case assign_com_tr_Word_SRd: /* 0x082 sub-op */
      for (; Ia > 0; Ia--) {
	cnv_pc_word();
	cnv_pc_reg();
      }
      next_instr();
    case copy_Word_CpId:         /* 0x047 sub-op */
      for (; Ia > 0; Ia--) {
	cnv_pc_word();
 	cnv_pc_subarg();
      }
      next_instr();
    case copy_Real_Rd:           /* 0x049 sub-op */
    case copy_Real_SRd:          /* 0x04b sub-op */
    case asgn_Real_SRd:          /* 0x05c sub-op */
    case assign_Real_SRd:        /* 0x069 sub-op */
    case assign_com_Real_SRd:    /* 0x076 sub-op */
    case assign_com_tr_Real_SRd: /* 0x083 sub-op */
      for (; Ia > 0; Ia--) {
	cnv_pc_double();
	cnv_pc_reg();
      }
      next_instr();
    case copy_Real_CpId:         /* 0x04a sub-op */
      for (; Ia > 0; Ia--) {
 	cnv_pc_double();
 	cnv_pc_subarg();
      }
      next_instr();
    case copy_String_Rd:         /* 0x04c sub-op */
    case copy_String_SRd:        /* 0x04e sub-op */
    case asgn_String_SRd:        /* 0x05d sub-op */
    case assign_String_SRd:      /* 0x06a sub-op */
    case assign_com_String_SRd:  /* 0x077 sub-op */
    case assign_com_tr_String_SRd:/* 0x084 sub-op */
      for (; Ia > 0; Ia--) {
	cnv_pc_ref();
	cnv_pc_reg();
      }
      next_instr();
    case copy_String_CpId:       /* 0x04d sub-op */
      for (; Ia > 0; Ia--) {
	cnv_pc_ref();
 	cnv_pc_subarg();
      }
      next_instr();
    }

    /* Ask kernels */

    /* Ask/0 */

  case otherwise:		/* "otherwise", 0 */  /* 0x140 */
    cnv_pc_intra_offset();
    cnv_pc_intra_offset();  
    next_instr();

    /* Ask/1 */

  case is_nonvar:	 	/* "nonvar", 1 */   /* 0x141 - 0x152 */
  case ask_unknown:		/* "ask_unknown", 1 */
  case is_known:	  	/* "known", 1 */
  case is_var:			/* "var", 1 */
  case is_we:                   /* "we", 1 */
  case is_not_we:               /* "not_we", 1 */
  case is_ro:			/* "ro", 1 */
  case is_integer:  		/* "integer", 1 */
  case is_real:			/* "real", 1 */
  case is_string:	  	/* "string",1 */
  case is_list:			/* "list", 1 */
  case is_tuple:	  	/* "tuple", 1 */
  case is_vector:	  	/* "vector", 1 */
  case is_module:		/* "module", 1 */
  case is_constant:		/* "constant", 1 */
  case is_compound:		/* "compound", 1 */
  case is_number:		/* "number", 1 */
  case grounded:		/* "grounded", 1 */
  case exceptions:		/* "exceptions", 1 */   /* 0x188 */
    cnv_pc_reg();
    cnv_pc_intra_offset();  
    cnv_pc_intra_offset();  
    next_instr();

    /* Ask/2 */
    
  case wait_equals:		/* "=?=", 2 */  /* 0x153 - 0x164 */
  case wait_not_equals:		/* "=\=", 2 */
  case is_less:			/* "@<", 2 */
  case not_identical:		/* "\=", 2 */
  case lt:			/* "<", 2 */
  case le:			/* "=<", 2 */
  case bitwise_not:		/* "bitwise_not", 2 */
  case string_length:		/* "string_length", 2 */
  case string_hash:		/* "string_hash", 2 */
  case cnv_to_integer:		/* "cnvrt_to_integer", 2 */
  case cnv_to_real:		/* "cnvrt_to_real", 2 */
  case cnv_to_string:		/* "cnvrt_to_string", 2 */
  case arity:			/* "arity", 2 */
  case make_tuple:		/* "make_tuple", 2 */	
  case info:			/* "info", 2 */
  case code_info:		/* "code_info", 2 */
  case invalid_g:		/* "invalid", 2 */
  case var_info_g:		/* "var_info", 2 */ 
  case identical:               /* "==", 2 */       /* 0x187 */
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_intra_offset();  
    cnv_pc_intra_offset();  
    next_instr();
   
    /* Ask/3 */
    
  case plus:			/* "+", 3 */   /* 0x165 - 0x16f */
  case diff:			/* "-", 3 */
  case times:			/* "*", 3 */
  case div:			/* "/", 3 */
  case mod:			/* "mod", 3 */
  case bitwise_and:		/* "bitwise_and", 3 */
  case bitwise_or:		/* "bitwise_or", 3 */
  case list_to_string:		/* "list_to_string", 3 */
  case string_to_dlist:		/* "string_to_dlist", 3 */
  case arg:			/* "arg", 3 */
  case read_vector:		/* "read_vector", 3 */ 
  case nth_char:		/* "nth_char", 3 */	/* 0x189 */
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_intra_offset();  
    cnv_pc_intra_offset();  
    next_instr();
   
    /* Ask/4 */

  case freeze:			/* "freeze", 4 */ /* 0x0170 */
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_intra_offset();  
    cnv_pc_intra_offset();  
    next_instr();

    /* Ask/5 */

#ifdef DEEPFREEZE
  case dfreeze:			/* "dfreeze", 5 */  /* 0x171 */
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_intra_offset();  
    cnv_pc_intra_offset();  
    next_instr();
#endif

    /* Tell/0 Kernels */
    
  case deschedule:		/* "deschedule", 0 */  /* 0x172 */
    cnv_pc_intra_offset();  
    cnv_pc_intra_offset();  
    next_instr();
    
    /* Tell/1 */
    
  case machine_output: 	  /* "machine_output", 1 */ /* 0x173 - 0x177 */
  case ttyput_byte:		/* "ttyput_byte", 1 */
  case ttyput_integer:		/* "ttyput_integer", 1 */
  case ttyput_string:		/* "ttyput_string", 1 */
  case make_unshared_g:		/* "make_unshared", 1 */
    cnv_pc_reg();
    cnv_pc_intra_offset();  
    cnv_pc_intra_offset();  
    next_instr();

    /* Tell/2 */
    
  case debug:			/* "debug", 2 */  /* 0x178 - 0x180 */
  case equals:			/* "=", 2 */
  case do_execute:		/* "execute", 2 */
  case close_vector:		/* "close_vector", 2 */
  case priority:		/* "priority", 2 */
  case request:			/* "request", 2 */
  case link:			/* "link", 2 */
  case bind_ro_g:	        /* "bind_ro", 2 */
  case make_invalid_g:          /* "make_invalid", 2 */
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_intra_offset();  
    cnv_pc_intra_offset();  
    next_instr();

    /* Tell/3 */
    
  case activate:		/* "activate", 3 */ /* 0x181 - 0x184 */
  case melt:			/* "melt", 3 */
  case make_vector:		/* "make_vector", 3 */
  case make_shared_g:	       /* "make_shared", 2 */
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_intra_offset();  
    cnv_pc_intra_offset();  
    next_instr();

    /* Tell/4 */
    
  case store_vector:   	  /* "store_vector", 4 */ /* 0x185 - 0x186 */
  case write_vector:		/* "write_vector", 4 */
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_reg();
    cnv_pc_intra_offset();  
    cnv_pc_intra_offset();  
    next_instr();
    
  default:
    err_tbl_add(MACHINE, ErBADARG);
    return(False);
  }
}
