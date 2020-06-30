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
 **	emulate.c  -  control and main loop of the emulator
 */

#include	<stdio.h>
extern	FILE *DbgFile;

#include	<signal.h>

#include	"fcp.h"
#include	"codes.h"
#include	"global.h"
#include	"macros.h"
#include	"opcodes.h"
#include	"emulate.h"

/*
 **  The emulator
 */

emulate(Prcdr, W)
     heapT Prcdr;
     register workerP W;
{
  register opcodeP PC;	/* Program Counter */

  /* heapT is unsigned */

  register heapT Va;
  register heapP Pa;
  register heapT Vb;
  register heapP Pb;

  register int Ia;

  register heapT Vc, Vd, Ve;

  register heapP Pc, Pd, Pe;

  register int Ib, Ic;

  register char *Ca, *Cb;

  register heapP HBT;

  register opcodeP PCa, PCb;

  register trailP TRPa, TRPb;

  codeU CU;

  int Last_DevsTime;	/* Value of Reductions when devices where
			   checked */
  int (*f)();
  
  Last_DevsTime = Reductions;
  Last_Redo_GC = 0;

  {
    extern int BootType;

    switch (BootType) {
    case EmulationBoot:
      {
	      register heapP P;
  
        FLs_allocate((PR_Header + 2), P, HP); // AH@@@only in new emulator why?
      
        pr_init(2, P);

        *(Prcdr_PR(P)) = Prcdr;
        pr_enqueue(P, False);
        P = Args_PR(P);
        Creations++;
        *P++ = Ref_Word(HP);
        *HP++ = ZeroedWrt;
        *P = Ref_Word(HP);
      }
      *HP++ = ZeroedWrt;
      McnInP = HP;
      *HP++ = ZeroedWrt;
      McnOutP = HP;
      *HP = Var_Word((HP+1), WrtTag);
      HP++;
      *HP = Var_Word((HP-1), RoTag);
      HP++;
      break;
    case WarmBoot:
      mcn_output(BootC);
      break;
    }
  }
  asgn_trl_reset();
  chng_trl_reset();
  sus_tbl_reset();
  err_tbl_reset();
  HB = HP;
  pr_dequeue();

 dequeue_pr_label:
  deref_ptr(McnInP);
  if (!IsWrt(*McnInP)) {
    sigset_t MaskSignals, *MaskSignalsP, CurSignals, *CurSignalsP;

    MaskSignalsP = (sigset_t *) &MaskSignals;
    CurSignalsP = (sigset_t *) &CurSignals;

    sigemptyset(MaskSignalsP);
    sigaddset(MaskSignalsP, SIGINT);
    sigaddset(MaskSignalsP, SIGALRM);
    sigaddset(MaskSignalsP, SIGPIPE);
    sigprocmask(SIG_BLOCK, MaskSignalsP, CurSignalsP);

    handle_mcn_input();
    sigprocmask(SIG_SETMASK, CurSignalsP, ((sigset_t *) NULL));
  }

  if (DevsNo != 0) {
    if ((Last_DevsTime + DevsTime) <= Reductions) {
      Last_DevsTime = Reductions;
      select_devices(True);
    }
    if (QF == Nil) {
      mcn_output(IdleC);
    }
    if ((QF == Nil) && (waiting_to_devices())) {
      do {
	select_devices(False);
      }
      while ((QF == Nil) && (waiting_to_devices()));
    }
  }


  if (QF == Nil) {
    mcn_output(IdleC);
  }
  check_gc();
  if (QF == Nil) {
    CP = Nil;
    return(True);
  }
  CP = QF;
  HighQ = (HQB != Nil);
  if (HQB == QF) {
    HQB = Nil;
  }
  QF = Ref_Val(*(Next_PR(QF)));
  *(Next_PR(CP)) = Ref_Word(Nil);
#ifdef DEBUG
  if (Debug_Unset) {
    Debug_Process = False;
    Debug_Clause = False;
    Debug_Guard = False;
    Debug_Outargs = False;
    Debug_Activation = False;
    Debug_Unset = False;
  }
  if (Debug_Process) {
    fprintf(DbgFile, "Dequeued: ");
    pr_print(CP);
  }
#endif
  emulation_start();
    
 fail_pr_label:
  Failures++;
  mcn_output(FailedC);
#ifdef	DEBUG
  if (Debug_Process) {
    fprintf(DbgFile, "process failed\n");
  }
#endif
  err_tbl_reset();
  check_gc();
  pr_dequeue();

 suspend_pr_label:
  {
    register heapT Va, Vb;
    register heapP Pa, Pb, Pc;

    int PrcssWrtFlt = False;
    heapP Suspender;
      
    FLs_allocate(SR_Size, Suspender, HP);
    *(Ref_SR(Suspender)) = L_Ref_Word(CP);
    *(Next_SR(Suspender)) = Ref_Word(Nil);
    Va = L_Ref_Word(Suspender);
    do {
      Pa = Sus_Address(STP);
      Pa = (heapP) FixHighBytesP(Pa);
      Vb = *Pa;
      if (IsWrt(Vb)) {
	      if (IsZeroed(Vb)) {
	        *Pa = Var_Word(HP, WrtTag);
	        Vb = *HP = Var_Word(Pa, RoTag);
	        Pa = HP++;
	      }
	      else {
          Pa = Var_Val(Vb);
          Vb = *Pa;
	      }
      }
      if (*((heapP) Ref_SR(Var_Val(Vb))) != Va) {
        FLs_allocate(SR_Size, Pb, HP);
        *(Ref_SR(Pb)) = Va;
        *(Next_SR(Pb)) = Ref_Word(Var_Val(Vb));
        *Pa = Var_Word(Pb, RoTag);
      }
    }
    while ((--STP) != SusTbl);
    pr_hangover(Suspender);
    Suspensions++;
#ifdef	DEBUG
    if (Debug_Process) {
      fprintf(DbgFile, "process suspended\n");
    }
#endif
  }
#ifdef	DEBUG
  PrevSTP = STP;
#endif
  HB = HP;
  err_tbl_reset();
  check_gc();
  pr_dequeue();

 kernel_succeeded_label:
  PC = (opcodeP) char_offset(PC, (2 * pc_intra_offset_skip()));
  next_instr();

 kernel_suspended_label:
#ifdef	DEBUG
  if (Debug_Guard) {
    fprintf(DbgFile, "kernel suspended\n");
  }
#endif
  PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
  PC = (opcodeP) char_offset(PC, pc_intra_offset());
  next_instr();

 kernel_failed_label:
#ifdef	DEBUG
  if (Debug_Guard) {
    fprintf(DbgFile, "kernel failed\n");
  }
#endif
  PC = (opcodeP) char_offset(PC, pc_intra_offset());
  next_instr();

 emulation_start_label:
#ifdef	DEBUG
  if (Debug) {
    set_debug_flags();
    fprintf(DbgFile, "Reducing:\n");
    pr_print(CP);
  }
#endif
  load_pr_args(Ia, Pa);
  reset_time_slice();
  PC = pr_code_addr();
  next_instr();

  /* Instructions Switch */

 next_instr_label:
  PC = (opcodeP) FixHighBytesP(PC);
  switch (*PC++) {
  case 0:
    fprintf(DbgFile, "Instructions Opcode has a value of 0!\n");
    return(456);

  case deref_3:
    Va = pc_reg();
    if (!IsRef(Va)) {
      pc_reg() = Va;
    }
    else {
      deref_ref(Va, Pa);
      pc_reg() = Ref_Word(Pa);
    }
    pc_reg() = Va;
    next_instr();

  case deref_2:
    Va = pc_reg_offset();
    Vb = reg(Va);
    if (IsRef(Vb)) {
     deref_ref(Vb, Pb);
     reg(Va) = Ref_Word(Pb);
    }
    pc_reg() = Vb;
    next_instr();

  case deref_2_addr:
    Va = pc_reg();
    if (!IsRef(Va)) {
      pc_reg() = Va;
    }
    else {
      deref_ref(Va, Pa);
      pc_reg() = Ref_Word(Pa);
    }
    next_instr();

  case deref_subarg_3:
    Pa = pc_subarg_addr();
    Va = *Pa;
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    if (IsRef(Va)) {
      deref_ref(Va, Pa);
    }
    pc_reg() = Ref_Word(Pa);
    pc_reg() = Va;
    next_instr();

  case deref_subarg_2:
    Pa = pc_subarg_addr();
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    Va = *Pa;
    deref(Va, Pa);
    pc_reg() = Ref_Word(Pa);
    next_instr();

  case deref_car_3:
    Va = Off_List(*Ref_Val(pc_reg()));
    if (!IsRef(Va)) {
      pc_reg() = Va;
    }
    else {
      deref_ref(Va, Pa);
      pc_reg() = Ref_Word(Pa);
    }
    pc_reg() = Va;
    next_instr();

  case deref_car_2:
    Va = Off_List(*Ref_Val(pc_reg()));
    if (!IsRef(Va)) {
      pc_reg() = Va;
    }
    else {
      deref_ref(Va, Pa);
      pc_reg() = Ref_Word(Pa);
    }
    next_instr();

  case load_reg:
    if (Vb == 0) {
      *Pb++ = pc_reg();
    }
    else {
      *Pb++ = pc_reg() | ListFlag;
      Vb--;
    }
    next_instr();

  case load_pr_arg:
    if (Vb == 0) {
      *Pb++ = pc_cp_arg();
    }
    else {
      *Pb++ = pc_cp_arg() | ListFlag;
      Vb--;
    }
    next_instr();

  case load_reg_indirect:
    if (Vb == 0) {
      *Pb++ = *Ref_Val(pc_reg());
    }
    else {
      *Pb++ = (*Ref_Val(pc_reg())) | ListFlag;
      Vb--;
    }
    next_instr();

  case load_subarg:
    if (Vb == 0) {
      *Pb++ = pc_subarg();
    }
    else {
      *Pb++ = pc_subarg() | ListFlag;
      Vb--;
    }
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    next_instr();

  case load_ro_of_reg:
    Va = pc_reg();
    set_to_ro(Va, Pa, Pb);
    Pb++;
    next_instr();

  case load_ref_to_ro_of_reg:
    Va = pc_reg();
    set_to_ro_ref(Va, Pa);
    if (Vb == 0) {
      *Pb++ = Va;
    }
    else {
      *Pb++ = Va | ListFlag;
      Vb--;
    }
    next_instr();

  case load_car_of_reg:
    Va = pc_reg();
    set_to_car(Va);
    if (Vb == 0) {
      *Pb++ = Va;
    }
    else {
      *Pb++ = Va | ListFlag;
      Vb--;
    }
    next_instr();

  case load_ref_to_subarg:
    if (Vb == 0) {
      *Pb++ = Ref_Word(pc_subarg_addr());
    }
    else {
      *Pb++ = Ref_Word(pc_subarg_addr()) | ListFlag;
      Vb--;
    }
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    next_instr();

  case load_ro_of_subarg:
    Va = Ref_Word(pc_subarg_addr());
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    set_to_ro(Va, Pa, Pb);
    Pb++;
    next_instr();

  case load_ref_to_ro_of_subarg:
    Va = Ref_Word(pc_subarg_addr());
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    set_to_ro_ref(Va, Pa);
    if (Vb == 0) {
      *Pb++ = Va;
    }
    else {
      *Pb++ = Va | ListFlag;
      Vb--;
    }
    next_instr();

  case load_nil:
    if (Vb == 0) {
      *Pb++ = Word(0, NilTag);
    }
    else {
      *Pb++ = Word(0, NilTag) | ListFlag;
      Vb--;
    }
    next_instr();

  case load_word:
    if (Vb == 0) {
      *Pb++ = pc_word();
    }
    else {
      *Pb++ = pc_word() | ListFlag;
      Vb--;
    }
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    next_instr();

  case load_real:
    if ((Pb+1) == HP) {
      HP--;
    }
    else {
      *Pb = Ref_Word(HP);
    }
    creat_pc_double(Va);
    next_instr();

  case load_ref_to_real:
    if (Vb == 0) {
      *Pb++ = Ref_Word(HP);
    }
    else {
      *Pb++ = Ref_Word(HP) | ListFlag;
      Vb--;
    }
    creat_pc_double(Va);
    next_instr();

  case load_ref_to_string:
    if (Vb == 0) {
      *Pb++ = pc_ref();
    }
    else {
      *Pb++ = pc_ref() | ListFlag;
      Vb--;
    }
    PC = (opcodeP) char_offset(PC, pc_inter_offset_skip());
    next_instr();

  case load_we_var:
    *Pb++ = ZeroedWrt;
    next_instr();

  case load_ref_to_we_var:
    if (Vb == 0) {
      *Pb++ = Ref_Word(HP);
    }
    else {
      *Pb++ = Ref_Word(HP) | ListFlag;
      Vb--;
    }
    *HP++ = ZeroedWrt;
    next_instr();

  case allocate_var:
    pc_reg() = Ref_Word(HP);
    *HP++ = ZeroedWrt;
    next_instr();
    
  case allocate_vars:
    Ia = pc_nibble();
    for(; Ia > 0; Ia--) {
	pc_reg() = Ref_Word(HP);
	*HP++ = ZeroedWrt;
    }
    next_instr();
 
  case allocate_tuple:
    pc_reg() = Ref_Word(HP);
    Va = pc_nibble();
    *HP++ = Word(Va, TplTag);
    Pb = HP;
    HP += Va;
    Vb = 0;
    next_instr();

  case allocate_list_cell:
    pc_reg() = Ref_Word(HP);
    Pb = HP;
    HP += 2;
    Vb = 1;
    next_instr();

  case allocate_list_we:
    pc_reg() = Ref_Word(HP);
    *HP++ = Set_List(pc_reg());
    *HP++ = ZeroedWrt;
    next_instr();

  case list_assign:
    Ia = pc_reg_offset();
    Ib = pc_reg_offset();
    *Ref_Val(reg(Ib)) = Ref_Word(HP);
    *HP++ = Set_List(reg(Ia));
    reg(Ib) = Ref_Word(HP);
    *HP++ = ZeroedWrt;
    next_instr();

  case list_assign_with_check:
    Ia = pc_reg_offset();
    Ib = pc_reg_offset();
    if(reg(Ib) == Ref_Word(HP) - sizeof(heapT)) {
	*Ref_Val(reg(Ib)) = Set_List(reg(Ia));
	reg(Ib) += sizeof(heapT);
    } else {
	*Ref_Val(reg(Ib)) = Ref_Word(HP);
	*HP++ = Set_List(reg(Ia));
	reg(Ib) = Ref_Word(HP);
    }
    *HP++ = ZeroedWrt;
    next_instr();

  case allocate_listN:
    pc_reg() = Ref_Word(HP);
    Va = pc_nibble();
    Pb = HP;
    HP += Va;
    Vb = Va - 1;
    next_instr();

  case allocate_pr:
    Va = pc_nibble();
    FLs_allocate((PR_Header + Va), Pa, HP);
    pr_init(Va, Pa);
    Creations++;
    pc_reg() = Ref_Word(Pa);
    Pb = Args_PR(Pa);
    Vb = 0;
    next_instr();

  case fetch:
    pc_reg() = Ref_Word(HP);
    Pa = (heapP) pc_inter_addr();
    PC = (opcodeP) char_offset(PC, pc_inter_offset_skip());
    /* It is assumed that the melted term is not big enough
       to overflow the heap */
    Ia = Str_Words(Pa) - 1; /* null word */
    Pa = Pa + FrznHdrWords;
    Pb = HP;
    for (; Ia > 0; Ia--) {
      *HP++ = *Pa++;
    }
    Pa = Pb;
    Pb = HP;
    while (Pa < Pb) {
      switch (Tag_of(*Pa)) {
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
	*Pa = Ref_Word(((heapP) ((char *) Pa + (int) Ref_Val(*Pa))));
	Pa++;
	continue;
      case IntTag:
	Pa++;
	continue;
      case RealTag:
	Pa += 1 + realTbits/heapTbits;
	continue;
      case StrTag:
	Pa += StrHdrWords + Str_Words(Pa);
	continue;
      case NilTag:
	Pa++;
	continue;
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
	*Pa = L_Ref_Word(((heapP) ((char *) Pa + (int) Ref_Val(*Pa))));
	Pa++;
	continue;
      case L_IntTag: 
      case L_NilTag: 
      case TplTag:
	Pa++;
	continue;
      }
    }
    next_instr();

  case copy_Rs_Rd:
    Va = pc_reg();
    pc_reg() = Va;
    next_instr();

  case copy_Rs_CpId:
    Va = pc_reg();
    pc_cp_arg() = Va;
    next_instr();

  case copy_Rs_SRd:
    Va = pc_reg();
    *Ref_Val(pc_reg()) = Va;
    next_instr();

  case copy_CpIs_Rd:
    Va = pc_cp_arg();
    pc_reg() = Va;
    next_instr();

  case copy_CpIs_CpId:
    Va = pc_cp_arg();
    pc_cp_arg() = Va;
    next_instr();

  case copy_CpIs_SRd:
    Va = pc_cp_arg();
    *Ref_Val(pc_reg()) = Va;
    next_instr();

  case copy_SRs_Rd:
    Va = *Ref_Val(pc_reg());
    pc_reg() = Va;
    next_instr();

  case copy_SRs_CpId:
    Va = *Ref_Val(pc_reg());
    pc_cp_arg() = Va;
    next_instr();

  case copy_SRs_SRd:
    Va = *Ref_Val(pc_reg());
    *Ref_Val(pc_reg()) = Va;
    next_instr();

  case copy_RsIs_Rd:
    Va = pc_subarg();
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    pc_reg() = Va;
    next_instr();

  case copy_RsIs_CpId:
    Va = pc_subarg();
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    pc_cp_arg() = Va;
    next_instr();

  case copy_RsIs_SRd:
    Va = pc_subarg();
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    *Ref_Val(pc_reg()) = Va;
    next_instr();

  case copy_RRs_Rd:
    Va = pc_reg();
    set_to_ro_ref(Va, Pa);
    pc_reg() = Va;
    next_instr();

  case copy_RRs_CpId:
    Va = pc_reg();
    set_to_ro_ref(Va, Pa);
    pc_cp_arg() = Va;
    next_instr();

  case copy_RRs_SRd:
    Va = pc_reg();
    set_to_ro_ref(Va, Pa);
    *Ref_Val(pc_reg()) = Va;
    next_instr();

  case copy_CRs_Rd:
    Va = pc_reg();
    set_to_car(Va);
    pc_reg() = Va;
    next_instr();

  case copy_CRs_CpId:
    Va = pc_reg();
    set_to_car(Va);
    pc_cp_arg() = Va;
    next_instr();

  case copy_CRs_SRd:
    Va = pc_reg();
    set_to_car(Va);
    *Ref_Val(pc_reg()) = Va;
    next_instr();

  case copy_ARsIs_Rd:
    Va = Ref_Word(pc_subarg_addr());
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    pc_reg() = Va;
    next_instr();

  case copy_ARsIs_CpId:
    Va = Ref_Word(pc_subarg_addr());
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    pc_cp_arg() = Va;
    next_instr();

  case copy_ARsIs_SRd:
    Va = Ref_Word(pc_subarg_addr());
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    *Ref_Val(pc_reg()) = Va;
    next_instr();

  case copy_RARsIs_Rd:
    Va = Ref_Word(pc_subarg_addr());
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    set_to_ro_ref(Va, Pa);
    pc_reg() = Va;
    next_instr();

  case copy_RARsIs_CpId:
    Va = Ref_Word(pc_subarg_addr());
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    set_to_ro_ref(Va, Pa);
    pc_cp_arg() = Va;
    next_instr();

  case copy_RARsIs_SRd:
    Va = Ref_Word(pc_subarg_addr());
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    set_to_ro_ref(Va, Pa);
    *Ref_Val(pc_reg()) = Va;
    next_instr();

  case copy_Nil_Rd:
    pc_reg() = Word(0, NilTag);
    next_instr();

  case copy_Nil_CpId:
    pc_cp_arg() = Word(0, NilTag);
    next_instr();

  case copy_Nil_SRd:
    *Ref_Val(pc_reg()) = Word(0, NilTag);
    next_instr();

  case copy_Word_Rd:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    pc_reg() = Va;
    next_instr();

  case copy_Word_CpId:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    pc_cp_arg() = Va;
    next_instr();

  case copy_Word_SRd:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    *Ref_Val(pc_reg()) = Va;
    next_instr();

  case copy_Real_Rd:
    Va = Ref_Word(HP);
    creat_pc_double(Vb);
    pc_reg() = Va;
    next_instr();

  case copy_Real_CpId:
    Va = Ref_Word(HP);
    creat_pc_double(Vb);
    pc_cp_arg() = Va;
    next_instr();

  case copy_Real_SRd:
    Va = Ref_Word(HP);
    creat_pc_double(Vb);
    *Ref_Val(pc_reg()) = Va;
    next_instr();

  case copy_String_Rd:
    Va = pc_ref();
    PC = (opcodeP) char_offset(PC, pc_inter_offset_skip());
    pc_reg() = Va;
    next_instr();

  case copy_String_CpId:
    Va = pc_ref();
    PC = (opcodeP) char_offset(PC, pc_inter_offset_skip());
    pc_cp_arg() = Va;
    next_instr();

  case copy_String_SRd:
    Va = pc_ref();
    PC = (opcodeP) char_offset(PC, pc_inter_offset_skip());
    *Ref_Val(pc_reg()) = Va;
    next_instr();

  case copy_WeVar_Rd:
    pc_reg() = Ref_Word(HP);
    *HP++ = ZeroedWrt;
    next_instr();

  case copy_WeVar_CpId:
    pc_cp_arg() = Ref_Word(HP);
    *HP++ = ZeroedWrt;
    next_instr();

  case copy_WeVar_SRd:
    *Ref_Val(pc_reg()) = Ref_Word(HP);
    *HP++ = ZeroedWrt;
    next_instr();

  case asgn_Rs_SRd:
    Va = pc_reg();
    Pa = Ref_Val(pc_reg());
    asgn(*Pa, Pa, Va);
    next_instr();

  case asgn_CpIs_SRd:
    Va = pc_cp_arg();
    Pa = Ref_Val(pc_reg());
    asgn(*Pa, Pa, Va);
    next_instr();

  case asgn_SRs_SRd:
    Va = *Ref_Val(pc_reg());
    Pa = Ref_Val(pc_reg());
    asgn(*Pa, Pa, Va);
    next_instr();

  case asgn_RsIs_SRd:
    Va = pc_subarg();
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    Pa = Ref_Val(pc_reg());
    asgn(*Pa, Pa, Va);
    next_instr();

  case asgn_RRs_SRd:
    Va = pc_reg();
    set_to_ro_ref(Va, Pa);
    Pa = Ref_Val(pc_reg());
    asgn(*Pa, Pa, Va);
    next_instr();

  case asgn_CRs_SRd:
    Va = pc_reg();
    set_to_car(Va);
    Pa = Ref_Val(pc_reg());
    asgn(*Pa, Pa, Va);
    next_instr();

  case asgn_ARsIs_SRd:
    Va = Ref_Word(pc_subarg_addr());
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    Pa = Ref_Val(pc_reg());
    asgn(*Pa, Pa, Va);
    next_instr();

  case asgn_RARsIs_SRd:
    Va = Ref_Word(pc_subarg_addr());
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    set_to_ro_ref(Va, Pa);
    Pa = Ref_Val(pc_reg());
    asgn(*Pa, Pa, Va);
    next_instr();

  case asgn_Nil_SRd:
    Pa = Ref_Val(pc_reg());
    asgn(*Pa, Pa, Word(0, NilTag));
    next_instr();

  case asgn_Word_SRd:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Pa = Ref_Val(pc_reg());
    asgn(*Pa, Pa, Va);
    next_instr();

  case asgn_Real_SRd:
    Va = Ref_Word(HP);
    creat_pc_double(Vb);
    Pa = Ref_Val(pc_reg());
    asgn(*Pa, Pa, Va);
    next_instr();

  case asgn_String_SRd:
    Va = pc_ref();
    PC = (opcodeP) char_offset(PC, pc_inter_offset_skip());
    Pa = Ref_Val(pc_reg());
    asgn(*Pa, Pa, Va);
    next_instr();

  case asgn_WeVar_SRd:
    Pa = Ref_Val(pc_reg());
    asgn(*Pa, Pa, Ref_Word(HP));
    *HP++ = ZeroedWrt;
    next_instr();

  case assign_and_inc:
    Ia = pc_reg_offset();
    Ib = pc_reg_offset();
    *Ref_Val(reg(Ib)) = reg(Ia);
    reg(Ib) = reg(Ia)  + sizeof(heapT);
    next_instr();

  case assign_inc_com:
    Ia = pc_reg_offset();
    Ib = pc_reg_offset();
    Va = reg(Ia);
    Pb = Ref_Val(reg(Ib));
    do_commit(Va, *Pb);
    *Pb = Va;
    reg(Ib) = Va  + sizeof(heapT);
    next_instr();

  case assign_inc_com_trail:
    Ia = pc_reg_offset();
    Ib = pc_reg_offset();
    Va = reg(Ia);
    Pb = Ref_Val(reg(Ib));
    do_commit(Va, *Pb);
    *Pb = Va;
    reg(Ib) = Va  + sizeof(heapT);
    do_commit1();
    next_instr();

  case assign_Rs_SRd:
    Va = pc_reg();
    Pa = Ref_Val(pc_reg());
    *Pa = Va;
    next_instr();

  case assign_CpIs_SRd:
    Va = pc_cp_arg();
    Pa = Ref_Val(pc_reg());
    *Pa = Va;
    next_instr();

  case assign_SRs_SRd:
    Va = *Ref_Val(pc_reg());
    Pa = Ref_Val(pc_reg());
    *Pa = Va;
    next_instr();

  case assign_RsIs_SRd:
    Va = pc_subarg();
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    Pa = Ref_Val(pc_reg());
    *Pa = Va;
    next_instr();

  case assign_RRs_SRd:
    Va = pc_reg();
    set_to_ro_ref(Va, Pa);
    Pa = Ref_Val(pc_reg());
    *Pa = Va;
    next_instr();

  case assign_CRs_SRd:
    Va = pc_reg();
    set_to_car(Va);
    Pa = Ref_Val(pc_reg());
    *Pa = Va;
    next_instr();

  case assign_ARsIs_SRd:
    Va = Ref_Word(pc_subarg_addr());
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    Pa = Ref_Val(pc_reg());
    *Pa = Va;
    next_instr();

  case assign_RARsIs_SRd:
    Va = Ref_Word(pc_subarg_addr());
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    set_to_ro_ref(Va, Pa);
    Pa = Ref_Val(pc_reg());
    *Pa = Va;
    next_instr();

  case assign_Nil_SRd:
    Pa = Ref_Val(pc_reg());
    *Pa = Word(0, NilTag);
    next_instr();

  case assign_Word_SRd:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Pa = Ref_Val(pc_reg());
    *Pa = Va;
    next_instr();

  case assign_Real_SRd:
    Va = Ref_Word(HP);
    creat_pc_double(Vb);
    Pa = Ref_Val(pc_reg());
    *Pa = Va;
    next_instr();

  case assign_String_SRd:
    Va = pc_ref();
    PC = (opcodeP) char_offset(PC, pc_inter_offset_skip());
    Pa = Ref_Val(pc_reg());
    *Pa = Va;
    next_instr();

  case assign_WeVar_SRd:
    Pa = Ref_Val(pc_reg());
    *Pa = Ref_Word(HP);
    *HP++ = ZeroedWrt;
    next_instr();

  case assign_com_Rs_SRd:
    Va = pc_reg();
    Pb = Ref_Val(pc_reg());
    do_commit(Va,*Pb);
    *Pb = Va;
    next_instr();

  case assign_com_CpIs_SRd:
    Va = pc_cp_arg();
    Pb = Ref_Val(pc_reg());
    do_commit(Va, *Pb);
    *Pb = Va;
    next_instr();

  case assign_com_SRs_SRd:
    Va = *Ref_Val(pc_reg());
    Pb = Ref_Val(pc_reg());
    do_commit(Va,*Pb);
    *Pb = Va;
    next_instr();

  case assign_com_RsIs_SRd:
    Va = pc_subarg();
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    Pb = Ref_Val(pc_reg());
    do_commit(Va,*Pb);
    *Pb = Va;
    next_instr();

  case assign_com_RRs_SRd:
    Va = pc_reg();
    set_to_ro_ref(Va, Pa);
    Pb = Ref_Val(pc_reg());
    do_commit(Va,*Pb);
    *Pb = Va;
    next_instr();

  case assign_com_CRs_SRd:
    Va = pc_reg();
    set_to_car(Va);
    Pb = Ref_Val(pc_reg());
    do_commit(Va,*Pb);
    *Pb = Va;
    next_instr();

  case assign_com_ARsIs_SRd:
    Va = Ref_Word(pc_subarg_addr());
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    Pb = Ref_Val(pc_reg());
    do_commit(Va,*Pb);
    *Pb = Va;
    next_instr();

  case assign_com_RARsIs_SRd:
    Va = Ref_Word(pc_subarg_addr());
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    set_to_ro_ref(Va, Pa);
    Pb = Ref_Val(pc_reg());
    do_commit(Va,*Pb);
    *Pb = Va;
    next_instr();

  case assign_com_Nil_SRd:
    Va = Word(0,NilTag);
    Pb = Ref_Val(pc_reg());
    do_commit(Va,*Pb);
    *Pb = Va;
    next_instr();

  case assign_com_Word_SRd:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Pb = Ref_Val(pc_reg());
    do_commit(Va,*Pb);
    *Pb = Va;
    next_instr();

  case assign_com_Real_SRd:
    Va = Ref_Word(HP);
    creat_pc_double(Vb);
    Pb = Ref_Val(pc_reg());
    do_commit(Va,*Pb);
    *Pb = Va;
    next_instr();

  case assign_com_String_SRd:
    Va = pc_ref();
    PC = (opcodeP) char_offset(PC, pc_inter_offset_skip());
    Pb = Ref_Val(pc_reg());
    do_commit(Va,*Pb);
    *Pb = Va;
    next_instr();

  case assign_com_WeVar_SRd:
    Va = Ref_Word(HP);
    Pb = Ref_Val(pc_reg());
    *HP++ = ZeroedWrt;
    do_commit(Va,*Pb);
    *Pb = Va;
    next_instr();

  case assign_com_tr_Rs_SRd:
    Va = pc_reg();
    Pb = Ref_Val(pc_reg());
    do_commit(Va,*Pb);
    *Pb = Va;
    do_commit1();
    next_instr();

  case assign_com_tr_CpIs_SRd:
    Va = pc_cp_arg();
    Pb = Ref_Val(pc_reg());
    do_commit(Va,*Pb);
    *Pb = Va;
    do_commit1();
    next_instr();

  case assign_com_tr_SRs_SRd:
    Va = *Ref_Val(pc_reg());
    Pb = Ref_Val(pc_reg());
    do_commit(Va,*Pb);
    *Pb = Va;
    do_commit1();
    next_instr();

  case assign_com_tr_RsIs_SRd:
    Va = pc_subarg();
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    Pb = Ref_Val(pc_reg());
    do_commit(Va,*Pb);
    *Pb = Va;
    do_commit1();
    next_instr();

  case assign_com_tr_RRs_SRd:
    Va = pc_reg();
    set_to_ro_ref(Va, Pa);
    Pb = Ref_Val(pc_reg());
    do_commit(Va,*Pb);
    *Pb = Va;
    do_commit1();
    next_instr();

  case assign_com_tr_CRs_SRd:
    Va = pc_reg();
    set_to_car(Va);
    Pb = Ref_Val(pc_reg());
    do_commit(Va,*Pb);
    *Pb = Va;
    do_commit1();
    next_instr();

  case assign_com_tr_ARsIs_SRd:
    Va = Ref_Word(pc_subarg_addr());
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    Pb = Ref_Val(pc_reg());
    do_commit(Va,*Pb);
    *Pb = Va;
    do_commit1();
    next_instr();

  case assign_com_tr_RARsIs_SRd:
    Va = Ref_Word(pc_subarg_addr());
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    set_to_ro_ref(Va, Pa);
    Pb = Ref_Val(pc_reg());
    do_commit(Va,*Pb);
    *Pb = Va;
    do_commit1();
    next_instr();

  case assign_com_tr_Nil_SRd:
    Va = Word(0, NilTag);
    Pb = Ref_Val(pc_reg());
    do_commit(Va,*Pb);
    *Pb = Va;
    do_commit1();
    next_instr();

  case assign_com_tr_Word_SRd:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Pb = Ref_Val(pc_reg());
    do_commit(Va,*Pb);
    *Pb = Va;
    do_commit1();
    next_instr();

  case assign_com_tr_Real_SRd:
    Va = Ref_Word(HP);
    creat_pc_double(Vb);
    Pb = Ref_Val(pc_reg());
    do_commit(Va,*Pb);
    *Pb = Va;
    do_commit1();
    next_instr();

  case assign_com_tr_String_SRd:
    Va = pc_ref();
    PC = (opcodeP) char_offset(PC, pc_inter_offset_skip());
    Pb = Ref_Val(pc_reg());
    do_commit(Va,*Pb);
    *Pb = Va;
    do_commit1();
    next_instr();

  case assign_com_tr_WeVar_SRd:
    Va = Ref_Word(HP);
    Pb = Ref_Val(pc_reg());
    *HP++ = ZeroedWrt;
    do_commit(Va,*Pb);
    *Pb = Va;
    do_commit1();
    next_instr();

  case goto_there:
    PC = (opcodeP) char_offset(PC, pc_intra_offset());
    next_instr();

  case if_not_reference:
    Va = pc_reg();
    if (!IsRef(Va)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_not_variable:
    Va = pc_reg();
    if (!IsVar(Va)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_not_writable:
    Va = pc_reg();
    if (!IsWrt(Va)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_not_read_only:
    Va = pc_reg();
    if (!IsRo(Va)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_not_integer:
    Va = pc_reg();
    if (!IsInt(Va)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_not_real:
    Va = pc_reg();
    if (!IsReal(Va)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_not_string:
    Va = pc_reg();
    if (!IsStr(Va)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_not_nil:
    Va = pc_reg();
    if (!IsNil(Va)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_not_list:
    Va = pc_reg();
    if (!IsList(Va)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_not_tuple:
    Va = pc_reg();
    if (!IsTpl(Va)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_not_vector:
    Va = pc_reg();
    if (!IsVctr(Va)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_int_lt:
    Ia = pc_reg();
    Ib = pc_reg();
    if (!IsInt(((heapT) Ia)) || (Ia < Ib)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_int_bt:
    Ia = pc_reg();
    Ib = pc_reg();
    if (!IsInt(((heapT) Ia)) || (Ia > Ib)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_int_le:
    Ia = pc_reg();
    Ib = pc_reg();
    if (!IsInt(((heapT) Ia)) || (Ia <= Ib)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();
    
  case if_int_be:
    Ia = pc_reg();
    Ib = pc_reg();
    if (!IsInt(((heapT) Ia)) || (Ia >= Ib)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();
    
  case if_int_eq:
    Va = pc_reg();
    Vb = pc_reg();
    if (!IsInt(((heapT) Va)) || (Va == Vb)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_int_neq:
    Va = pc_reg();
    Vb = pc_reg();
    if (!IsInt(((heapT) Va)) || (Va != Vb)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_real_lt:
    Va = pc_reg();
    Vb = pc_reg();
    if (!(IsRef(Va) && IsReal(*Ref_Val(Va))) ||
	(real_val((Ref_Val(Va)+1)) < real_val((Ref_Val(Vb)+1)))) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_real_bt:
    Va = pc_reg();
    Vb = pc_reg();
    if (!(IsRef(Va) && IsReal(*Ref_Val(Va))) ||
	(real_val((Ref_Val(Va)+1)) > real_val((Ref_Val(Vb)+1)))) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_real_le:
    Va = pc_reg();
    Vb = pc_reg();
    if (!(IsRef(Va) && IsReal(*Ref_Val(Va))) ||
	(real_val((Ref_Val(Va)+1)) <= real_val((Ref_Val(Vb)+1)))) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_real_be:
    Va = pc_reg();
    Vb = pc_reg();
    if (!(IsRef(Va) && IsReal(*Ref_Val(Va))) ||
	(real_val((Ref_Val(Va)+1)) >= real_val((Ref_Val(Vb)+1)))) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_real_eq:
    Va = pc_reg();
    Vb = pc_reg();
    if (!(IsRef(Va) && IsReal(*Ref_Val(Va))) ||
	(real_val((Ref_Val(Va)+1)) == real_val((Ref_Val(Vb)+1)))) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_real_neq:
    Va = pc_reg();
    Vb = pc_reg();
    if (!(IsRef(Va) && IsReal(*Ref_Val(Va))) ||
	(real_val((Ref_Val(Va)+1)) != real_val((Ref_Val(Vb)+1)))) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_str_lt:
    Va = pc_reg();
    Pb = Ref_Val(pc_reg());
    if (!(IsRef(Va) && IsStr(*(Pa = Ref_Val(Va))))) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
      next_instr();
    }
    if (Str_Type(Pa) != Str_Type(Pb)) {
      if (StrOrder[Str_Type(Pa)] < StrOrder[Str_Type(Pb)]) {
	PC = (opcodeP) char_offset(PC, pc_intra_offset());
      }
      else {
	PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
      }
      next_instr();
    }
    Ia = Str_Words(Pa);
    Ib = Str_Words(Pb);
    Ic = ((Ia < Ib) ? Ia : Ib);
    Pa = Pa + 2;
    Pb = Pb + 2;
    for (; (Ic > 1) && (*Pa == *Pb) ; Ic--, Pa++, Pb++) {
    }
    if (*Pa == *Pb) {
      if (Ia < Ib) {
	PC = (opcodeP) char_offset(PC, pc_intra_offset());
      }
      else {
	PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
      }
      next_instr();
    }
    Ca = (char *) Pa;
    Cb = (char *) Pb;
    while (*Ca == *Cb) {
      Ca++; Cb++;
    }
    if (*Ca < *Cb) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_str_bt:
    Va = pc_reg();
    Pb = Ref_Val(pc_reg());
    if (!(IsRef(Va) && IsStr(*(Pa = Ref_Val(Va))))) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
      next_instr();
    }
    if (Str_Type(Pa) != Str_Type(Pb)) {
      if (StrOrder[Str_Type(Pa)] > StrOrder[Str_Type(Pb)]) {
	PC = (opcodeP) char_offset(PC, pc_intra_offset());
      }
      else {
	PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
      }
      next_instr();
    }
    Ia = Str_Words(Pa);
    Ib = Str_Words(Pb);
    Ic = ((Ia < Ib) ? Ia : Ib);
    Pa = Pa + 2;
    Pb = Pb + 2;
    for (; (Ic > 1) && (*Pa == *Pb) ; Ic--, Pa++, Pb++) {
    }
    if (*Pa == *Pb) {
      if (Ia > Ib) {
	PC = (opcodeP) char_offset(PC, pc_intra_offset());
      }
      else {
	PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
      }
      next_instr();
    }
    Ca = (char *) Pa;
    Cb = (char *) Pb;
    while (*Ca == *Cb) {
      Ca++; Cb++;
    }
    if (*Ca > *Cb) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_str_le:
    Va = pc_reg();
    Pb = Ref_Val(pc_reg());
    if (!(IsRef(Va) && IsStr(*(Pa = Ref_Val(Va))))) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
      next_instr();
    }
    if (Str_Type(Pa) != Str_Type(Pb)) {
      if (StrOrder[Str_Type(Pa)] < StrOrder[Str_Type(Pb)]) {
	PC = (opcodeP) char_offset(PC, pc_intra_offset());
      }
      else {
	PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
      }
      next_instr();
    }
    Ia = Str_Words(Pa);
    Ib = Str_Words(Pb);
    Ic = ((Ia < Ib) ? Ia : Ib);
    Pa = Pa + 2;
    Pb = Pb + 2;
    for (; (Ic > 1) && (*Pa == *Pb) ; Ic--, Pa++, Pb++) {
    }
    if (*Pa == *Pb) {
      if (Ia <= Ib) {
	PC = (opcodeP) char_offset(PC, pc_intra_offset());
      }
      else {
	PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
      }
      next_instr();
    }
    Ca = (char *) Pa;
    Cb = (char *) Pb;
    while (*Ca == *Cb) {
      Ca++; Cb++;
    }
    if (*Ca <= *Cb) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_str_be:
    Va = pc_reg();
    Pb = Ref_Val(pc_reg());
    if (!(IsRef(Va) && IsStr(*(Pa = Ref_Val(Va))))) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
      next_instr();
    }
    if (Str_Type(Pa) != Str_Type(Pb)) {
      if (StrOrder[Str_Type(Pa)] > StrOrder[Str_Type(Pb)]) {
	PC = (opcodeP) char_offset(PC, pc_intra_offset());
      }
      else {
	PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
      }
      next_instr();
    }
    Ia = Str_Words(Pa);
    Ib = Str_Words(Pb);
    Ic = ((Ia < Ib) ? Ia : Ib);
    Pa = Pa + 2;
    Pb = Pb + 2;
    for (; (Ic > 1) && (*Pa == *Pb) ; Ic--, Pa++, Pb++) {
    }
    if (*Pa == *Pb) {
      if (Ia >= Ib) {
	PC = (opcodeP) char_offset(PC, pc_intra_offset());
      }
      else {
	PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
      }
      next_instr();
    }
    Ca = (char *) Pa;
    Cb = (char *) Pb;
    while (*Ca == *Cb) {
      Ca++; Cb++;
    }
    if (*Ca >= *Cb) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_str_eq:
    Va = pc_reg();
    Pb = Ref_Val(pc_reg());
    if (!(IsRef(Va) && IsStr(*(Pa = Ref_Val(Va))))) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
      next_instr();
    }
    if (Str_Type(Pa) != Str_Type(Pb)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
      next_instr();
    }
    Ia = Str_Words(Pa);
    Ib = Str_Words(Pb);
    if (Ia != Ib) {
      next_instr();
    }
    Pa = Pa + 2;
    Pb = Pb + 2;
    for (; (Ia > 1) && (*Pa == *Pb) ; Ia--, Pa++, Pb++) {
    }
    if (*Pa == *Pb) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_str_neq:
    Va = pc_reg();
    Pb = Ref_Val(pc_reg());
    if (!(IsRef(Va) && IsStr(*(Pa = Ref_Val(Va)))) ||
	(Str_Type(Pa) != Str_Type(Pb))) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
      next_instr();
    }
    Ia = Str_Words(Pa);
    Ib = Str_Words(Pb);
    if (Ia != Ib) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
      next_instr();
    }
    Pa = Pa + 2;
    Pb = Pb + 2;
    for (; (Ia > 1) && (*Pa == *Pb) ; Ia--, Pa++, Pb++) {
    }
    if (*Pa != *Pb) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_tuple_lt:
    Va = pc_reg();
    Vb = pc_reg();
    if (!IsTpl(Va) || (Va < Vb)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_tuple_bt:
    Va = pc_reg();
    Vb = pc_reg();
    if (!IsTpl(Va) || (Va > Vb)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_tuple_le:
    Va = pc_reg();
    Vb = pc_reg();
    if (!IsTpl(Va) || (Va <= Vb)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();
    
  case if_tuple_be:
    Va = pc_reg();
    Vb = pc_reg();
    if (!IsTpl(Va) || (Va >= Vb)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();
    
  case if_tuple_eq:
    Va = pc_reg();
    Vb = pc_reg();
    if (!IsTpl(Va) || (Va == Vb)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case if_tuple_neq:
    Va = pc_reg();
    Vb = pc_reg();
    if (!IsTpl(Va) || (Va != Vb)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case switch_on_tag:
    Va = pc_reg();
    switch (Tag_of(Va)) {
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
      Va = (0x1 << RefIndex);
      break;
    case WrtTag:
      Va = (0x1 << WrtIndex);
      break;
    case RoTag:
      Va = (0x1 << RoIndex);
      break;
    case IntTag:
      Va = (0x1 << IntIndex);
      break;
    case RealTag:
      Va = (0x1 << RealIndex);
      break;
    case StrTag:
      Va = (0x1 << StrIndex);
      break;
    case NilTag:
      Va = (0x1 << NilIndex);
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
      Va = (0x1 << L_RefIndex);
      break;
    case L_IntTag: 
      Va = (0x1 << L_IntIndex);
      break;
    case L_NilTag: 
      Va = (0x1 << L_NilIndex);
      break;
    case TplTag:
      Va = (0x1 << TplIndex);
      break;
    case VctrTag:
      Va = (0x1 << VctrIndex);
      break;
    case InvldTag:
      Va = (0x1 << InvldIndex);
      break;
    }
    Vc = pc_nibble();
    Vb = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vb = Int_Val(Vb);
    if (Va & Vb) {
      for (Va = (Va >> 0x1), Vc = 0; Va != 0x1; Va = (Va >> 0x1)) {
	if (Va & Vb) {
	  Vc++;
	}
      }
    }
    PC = (opcodeP) char_offset(PC, (Vc * pc_intra_offset_skip()));
    PC = (opcodeP) char_offset(PC, pc_intra_offset());
    next_instr();

  case branch_integer:
    Ia = pc_reg();
    Ib = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Ic = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    if (IsInt(((heapT) Ia)) && (Ib <= Ia) && (Ia <= Ic)) {
      Va = Val_of(((heapT) (Ia - Ib)));
    }
    else {
      Va = Val_of(((heapT) (Ic - Ib))) + 1;
    }
    PC = (opcodeP) char_offset(PC, (Va * pc_intra_offset_skip()));
    PC = (opcodeP) char_offset(PC, pc_intra_offset());
    next_instr();

  case branch_real:
    /* Not defined yet */
    store_pr_args(Ia, Pa);
    fprintf(DbgFile, "Undefined Instruction, branch real\n");
    do_exit("Undefined Instruction Opcode", MACHINE, ErUDFDCD, True);

  case branch_tuple:
    Va = pc_reg();
    Vb = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vc = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    if (IsTpl(Va) && (Vb <= Va) && (Va <= Vc)) {
      Va = Val_of(((heapT) (Va - Vb)));
    }
    else {
      Va = Val_of(((heapT) (Vc - Vb))) + 1;
    }
    PC = (opcodeP) char_offset(PC, (Va * pc_intra_offset_skip()));
    PC = (opcodeP) char_offset(PC, pc_intra_offset());
    next_instr();

  case case_hash_integer:
    Va = pc_reg();
    Vb = pc_nibble();
    if (IsInt(Va)) {
      if ((Ia = Int_Val(Va)) < 0) {
	Ia = (0 - Ia);
      }
      Vb = Ia % Vb;
    }
    PC = (opcodeP) char_offset(PC, (Vb * pc_intra_offset_skip()));
    PC = (opcodeP) char_offset(PC, pc_intra_offset());
    next_instr();

  case case_hash_string:
    Va = pc_reg();
    Vb = pc_nibble();
/*
*/
    if (IsRef(Va) && IsStr(*Ref_Val(Va))) {
      Vb = Str_Hash(Ref_Val(Va)) % Vb;
    }
    PC = (opcodeP) char_offset(PC, (Vb * pc_intra_offset_skip()));
    PC = (opcodeP) char_offset(PC, pc_intra_offset());
/*
*/
    next_instr();

  case cmp_int_lt:
    Ia = pc_reg();
    Ib = pc_reg();
    if(Ia < Ib) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    next_instr();

  case cmp_int_bt:
    Ia = pc_reg();
    Ib = pc_reg();
    if(Ia > Ib) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    next_instr();

  case cmp_int_le:
    Ia = pc_reg();
    Ib = pc_reg();
    if(Ia <=  Ib) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    next_instr();

  case cmp_int_be:
    Ia = pc_reg();
    Ib = pc_reg();
    if(Ia >=  Ib) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    next_instr();

  case cmp_int_eq:
    Va = pc_reg();
    Vb = pc_reg();
    if(Va == Vb) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    next_instr();

  case cmp_int_ne:
    Va = pc_reg();
    Vb = pc_reg();
    if(Va != Vb) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    next_instr();

  case unify_args:
    Ia = pc_nibble();
    PCa = (opcodeP) char_offset(PC, pc_intra_offset()); /* error label */
    PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());

    for(; Ia > 0; Ia--) {
      switch (*PC++) {
      case unify_reg_reg:
	Va = pc_reg();
	Vb = pc_reg();
	break;
      case unify_reg_xreg:	/* always treated as if preceeded by & */
	Va = pc_reg();
	Vb = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	break;
      case unify_reg_axreg:
	Va = pc_reg();
	Vb = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	break;
      case unify_reg_roreg:
	Va = pc_reg();
	Vb = pc_reg();
	set_to_ro_ref(Vb, Pa);
	break;
      case unify_reg_roaxreg:
	Va = pc_reg();
	Vb = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	set_to_ro_ref(Vb, Pa);
	break;
      case unify_reg_carreg:
	Va = pc_reg();
	Vb = pc_reg();
	set_to_car(Vb);
	break;
      case unify_reg_word:
	Va = pc_reg();
	Vb = pc_word();
	PC = (opcodeP) char_offset(PC, pc_word_skip());
	break;
      case unify_reg_string:
	Va = pc_reg();
	Vb = pc_ref();
	PC = (opcodeP) char_offset(PC, pc_inter_offset_skip());
	break;
      case unify_reg_real:
	Va = pc_reg();
	Vb = Ref_Word(HP);
	creat_pc_double(Ib);
	break;
      case unify_reg_nil:
	Va = pc_reg();
	Vb = Word(0, NilTag);
	break;
      case unify_xreg_reg:
	Va = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	Vb = pc_reg();
	break;
      case unify_xreg_xreg:
	Va = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	Vb = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	break;
      case unify_xreg_axreg:
	Va = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	Vb = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	break;
      case unify_xreg_roreg:
	Va = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	Vb = pc_reg();
	set_to_ro_ref(Vb, Pa);
	break;
      case unify_xreg_roaxreg:
	Va = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	Vb = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	set_to_ro_ref(Vb, Pa);
	break;
      case unify_xreg_carreg:
	Va = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	Vb = pc_reg();
	set_to_car(Vb);
	break;
      case unify_xreg_word:
	Va = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	Vb = pc_word();
	PC = (opcodeP) char_offset(PC, pc_word_skip());
	break;
      case unify_xreg_string:
	Va = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	Vb = pc_ref();
	PC = (opcodeP) char_offset(PC, pc_inter_offset_skip());
	break;
      case unify_xreg_real:
	Va = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	Vb = Ref_Word(HP);
	creat_pc_double(Ib);
	break;
      case unify_xreg_nil:
	Va = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	Vb = Word(0, NilTag);
	break;
      default:
	store_pr_args(Ia, Pa);
	fprintf(DbgFile, "Instructions Switch for unify, Opcode = %x  ",
		*(--PC));
	do_exit("Unknown Instruction Opcode", MACHINE, ErUDFDCD, True);
      }
      if (unify(Va, Vb) == False) {
	PC = PCa;
	next_instr();
      }
    }
    next_instr();

  case decrement_2_reg:
    Va = pc_reg();
    pc_reg() = Va - (1<<TagShift);
    next_instr();

  case decrement_2_xreg:
    Va = pc_reg();
    *(pc_subarg_addr()) = Va - (1<<TagShift);
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    next_instr();

  case decrement:
    Ia = pc_reg_offset();
    reg(Ia) -= (1<<TagShift);
    next_instr();

  case decrement_and_commit:
    Va = pc_reg() - (1<<TagShift);
    Pb = Ref_Val(pc_reg());
    do_commit(Va,*Pb);
    *Pb = Va;
    next_instr();

  case decrement_pointer:
    Ia = pc_reg_offset();
    reg(Ia) -= sizeof(heapT);
    next_instr();

  case decrement_2_pointer:
    Ia = pc_reg();
    pc_reg() = Ia - sizeof(heapT);
    next_instr();

  case increment_pointer:
    Ia = pc_reg_offset();
    reg(Ia) += sizeof(heapT);
    next_instr();

  case increment_2_pointer:
    Ia = pc_reg();
    pc_reg() = Ia + sizeof(heapT);
    next_instr();

  case increment_2_reg:
    Va = pc_reg();
    pc_reg() = Va + (1<<TagShift);
    next_instr();

  case increment_2_xreg:
    Va = pc_reg();
    *(pc_subarg_addr()) = Va +(1<<TagShift);
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    next_instr();

  case increment:
    Ia = pc_reg_offset();
    reg(Ia) += (1<<TagShift);
    next_instr();

  case increment_and_commit:
    Va = pc_reg() + (1<<TagShift);
    Pb = Ref_Val(pc_reg());
    do_commit(Va,*Pb);
    *Pb = Va;
    next_instr();

  case plus_reg_reg_reg:
    Va = pc_reg();
    Vb = pc_reg();
    pc_reg() = ((heapT) ((int) Va + (int) Off_Tag(Vb)));
    next_instr();

  case plus_reg_reg_xreg:
    Va = pc_reg();
    Vb = pc_reg();
    *(pc_subarg_addr()) = ((heapT) ((int) Va + (int) Off_Tag(Vb)));
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    next_instr();

  case plus_reg_int_reg:
    Va = pc_reg();
    Vb = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    pc_reg() = ((heapT) ((int) Va + (int) Off_Tag(Vb)));
    next_instr();

  case plus_reg_int_xreg:
    Va = pc_reg();
    Vb = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    *(pc_subarg_addr()) = ((heapT) ((int) Va + (int) Off_Tag(Vb)));
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    next_instr();

  case plus_int_reg_reg:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vb = pc_reg();
    pc_reg() = ((heapT) ((int) Va + (int) Off_Tag(Vb)));
    next_instr();

  case plus_int_reg_xreg:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vb = pc_reg();
    *(pc_subarg_addr()) = ((heapT) ((int) Va + (int) Off_Tag(Vb)));
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    next_instr()

  case plus_int_int_reg:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vb = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    pc_reg() = ((heapT) ((int) Va + (int) Off_Tag(Vb)));
    next_instr();

  case plus_int_int_xreg:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vb = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    *(pc_subarg_addr()) = ((heapT) ((int) Va + (int) Off_Tag(Vb)));
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    next_instr();

  case cmt_plus_reg_reg_reg:
    Va = pc_reg();
    Vb = pc_reg();
    Pb = Ref_Val(pc_reg());
    Vc  = ((heapT) ((int) Va + (int) Off_Tag(Vb)));
    do_commit(Vc,*Pb);
    *Pb = Vc;
    next_instr();

  case cmt_plus_reg_reg_xreg:
    Va = pc_reg();
    Vb = pc_reg();
    Pb = pc_subarg_addr();
    Vc = ((heapT) ((int) Va + (int) Off_Tag(Vb)));
    do_commit(Vc,*Pb);
    *Pb = Vc;
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    next_instr();

  case cmt_plus_reg_int_reg:
    Va = pc_reg();
    Vb = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Pb = Ref_Val(pc_reg());
    Vc = ((heapT) ((int) Va + (int) Off_Tag(Vb)));
    do_commit(Vc,*Pb);
    *Pb = Vc;
    next_instr();

  case cmt_plus_reg_int_xreg:
    Va = pc_reg();
    Vb = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Pb = pc_subarg_addr();
    Vc = ((heapT) ((int) Va + (int) Off_Tag(Vb)));
    do_commit(Vc,*Pb);
    *Pb = Vc;
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    next_instr();

  case cmt_plus_int_reg_xreg:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vb = pc_reg();
    Pb = pc_subarg_addr();
    Vc = ((heapT) ((int) Va + (int) Off_Tag(Vb)));
    do_commit(Vc,*Pb);
    *Pb = Vc;
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    next_instr()

  case cmt_plus_int_int_reg:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vb = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Pb = Ref_Val(pc_reg());
    Vc = ((heapT) ((int) Va + (int) Off_Tag(Vb)));
    do_commit(Vc,*Pb);
    *Pb = Vc;
    next_instr();

  case cmt_plus_int_int_xreg:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vb = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Pb = pc_subarg_addr();
    Vc = ((heapT) ((int) Va + (int) Off_Tag(Vb)));
    do_commit(Vc,*Pb);
    *Pb = Vc;
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    next_instr();

  case plusnum_reg_reg:
    Va = pc_reg();
    Vb = pc_reg();
    deref(Va, Pa);
    deref(Vb, Pb);
    if (IsInt(Va)) {
      if (IsInt(Vb)) {
	*Ref_Val(pc_reg()) = ((heapT) ((int) Va + (int) Off_Tag(Vb)));
      }
      else {	/* assume Vb is real */
	*HP++ = Word(0, RealTag);
	real_copy_val(((realT) Int_Val(Va) + real_val((Pb+1))), HP);
	HP += realTbits/heapTbits;
	*Ref_Val(pc_reg()) = Ref_Word((HP - (1+(realTbits/heapTbits))));
      }
    } else {	/* assume Va is real */
      pc_reg() = Ref_Word(HP);
      *HP++ = Word(0, RealTag);
      if (IsInt(Vb)) {
	real_copy_val((real_val((Pa+1)) + (realT) Int_Val(Vb)), HP);
	HP += realTbits/heapTbits;
      }
      else {	/* assume Vb is also real */
	real_copy_val((real_val((Pa+1)) + real_val((Pb+1))), HP);
	HP += realTbits/heapTbits;
      }
    }

    next_instr();

  case plusnum_reg_int:
    Va = pc_reg();
    Vb = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    deref(Va, Pa);

    if (IsInt(Va)) {
	*Ref_Val(pc_reg()) = ((heapT) ((int) Va + (int) Off_Tag(Vb)));
    } else {	/* assume Va is real */
	*Ref_Val(pc_reg()) = Ref_Word(HP);
	*HP++ = Word(0, RealTag);
	real_copy_val((real_val((Pa+1)) + (realT) Int_Val(Vb)), HP);
	HP += realTbits/heapTbits;
    }

    next_instr();

  case plusnum_reg_real:
    Va = pc_reg();
    Vb = pc_double();
    PC = (opcodeP) char_offset(PC, pc_double_skip());
    deref(Va, Pa);

    if (IsInt(Va)) {
	*HP++ = Word(0, RealTag);
	real_copy_val(((realT) Int_Val(Va) + Vb), HP);
	HP += realTbits/heapTbits;
	*Ref_Val(pc_reg()) = Ref_Word((HP - (1+(realTbits/heapTbits))));
    } else {	/* assume Va is real */
	pc_reg() = Ref_Word(HP);
	*HP++ = Word(0, RealTag);
	real_copy_val((real_val((Pa+1)) + real_val((Pb+1))), HP);
	HP += realTbits/heapTbits;
    }

    next_instr();

  case plusnum_int_reg:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vb = pc_reg();
    deref(Vb, Pb);

    if (IsInt(Vb)) {
	*Ref_Val(pc_reg()) = ((heapT) ((int) Va + (int) Off_Tag(Vb)));
    } else {	/* assume Vb is real */
	*HP++ = Word(0, RealTag);
	real_copy_val(((realT) Int_Val(Va) + real_val((Pb+1))), HP);
	HP += realTbits/heapTbits;
	*Ref_Val(pc_reg()) = Ref_Word((HP - (1+(realTbits/heapTbits))));
    }

    next_instr();

  case plusnum_int_int:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vb = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    *Ref_Val(pc_reg()) = ((heapT) ((int) Va + (int) Off_Tag(Vb)));
    next_instr();


  case plusnum_int_real:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vb = pc_double();
    PC = (opcodeP) char_offset(PC, pc_double_skip());

    *HP++ = Word(0, RealTag);
    real_copy_val(((realT) Int_Val(Va) + Vb), HP);
    HP += realTbits/heapTbits;
   *Ref_Val( pc_reg()) = Ref_Word((HP - (1+(realTbits/heapTbits))));

    next_instr();

  case plusnum_real_reg:
    Va = pc_double();
    PC = (opcodeP) char_offset(PC, pc_double_skip());
    Vb = pc_reg();
    deref(Vb, Pb);

    *Ref_Val(pc_reg()) = Ref_Word(HP);
    *HP++ = Word(0, RealTag);
    if (IsInt(Vb)) {
      real_copy_val((Va + (realT) Int_Val(Vb)), HP);
      HP += realTbits/heapTbits;
    }
    else {	/* assume Vb is also real */
      real_copy_val((real_val((Pa+1)) + real_val((Pb+1))), HP);
      HP += realTbits/heapTbits;
    }

    next_instr();

  case plusnum_real_int:
    Va = pc_double();
    PC = (opcodeP) char_offset(PC, pc_double_skip());
    Vb = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());

    *Ref_Val(pc_reg()) = Ref_Word(HP);
    *HP++ = Word(0, RealTag);
    real_copy_val((Va + (realT) Int_Val(Vb)), HP);
    HP += realTbits/heapTbits;

    next_instr();

  case plusnum_real_real:
    Va = pc_double();
    PC = (opcodeP) char_offset(PC, pc_double_skip());
    Vb = pc_double();
    PC = (opcodeP) char_offset(PC, pc_double_skip());

    *Ref_Val(pc_reg()) = Ref_Word(HP);
    *HP++ = Word(0, RealTag);
    real_copy_val((Va + Vb), HP);
    HP += realTbits/heapTbits;

    next_instr();

  case cmt_plusnum_reg_reg:
    Va = pc_reg();
    Vb = pc_reg();
    deref(Va, Pa);
    deref(Vb, Pb);
    Pc = Ref_Val(pc_reg());
    if (IsInt(Va)) {
      if (IsInt(Vb)) {
	Vc = ((heapT) ((int) Va + (int) Off_Tag(Vb)));
      }
      else {	/* assume Vb is real */
	*HP++ = Word(0, RealTag);
	real_copy_val(((realT) Int_Val(Va) + real_val((Pb+1))), HP);
	HP += realTbits/heapTbits;
	Vc = Ref_Word((HP - (1+(realTbits/heapTbits))));
      }
    } else {	/* assume Va is real */
      Vc = Ref_Word(HP);
      *HP++ = Word(0, RealTag);
      if (IsInt(Vb)) {
	real_copy_val((real_val((Pa+1)) + (realT) Int_Val(Vb)), HP);
	HP += realTbits/heapTbits;
      }
      else {	/* assume Vb is also real */
	real_copy_val((real_val((Pa+1)) + real_val((Pb+1))), HP);
	HP += realTbits/heapTbits;
      }
    }
    do_commit(Vc, *Pc);
    *Pc = Vc;
    next_instr();

  case cmt_plusnum_reg_int:
    Va = pc_reg();
    Vb = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    deref(Va, Pa);
    Pc = Ref_Val(pc_reg());

    if (IsInt(Va)) {
	Vc = ((heapT) ((int) Va + (int) Off_Tag(Vb)));
    } else {	/* assume Va is real */
	Vc = Ref_Word(HP);
	*HP++ = Word(0, RealTag);
	real_copy_val((real_val((Pa+1)) + (realT) Int_Val(Vb)), HP);
	HP += realTbits/heapTbits;
    }
    do_commit(Vc, *Pc);
    *Pc = Vc;
    next_instr();

  case cmt_plusnum_reg_real:
    Va = pc_reg();
    Vb = pc_double();
    PC = (opcodeP) char_offset(PC, pc_double_skip());
    deref(Va, Pa);
    Pc = Ref_Val(pc_reg());
    if (IsInt(Va)) {
	*HP++ = Word(0, RealTag);
	real_copy_val(((realT) Int_Val(Va) + Vb), HP);
	HP += realTbits/heapTbits;
	Vc = Ref_Word((HP - (1+(realTbits/heapTbits))));
     } else {	/* assume Va is real */
        Vc = Ref_Word(HP);
	*HP++ = Word(0, RealTag);
	real_copy_val((real_val((Pa+1)) + real_val((Pb+1))), HP);
	HP += realTbits/heapTbits;
    }
    do_commit(Vc, *Pc);
    *Pc = Vc;
    next_instr();

  case cmt_plusnum_int_reg:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vb = pc_reg();
    deref(Vb, Pb);
    Pc = Ref_Val(pc_reg());
    if (IsInt(Vb)) {
	Vc = ((heapT) ((int) Va + (int) Off_Tag(Vb)));
    } else {	/* assume Vb is real */
	*HP++ = Word(0, RealTag);
	real_copy_val(((realT) Int_Val(Va) + real_val((Pb+1))), HP);
	HP += realTbits/heapTbits;
	Vc = Ref_Word((HP - (1+(realTbits/heapTbits))));
    }
    do_commit(Vc, *Pc);
    *Pc = Vc;
    next_instr();

  case cmt_plusnum_int_real:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vb = pc_double();
    PC = (opcodeP) char_offset(PC, pc_double_skip());
    Pc = Ref_Val(pc_reg());
    *HP++ = Word(0, RealTag);
    real_copy_val(((realT) Int_Val(Va) + Vb), HP);
    HP += realTbits/heapTbits;
    Vc = Ref_Word((HP - (1+(realTbits/heapTbits))));
    do_commit(Vc, *Pc);
    *Pc = Vc;
    next_instr();

  case cmt_plusnum_real_reg:
    Va = pc_double();
    PC = (opcodeP) char_offset(PC, pc_double_skip());
    Vb = pc_reg();
    deref(Vb, Pb);
    Pc = Ref_Val(pc_reg());
    Vc = Ref_Word(HP);
    *HP++ = Word(0, RealTag);
    if (IsInt(Vb)) {
      real_copy_val((Va + (realT) Int_Val(Vb)), HP);
      HP += realTbits/heapTbits;
    }
    else {	/* assume Vb is also real */
      real_copy_val((real_val((Pa+1)) + real_val((Pb+1))), HP);
      HP += realTbits/heapTbits;
    }
    do_commit(Vc, *Pc);
    *Pc = Vc;
    next_instr();

  case cmt_plusnum_real_int:
    Va = pc_double();
    PC = (opcodeP) char_offset(PC, pc_double_skip());
    Vb = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Pc = Ref_Val(pc_reg());
    Vc = Ref_Word(HP);
    *HP++ = Word(0, RealTag);
    real_copy_val((Va + (realT) Int_Val(Vb)), HP);
    HP += realTbits/heapTbits;
    do_commit(Vc, *Pc);
    *Pc = Vc;
    next_instr();

  case cmt_plusnum_real_real:
    Va = pc_double();
    PC = (opcodeP) char_offset(PC, pc_double_skip());
    Vb = pc_double();
    PC = (opcodeP) char_offset(PC, pc_double_skip());
    Pc = Ref_Val(pc_reg());
    Vc = Ref_Word(HP);
    *HP++ = Word(0, RealTag);
    real_copy_val((Va + Vb), HP);
    HP += realTbits/heapTbits;
    do_commit(Vc, *Pc);
    *Pc = Vc;
    next_instr();

  case diff_reg_reg_reg:
    Va = pc_reg();
    Vb = pc_reg();
    
    pc_reg() = ((heapT) ((int) Va - (int) Off_Tag(Vb)));
    next_instr();

  case diff_reg_reg_xreg:
    Va = pc_reg();
    Vb = pc_reg();
    *(pc_subarg_addr()) = ((heapT) ((int) Va - (int) Off_Tag(Vb)));
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    next_instr();

  case diff_reg_int_reg:
    Va = pc_reg();
    Vb = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    pc_reg() = ((heapT) ((int) Va - (int) Off_Tag(Vb)));
    next_instr();

  case diff_reg_int_xreg:
    Va = pc_reg();
    Vb = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    *(pc_subarg_addr()) = ((heapT) ((int) Va - (int) Off_Tag(Vb)));
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    next_instr();

  case diff_int_reg_reg:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vb = pc_reg();
    pc_reg() = ((heapT) ((int) Va - (int) Off_Tag(Vb)));
    next_instr();

  case diff_int_reg_xreg:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vb = pc_reg();
    *(pc_subarg_addr()) = ((heapT) ((int) Va - (int) Off_Tag(Vb)));
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    next_instr();

  case diff_int_int_reg:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vb = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    pc_reg() = ((heapT) ((int) Va - (int) Off_Tag(Vb)));
    next_instr();

  case diff_int_int_xreg:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vb = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    *(pc_subarg_addr()) = ((heapT) ((int) Va - (int) Off_Tag(Vb)));
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    next_instr();

  case cmt_diff_reg_reg_reg:
    Va = pc_reg();
    Vb = pc_reg();
    Pb = Ref_Val(pc_reg());
    Vc  = ((heapT) ((int) Va - (int) Off_Tag(Vb)));
    do_commit(Vc,*Pb);
    *Pb = Vc;
    next_instr();

  case cmt_diff_reg_reg_xreg:
    Va = pc_reg();
    Vb = pc_reg();
    Pb = pc_subarg_addr();
    Vc = ((heapT) ((int) Va - (int) Off_Tag(Vb)));
    do_commit(Vc,*Pb);
    *Pb = Vc;
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    next_instr();

  case cmt_diff_reg_int_reg:
    Va = pc_reg();
    Vb = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Pb = Ref_Val(pc_reg());
    Vc = ((heapT) ((int) Va - (int) Off_Tag(Vb)));
    do_commit(Vc,*Pb);
    *Pb = Vc;
    next_instr();

  case cmt_diff_reg_int_xreg:
    Va = pc_reg();
    Vb = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Pb = pc_subarg_addr();
    Vc = ((heapT) ((int) Va - (int) Off_Tag(Vb)));
    do_commit(Vc,*Pb);
    *Pb = Vc;
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    next_instr();

  case cmt_diff_int_reg_xreg:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vb = pc_reg();
    Pb = pc_subarg_addr();
    Vc = ((heapT) ((int) Va - (int) Off_Tag(Vb)));
    do_commit(Vc,*Pb);
    *Pb = Vc;
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    next_instr()

  case cmt_diff_int_int_reg:
  case cmt_diffnum_int_int:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vb = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Pb = Ref_Val(pc_reg());
    Vc = ((heapT) ((int) Va - (int) Off_Tag(Vb)));
    do_commit(Vc,*Pb);
    *Pb = Vc;
    next_instr();

  case cmt_diff_int_int_xreg:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vb = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Pb = pc_subarg_addr();
    Vc = ((heapT) ((int) Va - (int) Off_Tag(Vb)));
    do_commit(Vc,*Pb);
    *Pb = Vc;
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    next_instr();

  case diffnum_reg_reg:
    Va = pc_reg();
    Vb = pc_reg();
    deref(Va, Pa);
    deref(Vb, Pb);
    if (IsInt(Va)) {
      if (IsInt(Vb)) {
        *Ref_Val(pc_reg()) = ((heapT) ((int) Va - (int) Off_Tag(Vb)));
      }
      else {    /* assume Vb is real */
        *HP++ = Word(0, RealTag);
        real_copy_val(((realT) Int_Val(Va) - real_val((Pb+1))), HP);
        HP += realTbits/heapTbits;
        *Ref_Val(pc_reg()) = Ref_Word((HP - (1+(realTbits/heapTbits))));
      }
    } else {    /* assume Va is real */
      *Ref_Val(pc_reg()) = Ref_Word(HP);
      *HP++ = Word(0, RealTag);
      if (IsInt(Vb)) {
        real_copy_val((real_val((Pa+1)) - (realT) Int_Val(Vb)), HP);
        HP += realTbits/heapTbits;
      }
      else {    /* assume Vb is also real */
        real_copy_val((real_val((Pa+1)) - real_val((Pb+1))), HP);
        HP += realTbits/heapTbits;
      }
    }

    next_instr();

  case diffnum_reg_int:
    Va = pc_reg();
    Vb = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    deref(Va, Pa);

    if (IsInt(Va)) {
        *Ref_Val(pc_reg()) = ((heapT) ((int) Va - (int) Off_Tag(Vb)));
    } else {    /* assume Va is real */
        *Ref_Val(pc_reg()) = Ref_Word(HP);
        *HP++ = Word(0, RealTag);
        real_copy_val((real_val((Pa+1)) - (realT) Int_Val(Vb)), HP);
        HP += realTbits/heapTbits;
    }

    next_instr();

  case diffnum_reg_real:
    Va = pc_reg();
    Vb = pc_double();
    PC = (opcodeP) char_offset(PC, pc_double_skip());
    deref(Va, Pa);

    if (IsInt(Va)) {
        *HP++ = Word(0, RealTag);
        real_copy_val(((realT) Int_Val(Va) - Vb), HP);
        HP += realTbits/heapTbits;
       *Ref_Val(pc_reg()) = Ref_Word((HP - (1+(realTbits/heapTbits))));
    } else {    /* assume Va is real */
        *Ref_Val(pc_reg()) = Ref_Word(HP);
        *HP++ = Word(0, RealTag);
        real_copy_val((real_val((Pa+1)) - Vb), HP);
        HP += realTbits/heapTbits;
    }

    next_instr();

  case diffnum_int_reg:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vb = pc_reg();
    deref(Vb, Pb);

    if (IsInt(Vb)) {
        *Ref_Val(pc_reg()) = ((heapT) ((int) Va - (int) Off_Tag(Vb)));
    } else {    /* assume Vb is real */
        *HP++ = Word(0, RealTag);
        real_copy_val(((realT) Int_Val(Va) - real_val((Pb+1))), HP);
        HP += realTbits/heapTbits;
        *Ref_Val(pc_reg()) = Ref_Word((HP - (1+(realTbits/heapTbits))));
    }

    next_instr();

  case diffnum_int_int:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vb = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    *Ref_Val(pc_reg()) = ((heapT) ((int) Va - (int) Off_Tag(Vb)));
    next_instr();

  case diffnum_int_real:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vb = pc_double();
    PC = (opcodeP) char_offset(PC, pc_double_skip());

    *HP++ = Word(0, RealTag);
    real_copy_val(((realT) Int_Val(Va) - Vb), HP);
    HP += realTbits/heapTbits;
    *Ref_Val(pc_reg()) = Ref_Word((HP - (1+(realTbits/heapTbits))));

    next_instr();

  case diffnum_real_reg:
    Va = pc_double();
    PC = (opcodeP) char_offset(PC, pc_double_skip());
    Vb = pc_reg();
    deref(Vb, Pb);

    *Ref_Val(pc_reg()) = Ref_Word(HP);
    *HP++ = Word(0, RealTag);
    if (IsInt(Vb)) {
      real_copy_val((Va - (realT) Int_Val(Vb)), HP);
      HP += realTbits/heapTbits;
    }
    else {    /* assume Vb is also real */
      real_copy_val((Va - real_val((Pb+1))), HP);
      HP += realTbits/heapTbits;
    }

    next_instr();

  case diffnum_real_real:
    Va = pc_double();
    PC = (opcodeP) char_offset(PC, pc_double_skip());
    Vb = pc_double();
    PC = (opcodeP) char_offset(PC, pc_double_skip());

    *Ref_Val(pc_reg()) = Ref_Word(HP);
    *HP++ = Word(0, RealTag);
    real_copy_val((Va - Vb), HP);
    HP += realTbits/heapTbits;

    next_instr();

  case cmt_diffnum_reg_reg:
    Va = pc_reg();
    Vb = pc_reg();
    deref(Va, Pa);
    deref(Vb, Pb);
    Pc = Ref_Val(pc_reg());
    if (IsInt(Va)) {
      if (IsInt(Vb)) {
        Vc = ((heapT) ((int) Va - (int) Off_Tag(Vb)));
      }
      else {    /* assume Vb is real */
        *HP++ = Word(0, RealTag);
        real_copy_val(((realT) Int_Val(Va) - real_val((Pb+1))), HP);
        HP += realTbits/heapTbits;
        Vc = Ref_Word((HP - (1+(realTbits/heapTbits))));
      }
    } else {    /* assume Va is real */
      Vc = Ref_Word(HP);
      *HP++ = Word(0, RealTag);
      if (IsInt(Vb)) {
        real_copy_val((real_val((Pa+1)) - (realT) Int_Val(Vb)), HP);
        HP += realTbits/heapTbits;
      }
      else {    /* assume Vb is also real */
        real_copy_val((real_val((Pa+1)) - real_val((Pb+1))), HP);
        HP += realTbits/heapTbits;
      }
    }
    do_commit(Vc, *Pc);
    *Pc = Vc;
    next_instr();

  case cmt_diffnum_reg_int:
    Va = pc_reg();
    Vb = pc_word();
    Pc = Ref_Val(pc_reg());
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    deref(Va, Pa);

    if (IsInt(Va)) {
        Vc = ((heapT) ((int) Va - (int) Off_Tag(Vb)));
    } else {    /* assume Va is real */
        Vc = Ref_Word(HP);
        *HP++ = Word(0, RealTag);
        real_copy_val((real_val((Pa+1)) - (realT) Int_Val(Vb)), HP);
        HP += realTbits/heapTbits;
    }
    do_commit(Vc, *Pc);
    *Pc = Vc;
    next_instr();

  case cmt_diffnum_reg_real:
    Va = pc_reg();
    Vb = pc_double();
    PC = (opcodeP) char_offset(PC, pc_double_skip());
    deref(Va, Pa);
    Pc = Ref_Val(pc_reg());

    if (IsInt(Va)) {
        *HP++ = Word(0, RealTag);
        real_copy_val(((realT) Int_Val(Va) - Vb), HP);
        HP += realTbits/heapTbits;
        Vc = Ref_Word((HP - (1+(realTbits/heapTbits))));
    } else {    /* assume Va is real */
        Vc = Ref_Word(HP);
        *HP++ = Word(0, RealTag);
        real_copy_val((real_val((Pa+1)) - Vb), HP);
        HP += realTbits/heapTbits;
    }
    do_commit(Vc, *Pc);
    *Pc = Vc;
    next_instr();

  case cmt_diffnum_int_reg:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vb = pc_reg();
    deref(Vb, Pb);
    Pc = Ref_Val(pc_reg());

    if (IsInt(Vb)) {
        Vc = ((heapT) ((int) Va - (int) Off_Tag(Vb)));
    } else {    /* assume Vb is real */
        *HP++ = Word(0, RealTag);
        real_copy_val(((realT) Int_Val(Va) - real_val((Pb+1))), HP);
        HP += realTbits/heapTbits;
        Vc = Ref_Word((HP - (1+(realTbits/heapTbits))));
    }
    do_commit(Vc, *Pc);
    *Pc = Vc;
    next_instr();

  case cmt_diffnum_int_real:
    Va = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vb = pc_double();
    PC = (opcodeP) char_offset(PC, pc_double_skip());
    Pc = Ref_Val(pc_reg());
    *HP++ = Word(0, RealTag);
    real_copy_val(((realT) Int_Val(Va) - Vb), HP);
    HP += realTbits/heapTbits;
    Vc = Ref_Word((HP - (1+(realTbits/heapTbits))));
    do_commit(Vc, *Pc);
    *Pc = Vc;
    next_instr();

  case cmt_diffnum_real_reg:
    Va = pc_double();
    PC = (opcodeP) char_offset(PC, pc_double_skip());
    Vb = pc_reg();
    deref(Vb, Pb);
    Pc = Ref_Val(pc_reg());
    Vc = Ref_Word(HP);
    *HP++ = Word(0, RealTag);
    if (IsInt(Vb)) {
      real_copy_val((Va - (realT) Int_Val(Vb)), HP);
      HP += realTbits/heapTbits;
    }
    else {    /* assume Vb is also real */
      real_copy_val((Va - real_val((Pb+1))), HP);
      HP += realTbits/heapTbits;
    }
    do_commit(Vc, *Pc);
    *Pc = Vc;
    next_instr();

  case cmt_diffnum_real_real:
    Va = pc_double();
    PC = (opcodeP) char_offset(PC, pc_double_skip());
    Vb = pc_double();
    PC = (opcodeP) char_offset(PC, pc_double_skip());
    Pc = Ref_Val(pc_reg());
    Vc = Ref_Word(HP);
    *HP++ = Word(0, RealTag);
    real_copy_val((Va - Vb), HP);
    HP += realTbits/heapTbits;
    do_commit(Vc, *Pc);
    *Pc = Vc;
    next_instr();

  case enqueue:
    Pa = Ref_Val(pc_reg());
    PCa = pc_inter_addr();
    PC = (opcodeP) char_offset(PC, pc_inter_offset_skip());
    *Prcdr_PR(Pa) = Ref_Word(PCa - (CtlHdrWords*(heapTbits/opcodeTbits)));
    pr_enqueue(Pa, HighQ);
    next_instr();
    
  case execute:
    increment_reductions();
    PCa = pc_inter_addr();
    PC = (opcodeP) char_offset(PC, pc_inter_offset_skip());
    *Prcdr_PR(CP) = Ref_Word(PCa - (CtlHdrWords*(heapTbits/opcodeTbits)));
    HB = HP;
    asgn_trl_reset();
    chng_trl_reset();
    sus_tbl_reset();
    err_tbl_reset();
    if (ended_heap(HP)) {
      PCa = (opcodeP) Ref_Val(*Prcdr_PR(CP));
      store_pr_args(Ia, Pa);
      do_gc();
      load_pr_args(Ia, Pa);
      PC = ((opcodeP) Ref_Val(*Prcdr_PR(CP))) + (PC - PCa);
    }
    if (ended_time_slice()) {
      store_pr_args(Ia, Pa);
      Switches++;
      pr_enqueue(CP, HighQ);
      CP = Nil;
#ifdef	DEBUG
      if (Debug) {
	unset_debug_flags();
      }
#endif
      pr_dequeue();
    }
#ifdef DEBUG
    store_pr_args(Ia, Pa);
    if (Debug_Process) {
      fprintf(DbgFile, "Iterating: ");
      pr_print(CP);
    }
#endif
    reduce_time_slice();
    PC = (opcodeP) char_offset(PC, pc_inter_offset());
    next_instr();

  case execute1:
    increment_reductions();
    PCa = pc_inter_addr();
    *Prcdr_PR(CP) = Ref_Word(PCa - (CtlHdrWords*(heapTbits/opcodeTbits)));
    HB = HP;
    asgn_trl_reset();
    chng_trl_reset();
    sus_tbl_reset();
    err_tbl_reset();
    if (ended_heap(HP)) {
      PCa = (opcodeP) Ref_Val(*Prcdr_PR(CP));
      store_pr_args(Ia, Pa);
      do_gc();
      load_pr_args(Ia, Pa);
      PC = ((opcodeP) Ref_Val(*Prcdr_PR(CP))) + (PC - PCa);
    }
    if (ended_time_slice()) {
      store_pr_args(Ia, Pa);
      Switches++;
      pr_enqueue(CP, HighQ);
      CP = Nil;
#ifdef	DEBUG
      if (Debug) {
	unset_debug_flags();
      }
#endif
      pr_dequeue();
    }
#ifdef DEBUG
    store_pr_args(Ia, Pa);
    if (Debug_Process) {
      fprintf(DbgFile, "Iterating: ");
      pr_print(CP);
    }
#endif
    reduce_time_slice();
    PC = (opcodeP) char_offset(PC, pc_inter_offset());
    next_instr();

  case execute2:
    increment_reductions();
    PCb = pc_inter_addr();
    PC = (opcodeP) char_offset(PC, pc_inter_offset_skip());
    PCa = pc_inter_addr();
    PC = (opcodeP) char_offset(PC, -(pc_inter_offset_skip()));
    *Prcdr_PR(CP) = Ref_Word(PCa - (CtlHdrWords*(heapTbits/opcodeTbits)));
    HB = HP;
    asgn_trl_reset();
    chng_trl_reset();
    sus_tbl_reset();
    err_tbl_reset();
    if (ended_heap(HP)) {
      PCa = (opcodeP) Ref_Val(*Prcdr_PR(CP));
      store_pr_args(Ia, Pa);
      do_gc();
      load_pr_args(Ia, Pa);
      PC = ((opcodeP) Ref_Val(*Prcdr_PR(CP))) + (PC - PCa);
    }
    if (ended_time_slice()) {
      store_pr_args(Ia, Pa);
      Switches++;
      pr_enqueue(CP, HighQ);
      CP = Nil;
#ifdef	DEBUG
      if (Debug) {
      	unset_debug_flags();
      }
#endif
      pr_dequeue();
    }
#ifdef DEBUG
    store_pr_args(Ia, Pa);
    if (Debug_Process) {
      fprintf(DbgFile, "Going To: ");
      pr_print(CP);
    }
#endif
    reduce_time_slice();
    PC = (opcodeP) char_offset(PC, pc_inter_offset());
    next_instr();

  case iterate:
    HB = HP;
    asgn_trl_reset();
    chng_trl_reset();
    sus_tbl_reset();
    err_tbl_reset();
    increment_reductions();
    if (ended_heap(HP)) {
      PCa = (opcodeP) Ref_Val(*Prcdr_PR(CP));
      store_pr_args(Ia, Pa);
      do_gc();
      load_pr_args(Ia, Pa);
      PC = ((opcodeP) Ref_Val(*Prcdr_PR(CP))) + (PC - PCa);
    }
    if (ended_time_slice()) {
      store_pr_args(Ia, Pa);
      Switches++;
      pr_enqueue(CP, HighQ);
      CP = Nil;
#ifdef	DEBUG
      if (Debug) {
	      unset_debug_flags();
      }
#endif
      pr_dequeue();
    }
#ifdef DEBUG
    store_pr_args(Ia, Pa);
    if (Debug_Process) {
      fprintf(DbgFile, "Iterating: ");
      pr_print(CP);
    }
#endif
    reduce_time_slice();
    PC = (opcodeP) char_offset(PC, pc_intra_offset());
    next_instr();

  case iterate1:
    HB = HP;
    asgn_trl_reset();
    chng_trl_reset();
    sus_tbl_reset();
    err_tbl_reset();
    increment_reductions();
    if (ended_heap(HP)) {
      PCa = (opcodeP) Ref_Val(*Prcdr_PR(CP));
      store_pr_args(Ia, Pa);
      do_gc();
      load_pr_args(Ia, Pa);
      PC = ((opcodeP) Ref_Val(*Prcdr_PR(CP))) + (PC - PCa);
    }
    if (ended_time_slice()) {
      store_pr_args(Ia, Pa);
      Switches++;
      pr_enqueue(CP, HighQ);
      CP = Nil;
#ifdef	DEBUG
      if (Debug) {
	unset_debug_flags();
      }
#endif
      pr_dequeue();
    }
#ifdef DEBUG
    store_pr_args(Ia, Pa);
    if (Debug_Process) {
      fprintf(DbgFile, "Going To: ");
      pr_print(CP);
    }
#endif
    reduce_time_slice();
    PC = pc_inter_addr();
    next_instr();

  case halt:
#ifdef	DEBUG
    if (Debug_Process) {
      fprintf(DbgFile, "process halted\n");
    }
#endif
    HB = HP;
    asgn_trl_reset();
    chng_trl_reset();
    sus_tbl_reset();
    err_tbl_reset();
    Ia = (Arity_of(*CP)+1);
    FLs_deallocate(Ia, CP);
    CP = Nil;
    Terminations++;
    increment_reductions();
    check_gc();
#ifdef	DEBUG
    if (Debug) {
      unset_debug_flags();
    }
#endif
    pr_dequeue();

  case commit1:
    /* ???? temporary - until ctl adjusts itself to new vars */
    PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    do_commit1();	/* FALLTHRU */
  case commit0:
    do_commit0();
    next_instr();
  case commit_nolabel:
    do_commit1();
    next_instr();

  case set_cp_arity:
    Ia = (Arity_of(*CP)+1);
    FLs_deallocate(Ia, CP);
    Ia = pc_nibble();
    FLs_allocate((PR_Header + Ia), CP, HP);
    pr_init(Ia, CP);
    next_instr();

  case suspend2:
    store_pr_args(Ia, Pa);
    end_of_branch();
    if (sus_tbl_empty()) {
      PC = (opcodeP) char_offset(PC, pc_inter_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_inter_offset_skip());
      PC = (opcodeP) char_offset(PC, pc_inter_offset());
    }
    next_instr();

  case suspend1:
    store_pr_args(Ia, Pa);
    end_of_branch();
    if (sus_tbl_empty()) {
      PC = (opcodeP) char_offset(PC, pc_inter_offset());
      next_instr();
    }
#ifdef	DEBUG
    if (Debug) {
      unset_debug_flags();
    }
#endif
    pr_suspend();

  case suspend0:
    store_pr_args(Ia, Pa);
    end_of_branch();
#ifdef	DEBUG
    if (Debug) {
      unset_debug_flags();
    }
#endif
    if (sus_tbl_empty()) {
      pr_fail();
    }
    pr_suspend();

  case suspend_on:
    Pa = Ref_Val(pc_reg());
    sus_tbl_add(Pa);
    next_instr();
    
  case set_HBT:
    HBT = HP;
    next_instr();

  case undo:
    HP = HBT;
    asgn_trl_undo(AsgnTrl);
    chng_trl_undo(ChngTrl);
    next_instr();

  case drf2_if_not_nil:
    Va = pc_reg_offset();
    Vb = reg(Va);
    if (IsRef(Vb)) {
      deref_ref(Vb, Pb);
      reg(Va) = Ref_Word(Pb);
    }
    pc_reg() = Vb;
    if (IsNil(Vb)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    next_instr();

  case drf2_if_not_list:
    Va = pc_reg_offset();
    Vb = reg(Va);
    if (IsRef(Vb)) {
      deref_ref(Vb, Pb);
      reg(Va) = Ref_Word(Pb);
    }
    pc_reg() = Vb;
    if (IsList(Vb)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    next_instr();

  case deref_list:
    Va = pc_reg_offset();
    Vb = reg(Va);
    if( IsInt(Vb) || IsNil(Vb)) {
      pc_reg() = Vb;
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    } else {

    deref_ref(Vb, Pb);
    if (IsList(Vb)) {
      reg(Va) = Ref_Word(Cdr(Pb));
      pc_reg() = Off_List(Vb);
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    else {
      reg(Va) = Ref_Word(Pb);
      pc_reg() = Vb;
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    }
    next_instr();

  case deref_list3:
    Vb = pc_reg();
    Va = pc_reg_offset();
    reg(Va) = Vb;
    if( IsInt(Vb) || IsNil(Vb)) {
      pc_reg() = Vb;
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    } else {
    deref_ref(Vb, Pb);
    if (IsList(Vb)) {
      reg(Va) = Ref_Word(Cdr(Pb));
      pc_reg() = Off_List(Vb);
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    else {
      reg(Va) = Ref_Word(Pb);
      pc_reg() = Vb;
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    }
    next_instr();

  case deref_car_list:
    Vb = Off_List(*Ref_Val(pc_reg()));
    if( IsInt(Vb) || IsNil(Vb)) {
      pc_reg() = Vb;
      pc_reg() = Vb;	/* both registers get the value */
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    } else {

    deref_ref(Vb, Pb);
    if (IsList(Vb)) {
      pc_reg() = Ref_Word(Cdr(Pb));
      pc_reg() = Off_List(Vb);
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    else {
      pc_reg() = Ref_Word(Pb);
      pc_reg() = Vb;
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    }
    next_instr();

  case deref_sub_list:
    Pb = pc_subarg_addr();
    Vb = *Pb;
    PC = (opcodeP) char_offset(PC, pc_subarg_skip());
    deref(Vb, Pb);
    if (IsList(Vb)) {
      pc_reg() = Ref_Word(Cdr(Pb));
      pc_reg() = Off_List(Vb);
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    else {
      pc_reg() = Ref_Word(Pb);
      pc_reg() = Vb;
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    next_instr();

  case deref_integer1:
    Va = pc_reg_offset();
    if (IsInt(reg(Va))) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip() +
				pc_intra_offset_skip());
    }
    else {
      Vb = reg(Va);
      Pb = Ref_Val(Vb);
      if (IsRef(Vb)) {
	deref_ref(Vb, Pb);
      }
      if(IsInt(Vb)) {
	reg(Va) = Vb;
	PC = (opcodeP) char_offset(PC, pc_intra_offset_skip() +
				pc_intra_offset_skip());
      } 
      else {
        reg(Va) = Ref_Word(Pb);
        if(IsVar(Vb)) {
	  sus_tbl_add(Pb);
	  PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
	  PC = (opcodeP) char_offset(PC, pc_intra_offset());
        } 
	else {
          PC = (opcodeP) char_offset(PC, pc_intra_offset());
        }
      }
    }
    next_instr();

  case load_car:
    Va = Off_List(*Ref_Val(pc_reg()));
    pc_reg() = Va;
    next_instr();

  case deref_vars:
    Ia = Ib = pc_nibble();	/* count of number of registers */


#define MAXNUMVARS 10
#ifdef DEBUG
    if(Ib > MAXNUMVARS) {
      fprintf(DbgFile, "Too many vars (%d) in deref_vars: only %s allowed\n",
	      Ib, MAXNUMVARS);
    }
#endif
    	
    {heapP addrs[MAXNUMVARS]; int skip_label = True; int Id;
    for(; Ia > 0; Ia--) {
      Va = pc_reg_offset();
      Vb = reg(Va);
      if (IsRef(Vb)) {
        deref_ref(Vb, Pb);
        reg(Va) = Ref_Word(Pb);
      }
      if( IsVar(Vb) && (skip_label == True)) {
    	Ic = Ib - Ia;
	addrs[Ic] = Pb;
      	for(Id = 0; Id < Ic; Id++) {
	  if(addrs[Id] == Pb) {
		skip_label = False;
		break;
	  }
	}
      } else skip_label = False;
    }

    if(skip_label == True) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    } else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    }
    next_instr();

  case deref_var1:	/* NEW: optimization of deref_vars for Ia = 1 */
    Va = pc_reg_offset();
    Vb = reg(Va);
    if (IsRef(Vb)) {
	deref_ref(Vb, Pb);
        reg(Va) = Ref_Word(Pb);
    }
    if(IsVar(Vb)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    } else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    next_instr();

  case drf2_if_int_neq:
    Va = pc_reg_offset();
    Vb = reg(Va);
    if (IsRef(Vb)) {
      deref_ref(Vb, Pb);
      reg(Va) = Ref_Word(Pb);
    }
    pc_reg() = Vb;
    if (!IsInt(Vb) || (Vb != pc_word())) {
      PC = (opcodeP) char_offset(PC, pc_word_skip());
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_word_skip() + pc_intra_offset_skip());
    }
    next_instr();

  case drf2_if_real_neq:
    Va = pc_reg_offset();
    Vb = reg(Va);
    if (IsRef(Vb)) {
      deref_ref(Vb, Pb);
      reg(Va) = Ref_Word(Pb);
    }
    pc_reg() = Vb;
    copy_pc_double(Va);
    if (!IsReal(Vb) ||
	(real_val((Pb+1)) != CU.D)) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case drf2_if_str_neq:
    Va = pc_reg_offset();
    Vb = reg(Va);
    if (IsRef(Vb)) {
      deref_ref(Vb, Pb);
      reg(Va) = Ref_Word(Pb);
    }
    pc_reg() = Vb;
    Pa = (heapP) pc_inter_addr();
    PC = (opcodeP) char_offset(PC, pc_inter_offset_skip());
    if (!IsStr(Vb) || (Str_Type(Pa) != Str_Type(Pb))) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
      next_instr();
    }
    Ia = Str_Words(Pa);
    Ib = Str_Words(Pb);
    if (Ia != Ib) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
      next_instr();
    }
    Pa = Pa + 2;
    Pb = Pb + 2;
    for (; (Ia > 1) && (*Pa == *Pb) ; Ia--, Pa++, Pb++) {
    }
    if (*Pa != *Pb) {
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case drf2_if_tuple_neq:
    Va = pc_reg_offset();
    Vb = reg(Va);
    if (IsRef(Vb)) {
      deref_ref(Vb, Pb);
      reg(Va) = Ref_Word(Pb);
    }
    pc_reg() = Vb;
    if (!IsTpl(Vb) || (Vb != pc_word())) {
      PC = (opcodeP) char_offset(PC, pc_word_skip());
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_word_skip() + pc_intra_offset_skip());
    }
    next_instr();

  case drf2_switch_on_tag:
    Va = pc_reg_offset();
    Vb = reg(Va);
    if (IsRef(Vb)) {
      deref_ref(Vb, Pb);
      reg(Va) = Ref_Word(Pb);
    }
    pc_reg() = Vb;
    switch (Tag_of(Vb)) {
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
      Va = (0x1 << RefIndex);
      break;
    case WrtTag:
      Va = (0x1 << WrtIndex);
      break;
    case RoTag:
      Va = (0x1 << RoIndex);
      break;
    case IntTag:
      Va = (0x1 << IntIndex);
      break;
    case RealTag:
      Va = (0x1 << RealIndex);
      break;
    case StrTag:
      Va = (0x1 << StrIndex);
      break;
    case NilTag:
      Va = (0x1 << NilIndex);
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
      Va = (0x1 << L_RefIndex);
      break;
    case L_IntTag: 
      Va = (0x1 << L_IntIndex);
      break;
    case L_NilTag: 
      Va = (0x1 << L_NilIndex);
      break;
    case TplTag:
      Va = (0x1 << TplIndex);
      break;
    case VctrTag:
      Va = (0x1 << VctrIndex);
      break;
    case InvldTag:
      Va = (0x1 << InvldIndex);
      break;
    }
    Vc = pc_nibble();
    Vb = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vb = Int_Val(Vb);
    if (Va & Vb) {
      for (Va = (Va >> 0x1), Vc = 0; Va != 0x1; Va = (Va >> 0x1)) {
	if (Va & Vb) {
	  Vc++;
	}
      }
    }
    PC = (opcodeP) char_offset(PC, (Vc * pc_intra_offset_skip()));
    PC = (opcodeP) char_offset(PC, pc_intra_offset());
    next_instr();

  case drf2_branch_integer:
    Va = pc_reg_offset();
    Vb = reg(Va);
    if (IsRef(Vb)) {
      deref_ref(Vb, Pb);
      reg(Va) = Ref_Word(Pb);
    }
    pc_reg() = Ia = Vb;
    Ib = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Ic = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    if (IsInt(((heapT) Ia)) && (Ib <= Ia) && (Ia <= Ic)) {
      Va = Val_of(((heapT) (Ia - Ib)));
    }
    else {
      Va = Val_of(((heapT) (Ic - Ib))) + 1;
    }
    PC = (opcodeP) char_offset(PC, (Va * pc_intra_offset_skip()));
    PC = (opcodeP) char_offset(PC, pc_intra_offset());
    next_instr();

  case drf2_branch_real:
    Va = pc_reg_offset();
    Vb = reg(Va);
    if (IsRef(Vb)) {
      deref_ref(Vb, Pb);
      reg(Va) = Ref_Word(Pb);
    }
    pc_reg() = Vb;
    /* Not defined yet */
    store_pr_args(Ia, Pa);
    fprintf(DbgFile, "Undefined Instruction, branch real\n");
    do_exit("Undefined Instruction Opcode", MACHINE, ErUDFDCD, True);

  case drf2_branch_tuple:
    Va = pc_reg_offset();
    Vb = reg(Va);
    if (IsRef(Vb)) {
      deref_ref(Vb, Pb);
      reg(Va) = Ref_Word(Pb);
    }
    pc_reg() = Va = Vb;
    Vb = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    Vc = pc_word();
    PC = (opcodeP) char_offset(PC, pc_word_skip());
    if (IsTpl(Va) && (Vb <= Va) && (Va <= Vc)) {
      Va = Val_of(((heapT) (Va - Vb)));
    }
    else {
      Va = Val_of(((heapT) (Vc - Vb))) + 1;
    }
    PC = (opcodeP) char_offset(PC, (Va * pc_intra_offset_skip()));
    PC = (opcodeP) char_offset(PC, pc_intra_offset());
    next_instr();

  case drf2_hash_integer:
    Va = pc_reg_offset();
    Vb = reg(Va);
    if (IsRef(Vb)) {
      deref_ref(Vb, Pb);
      reg(Va) = Ref_Word(Pb);
    }
    pc_reg() = Vb;
    Va = pc_nibble();
    if (IsInt(Vb)) {
      if ((Ia = Int_Val(Vb)) < 0) {
	Ia = (0 - Ia);
      }
      Va = Ia % Va;
    }
    PC = (opcodeP) char_offset(PC, (Va * pc_intra_offset_skip()));
    PC = (opcodeP) char_offset(PC, pc_intra_offset());
    next_instr();

  case drf2_hash_string:
    Va = pc_reg_offset();
    Vb = reg(Va);
    if (IsRef(Vb)) {
      deref_ref(Vb, Pb);
      reg(Va) = Ref_Word(Pb);
    }
    pc_reg() = Vb;
    Va = pc_nibble();
    if (IsStr(Vb)) {
      Va = Str_Hash(Pb) % Va;
    }
    PC = (opcodeP) char_offset(PC, (Va * pc_intra_offset_skip()));
    PC = (opcodeP) char_offset(PC, pc_intra_offset());
    next_instr();

  case if_var_suspend:
    if (IsVar(pc_reg())) {
      Pa = Ref_Val(pc_reg());
      sus_tbl_add(Pa);
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_reg_offset_skip());
    }
    next_instr();

  case drf1_if_var_suspend:
    Va = pc_reg_offset();
    Vb = reg(Va);
    if (IsRef(Vb)) {
      deref_ref(Vb, Pb);
      reg(Va) = Ref_Word(Pb);
    }
    if (IsVar(Vb)) {
      sus_tbl_add(Pb);
      PC = (opcodeP) char_offset(PC, pc_intra_offset());
    }
    else {
      PC = (opcodeP) char_offset(PC, pc_intra_offset_skip());
    }
    next_instr();

  case alloc_pr_enqu:
    Va = pc_nibble();
    FLs_allocate((PR_Header + Va), Pa, HP);
    pr_init(Va, Pa);
    Creations++;
    PCa = pc_inter_addr();
    PC = (opcodeP) char_offset(PC, pc_inter_offset_skip());
    *Prcdr_PR(Pa) = Ref_Word(PCa - (CtlHdrWords*(heapTbits/opcodeTbits)));
    pr_enqueue(Pa, HighQ);
    Pb = Args_PR(Pa);
    Vb = 0;
    next_instr();

  case alloc_pr_regs_enqu:
    Va = pc_nibble();
    FLs_allocate((PR_Header + Va), Pa, HP);
    pr_init(Va, Pa);
    Creations++;
    PCa = pc_inter_addr();
    PC = (opcodeP) char_offset(PC, pc_inter_offset_skip());
    *Prcdr_PR(Pa) = Ref_Word(PCa - (CtlHdrWords*(heapTbits/opcodeTbits)));
    pr_enqueue(Pa, HighQ);
    Pa = Args_PR(Pa);
    for (; Va > 0; Va--) {
      *Pa++ = pc_reg();
    }
    next_instr();

  case mult_operation:
    Ia = pc_nibble();
    switch (*PC++) {
    case copy_Rs_Rd:
      for (; Ia > 0; Ia--) {
	Va = pc_reg();
	pc_reg() = Va;
      }
      next_instr();

    case copy_Rs_CpId:
      for (; Ia > 0; Ia--) {
	Va = pc_reg();
	pc_cp_arg() = Va;
      }
      next_instr();

    case copy_Rs_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_reg();
	*Ref_Val(pc_reg()) = Va;
      }
      next_instr();

    case copy_CpIs_Rd:
      for (; Ia > 0; Ia--) {
	Va = pc_cp_arg();
	pc_reg() = Va;
      }
      next_instr();

    case copy_CpIs_CpId:
      for (; Ia > 0; Ia--) {
	Va = pc_cp_arg();
	pc_cp_arg() = Va;
      }
      next_instr();
      
    case copy_CpIs_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_cp_arg();
	*Ref_Val(pc_reg()) = Va;
      }
      next_instr();

    case copy_SRs_Rd:
      for (; Ia > 0; Ia--) {
	Va = *Ref_Val(pc_reg());
	pc_reg() = Va;
      }
      next_instr();

    case copy_SRs_CpId:
      for (; Ia > 0; Ia--) {
	Va = *Ref_Val(pc_reg());
	pc_cp_arg() = Va;
      }
      next_instr();

    case copy_SRs_SRd:
      for (; Ia > 0; Ia--) {
	Va = *Ref_Val(pc_reg());
	*Ref_Val(pc_reg()) = Va;
      }
      next_instr();

    case copy_RsIs_Rd:
      for (; Ia > 0; Ia--) {
	Va = pc_subarg();
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	pc_reg() = Va;
      }
      next_instr();

    case copy_RsIs_CpId:
      for (; Ia > 0; Ia--) {
	Va = pc_subarg();
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	pc_cp_arg() = Va;
      }
      next_instr();

    case copy_RsIs_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_subarg();
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	*Ref_Val(pc_reg()) = Va;
      }
      next_instr();

    case copy_RRs_Rd:
      for (; Ia > 0; Ia--) {
	Va = pc_reg();
	set_to_ro_ref(Va, Pa);
	pc_reg() = Va;
      }
      next_instr();

    case copy_RRs_CpId:
      for (; Ia > 0; Ia--) {
	Va = pc_reg();
	set_to_ro_ref(Va, Pa);
	pc_cp_arg() = Va;
      }
      next_instr();

    case copy_RRs_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_reg();
	set_to_ro_ref(Va, Pa);
	*Ref_Val(pc_reg()) = Va;
      }
      next_instr();

    case copy_CRs_Rd:
      for (; Ia > 0; Ia--) {
	Va = pc_reg();
	set_to_car(Va);
	pc_reg() = Va;
      }
      next_instr();

    case copy_CRs_CpId:
      for (; Ia > 0; Ia--) {
	Va = pc_reg();
	set_to_car(Va);
	pc_cp_arg() = Va;
      }
      next_instr();

    case copy_CRs_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_reg();
	set_to_car(Va);
	*Ref_Val(pc_reg()) = Va;
      }
      next_instr();

    case copy_ARsIs_Rd:
      for (; Ia > 0; Ia--) {
	Va = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	pc_reg() = Va;
      }
      next_instr();

    case copy_ARsIs_CpId:
      for (; Ia > 0; Ia--) {
	Va = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	pc_cp_arg() = Va;
      }
      next_instr();

    case copy_ARsIs_SRd:
      for (; Ia > 0; Ia--) {
	Va = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	*Ref_Val(pc_reg()) = Va;
      }
      next_instr();

    case copy_RARsIs_Rd:
      for (; Ia > 0; Ia--) {
	Va = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	set_to_ro_ref(Va, Pa);
	pc_reg() = Va;
      }
      next_instr();

    case copy_RARsIs_CpId:
      for (; Ia > 0; Ia--) {
	Va = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	set_to_ro_ref(Va, Pa);
	pc_cp_arg() = Va;
      }
      next_instr();

    case copy_RARsIs_SRd:
      for (; Ia > 0; Ia--) {
	Va = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	set_to_ro_ref(Va, Pa);
	*Ref_Val(pc_reg()) = Va;
      }
      next_instr();

    case copy_Nil_Rd:
      for (; Ia > 0; Ia--) {
	pc_reg() = Word(0, NilTag);
      }
      next_instr();

    case copy_Nil_CpId:
      for (; Ia > 0; Ia--) {
	pc_cp_arg() = Word(0, NilTag);
      }
      next_instr();

    case copy_Nil_SRd:
      for (; Ia > 0; Ia--) {
	*Ref_Val(pc_reg()) = Word(0, NilTag);
      }
      next_instr();

    case copy_Word_Rd:
      for (; Ia > 0; Ia--) {
	Va = pc_word();
	PC = (opcodeP) char_offset(PC, pc_word_skip());
	pc_reg() = Va;
      }
      next_instr();

    case copy_Word_CpId:
      for (; Ia > 0; Ia--) {
	Va = pc_word();
	PC = (opcodeP) char_offset(PC, pc_word_skip());
	pc_cp_arg() = Va;
      }
      next_instr();

    case copy_Word_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_word();
	PC = (opcodeP) char_offset(PC, pc_word_skip());
	*Ref_Val(pc_reg()) = Va;
      }
      next_instr();

    case copy_Real_Rd:
      for (; Ia > 0; Ia--) {
	Va = Ref_Word(HP);
	creat_pc_double(Vb);
	pc_reg() = Va;
      }
      next_instr();

    case copy_Real_CpId:
      for (; Ia > 0; Ia--) {
	Va = Ref_Word(HP);
	creat_pc_double(Vb);
	pc_cp_arg() = Va;
      }
      next_instr();

    case copy_Real_SRd:
      for (; Ia > 0; Ia--) {
	Va = Ref_Word(HP);
	creat_pc_double(Vb);
	*Ref_Val(pc_reg()) = Va;
      }
      next_instr();

    case copy_String_Rd:
      for (; Ia > 0; Ia--) {
	Va = pc_ref();
	PC = (opcodeP) char_offset(PC, pc_inter_offset_skip());
	pc_reg() = Va;
      }
      next_instr();

    case copy_String_CpId:
      for (; Ia > 0; Ia--) {
	Va = pc_ref();
	PC = (opcodeP) char_offset(PC, pc_inter_offset_skip());
	pc_cp_arg() = Va;
      }
      next_instr();

    case copy_String_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_ref();
	PC = (opcodeP) char_offset(PC, pc_inter_offset_skip());
	*Ref_Val(pc_reg()) = Va;
      }
      next_instr();

    case copy_WeVar_Rd:
      for (; Ia > 0; Ia--) {
	pc_reg() = Ref_Word(HP);
	*HP++ = ZeroedWrt;
      }
      next_instr();

    case copy_WeVar_CpId:
      for (; Ia > 0; Ia--) {
	pc_cp_arg() = Ref_Word(HP);
	*HP++ = ZeroedWrt;
      }
      next_instr();

    case copy_WeVar_SRd:
      for (; Ia > 0; Ia--) {
	*Ref_Val(pc_reg()) = Ref_Word(HP);
	*HP++ = ZeroedWrt;
      }
      next_instr();

    case asgn_Rs_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_reg();
	Pa = Ref_Val(pc_reg());
	asgn(*Pa, Pa, Va);
      }
      next_instr();

    case asgn_CpIs_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_cp_arg();
	Pa = Ref_Val(pc_reg());
	asgn(*Pa, Pa, Va);
      }
      next_instr();

    case asgn_SRs_SRd:
      for (; Ia > 0; Ia--) {
	Va = *Ref_Val(pc_reg());
	Pa = Ref_Val(pc_reg());
	asgn(*Pa, Pa, Va);
      }
      next_instr();

    case asgn_RsIs_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_subarg();
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	Pa = Ref_Val(pc_reg());
	asgn(*Pa, Pa, Va);
      }
      next_instr();

    case asgn_RRs_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_reg();
	set_to_ro_ref(Va, Pa);
	Pa = Ref_Val(pc_reg());
	asgn(*Pa, Pa, Va);
      }
      next_instr();

    case asgn_CRs_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_reg();
	set_to_car(Va);
	Pa = Ref_Val(pc_reg());
	asgn(*Pa, Pa, Va);
      }
      next_instr();

    case asgn_ARsIs_SRd:
      for (; Ia > 0; Ia--) {
	Va = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	Pa = Ref_Val(pc_reg());
	asgn(*Pa, Pa, Va);
      }
      next_instr();

    case asgn_RARsIs_SRd:
      for (; Ia > 0; Ia--) {
	Va = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	set_to_ro_ref(Va, Pa);
	Pa = Ref_Val(pc_reg());
	asgn(*Pa, Pa, Va);
      }
      next_instr();

    case asgn_Nil_SRd:
      for (; Ia > 0; Ia--) {
	Pa = Ref_Val(pc_reg());
	asgn(*Pa, Pa, Word(0, NilTag));
      }
      next_instr();

    case asgn_Word_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_word();
	PC = (opcodeP) char_offset(PC, pc_word_skip());
	Pa = Ref_Val(pc_reg());
	asgn(*Pa, Pa, Va);
      }
      next_instr();

    case asgn_Real_SRd:
      for (; Ia > 0; Ia--) {
	Va = Ref_Word(HP);
	creat_pc_double(Vb);
	Pa = Ref_Val(pc_reg());
	asgn(*Pa, Pa, Va);
      }
      next_instr();

    case asgn_String_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_ref();
	PC = (opcodeP) char_offset(PC, pc_inter_offset_skip());
	Pa = Ref_Val(pc_reg());
	asgn(*Pa, Pa, Va);
      }
      next_instr();

    case asgn_WeVar_SRd:
      for (; Ia > 0; Ia--) {
	Pa = Ref_Val(pc_reg());
	asgn(*Pa, Pa, Ref_Word(HP));
	*HP++ = ZeroedWrt;
      }
      next_instr();

    case assign_Rs_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_reg();
	Pa = Ref_Val(pc_reg());
	*Pa = Va;
      }
      next_instr();

    case assign_CpIs_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_cp_arg();
	Pa = Ref_Val(pc_reg());
	*Pa = Va;
      }
      next_instr();

    case assign_SRs_SRd:
      for (; Ia > 0; Ia--) {
	Va = *Ref_Val(pc_reg());
        Pa = Ref_Val(pc_reg());
	*Pa = Va;
      }
      next_instr();

    case assign_RsIs_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_subarg();
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	Pa = Ref_Val(pc_reg());
	*Pa = Va;
      }
      next_instr();

    case assign_RRs_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_reg();
	set_to_ro_ref(Va, Pa);
	Pa = Ref_Val(pc_reg());
	*Pa = Va;
      }
      next_instr();

    case assign_CRs_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_reg();
	set_to_car(Va);
	Pa = Ref_Val(pc_reg());
	*Pa = Va;
      }
      next_instr();

    case assign_ARsIs_SRd:
      for (; Ia > 0; Ia--) {
	Va = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	Pa = Ref_Val(pc_reg());
	*Pa = Va;
      }
      next_instr();

    case assign_RARsIs_SRd:
      for (; Ia > 0; Ia--) {
	Va = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	set_to_ro_ref(Va, Pa);
	Pa = Ref_Val(pc_reg());
	*Pa = Va;
      }
      next_instr();

    case assign_Nil_SRd:
      for (; Ia > 0; Ia--) {
	Pa = Ref_Val(pc_reg());
	*Pa = Word(0, NilTag);
      }
      next_instr();

    case assign_Word_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_word();
	PC = (opcodeP) char_offset(PC, pc_word_skip());
	Pa = Ref_Val(pc_reg());
	*Pa = Va;
      }
      next_instr();

    case assign_Real_SRd:
      for (; Ia > 0; Ia--) {
	Va = Ref_Word(HP);
	creat_pc_double(Vb);
	Pa = Ref_Val(pc_reg());
	*Pa = Va;
      }
      next_instr();

    case assign_String_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_ref();
	PC = (opcodeP) char_offset(PC, pc_inter_offset_skip());
	Pa = Ref_Val(pc_reg());
	*Pa = Va;
      }
      next_instr();

    case assign_WeVar_SRd:
      for (; Ia > 0; Ia--) {
	Pa = Ref_Val(pc_reg());
	*Pa = Ref_Word(HP);
	*HP++ = ZeroedWrt;
      }
      next_instr();

    case assign_com_Rs_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_reg();
	Pb = Ref_Val(pc_reg());
	do_commit(Va,*Pb);
        *Pb = Va;
      }
      next_instr();

    case assign_com_CpIs_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_cp_arg();
	Pb = Ref_Val(pc_reg());
	do_commit(Va,*Pb);
        *Pb = Va;
      }
      next_instr();

    case assign_com_SRs_SRd:
      for (; Ia > 0; Ia--) {
	Va = *Ref_Val(pc_reg());
        Pb = Ref_Val(pc_reg());
	do_commit(Va,*Pb);
        *Pb = Va;
      }
      next_instr();

    case assign_com_RsIs_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_subarg();
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	Pb = Ref_Val(pc_reg());
	do_commit(Va,*Pb);
        *Pb = Va;
      }
      next_instr();

    case assign_com_RRs_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_reg();
	set_to_ro_ref(Va, Pa);
	Pb = Ref_Val(pc_reg());
	do_commit(Va,*Pb);
        *Pb = Va;
      }
      next_instr();

    case assign_com_CRs_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_reg();
	set_to_car(Va);
	Pb = Ref_Val(pc_reg());
	do_commit(Va,*Pb);
        *Pb = Va;
      }
      next_instr();

    case assign_com_ARsIs_SRd:
      for (; Ia > 0; Ia--) {
	Va = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	Pb = Ref_Val(pc_reg());
	do_commit(Va,*Pb);
        *Pb = Va;
      }
      next_instr();

    case assign_com_RARsIs_SRd:
      for (; Ia > 0; Ia--) {
	Va = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	set_to_ro_ref(Va, Pa);
	Pb = Ref_Val(pc_reg());
	do_commit(Va,*Pb);
        *Pb = Va;
      }
      next_instr();

    case assign_com_Nil_SRd:  
    for (; Ia > 0; Ia--) {
        Va = Word(0, NilTag);
	Pb = Ref_Val(pc_reg());
	do_commit(Va,*Pb);
        *Pb = Va;
      }
      next_instr();

    case assign_com_Word_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_word();
	PC = (opcodeP) char_offset(PC, pc_word_skip());
	Pb = Ref_Val(pc_reg());
	do_commit(Va,*Pb);
        *Pb = Va;
      }
      next_instr();

    case assign_com_Real_SRd:
      for (; Ia > 0; Ia--) {
	Va = Ref_Word(HP);
	creat_pc_double(Vb);
	Pb = Ref_Val(pc_reg());
	do_commit(Va,*Pb);
        *Pb = Va;
      }
      next_instr();

    case assign_com_String_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_ref();
	PC = (opcodeP) char_offset(PC, pc_inter_offset_skip());
	Pb = Ref_Val(pc_reg());
	do_commit(Va,*Pb);
        *Pb = Va;
      }
      next_instr();

    case assign_com_WeVar_SRd:
      for (; Ia > 0; Ia--) {
        Va = Ref_Word(HP);
	Pb = Ref_Val(pc_reg());
	do_commit(Va,*Pb);
        *Pb = Va;
	*HP++ = ZeroedWrt;
      }
      next_instr();

    case assign_com_tr_Rs_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_reg();
	Pb = Ref_Val(pc_reg());
	do_commit(Va,*Pb);
        *Pb = Va;
      }
      do_commit1();
      next_instr();

    case assign_com_tr_CpIs_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_cp_arg();
	Pb = Ref_Val(pc_reg());
	do_commit(Va,*Pb);
        *Pb = Va;
      }
      do_commit1();
      next_instr();

    case assign_com_tr_SRs_SRd:
      for (; Ia > 0; Ia--) {
	Va = *Ref_Val(pc_reg());
        Pb = Ref_Val(pc_reg());
	do_commit(Va,*Pb);
        *Pb = Va;
      }
      do_commit1();
      next_instr();

    case assign_com_tr_RsIs_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_subarg();
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	Pb = Ref_Val(pc_reg());
	do_commit(Va,*Pb);
        *Pb = Va;
      }
      do_commit1();
      next_instr();

    case assign_com_tr_RRs_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_reg();
	set_to_ro_ref(Va, Pa);
	Pb = Ref_Val(pc_reg());
	do_commit(Va,*Pb);
        *Pb = Va;
      }
      do_commit1();
      next_instr();

    case assign_com_tr_CRs_SRd:
      for (; Ia = 0; Ia--) {
	Va = pc_reg();
	set_to_car(Va);
	Pb = Ref_Val(pc_reg());
	do_commit(Va,*Pb);
        *Pb = Va;
      }
      do_commit1();
      next_instr();

    case assign_com_tr_ARsIs_SRd:
      for (; Ia > 0; Ia--) {
	Va = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	Pb = Ref_Val(pc_reg());
	do_commit(Va,*Pb);
        *Pb = Va;
      }
      do_commit1();
      next_instr();

    case assign_com_tr_RARsIs_SRd:
      for (; Ia > 0; Ia--) {
	Va = Ref_Word(pc_subarg_addr());
	PC = (opcodeP) char_offset(PC, pc_subarg_skip());
	set_to_ro_ref(Va, Pa);
	Pb = Ref_Val(pc_reg());
	do_commit(Va,*Pb);
        *Pb = Va;
      }
      do_commit1();
      next_instr();

    case assign_com_tr_Nil_SRd:
      for (; Ia > 0; Ia--) {
        Va = Word(0, NilTag);
	Pb = Ref_Val(pc_reg());
	do_commit(Va,*Pb);
        *Pb = Va;
      }
      do_commit1();
      next_instr();

    case assign_com_tr_Word_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_word();
	PC = (opcodeP) char_offset(PC, pc_word_skip());
	Pb = Ref_Val(pc_reg());
	do_commit(Va,*Pb);
        *Pb = Va;
      }
      do_commit1();
      next_instr();

    case assign_com_tr_Real_SRd:
      for (; Ia > 0; Ia--) {
	Va = Ref_Word(HP);
	creat_pc_double(Vb);
	Pb = Ref_Val(pc_reg());
	do_commit(Va,*Pb);
        *Pb = Va;
      }
      do_commit1();
      next_instr();

    case assign_com_tr_String_SRd:
      for (; Ia > 0; Ia--) {
	Va = pc_ref();
	PC = (opcodeP) char_offset(PC, pc_inter_offset_skip());
	Pb = Ref_Val(pc_reg());
	do_commit(Va,*Pb);
        *Pb = Va;
      }
      do_commit1();
      next_instr();

    case assign_com_tr_WeVar_SRd:
      for (; Ia > 0; Ia--) {
        Va = Ref_Word(HP);
	Pb = Ref_Val(pc_reg());
	do_commit(Va,*Pb);
        *Pb = Va;
	*HP++ = ZeroedWrt;
      }
      do_commit1();
      next_instr();

   default:
      store_pr_args(Ia, Pa);
      fprintf(DbgFile, "multiple_operation: Opcode = %x  ", *(--PC));
      do_exit("Unknown Instruction Opcode", MACHINE, ErUDFDCD, True);
    }

    /* Ask kernels */

    /* Ask/0 */

  case otherwise:		/* "otherwise", 0 */
    debug_ask_kernel_0("otherwise");
    if (!sus_tbl_empty() || 
	(!err_tbl_empty() &&
	 in_err_tbl(MACHINE, ErHPSPACE) &&
	 ((Creations + Suspensions + Activations + Switches + Reductions +
	   Terminations + Collections) != Last_Redo_GC))) {
      kernel_failed();
    }
    kernel_succeeded();

    /* Ask/1 */

  case is_nonvar:	 	/* "nonvar", 1 */
    Va = reg(Ia = pc_reg_offset());
    Pa = Ref_Val(Va);
    debug_ask_kernel_1("nonvar", Va);
    deref(Va, Pa);
    if (IsVar(Va)) {
      reg(Ia) = Ref_Word(Pa);
      kernel_failed();
    }
    reg(Ia) = (IsInt(Va) || IsNil(Va)) ? Va : Ref_Word(Pa);
    kernel_succeeded();

  case ask_unknown:		/* "ask_unknown", 1 */
    Va = reg(Ia = pc_reg_offset());
    debug_ask_kernel_1("ask_unknown", Va);
    deref(Va, Pa);
    if (!IsVar(Va)) {
      reg(Ia) = (IsInt(Va) || IsNil(Va)) ? Va : Ref_Word(Pa);
      kernel_failed();
    }
    reg(Ia) = Ref_Word(Pa);
    kernel_succeeded();

  case is_known:	  	/* "known", 1 */
    Va = reg(Ia = pc_reg_offset());
    Pa = Ref_Val(Va);
    debug_ask_kernel_1("known", Va);
    deref(Va, Pa);
    if (IsVar(Va)) {
      reg(Ia) = Ref_Word(Pa);
      sus_tbl_add(Pa);
      kernel_suspended();
    }
    reg(Ia) = (IsInt(Va) || IsNil(Va)) ? Va : Ref_Word(Pa);
    kernel_succeeded();

  case is_var:			/* "var", 1 */
  case is_we:                    /* "we", 1 */
    Va = reg(Ia = pc_reg_offset());
    Pa = Ref_Val(Va);
    debug_ask_kernel_1("we", Va);
    deref(Va, Pa);
    if (!IsWrt(Va)) {
      reg(Ia) = (IsInt(Va) || IsNil(Va)) ? Va : Ref_Word(Pa);
      kernel_failed();
    }
    reg(Ia) = Ref_Word(Pa);
    kernel_succeeded();

  case is_not_we:                /* not_we", 1 */
    Va = reg(Ia = pc_reg_offset());
    Pa = Ref_Val(Va);
    debug_ask_kernel_1("not_we", Va);
    deref(Va, Pa);
    if (!IsWrt(Va)) {
      reg(Ia) = (IsInt(Va) || IsNil(Va)) ? Va : Ref_Word(Pa);
      kernel_succeeded();
    }
    reg(Ia) = Ref_Word(Pa);
    kernel_failed();

  case is_ro:			/* "ro", 1 */
    Va = reg(Ia = pc_reg_offset());
    Pa = Ref_Val(Va);
    debug_ask_kernel_1("ro", Va);
    deref(Va, Pa);
    if (!IsRo(Va)) {
      reg(Ia) = (IsInt(Va) || IsNil(Va)) ? Va : Ref_Word(Pa);
      if (IsWrt(Va)) {
	sus_tbl_add(Pa);
	kernel_suspended();
      }
      kernel_failed();
    }
    reg(Ia) = Ref_Word(Pa);
    kernel_succeeded();

  case is_integer:  		/* "integer", 1 */
    Va = reg(Ia = pc_reg_offset());
    Pa = Ref_Val(Va);
    debug_ask_kernel_1("integer", Va);
    deref(Va, Pa);
    if (!IsInt(Va)) {
      reg(Ia) = Ref_Word(Pa);
      if (IsVar(Va)) {
	sus_tbl_add(Pa);
	kernel_suspended();
      }
      kernel_failed();
    }
    reg(Ia) = Va;
    kernel_succeeded();

  case is_real:			/* "real", 1 */
    Va = reg(Ia = pc_reg_offset());
    Pa = Ref_Val(Va);
    debug_ask_kernel_1("real", Va);
    deref(Va, Pa);
    if (!IsReal(Va)) {
      reg(Ia) = (IsInt(Va) || IsNil(Va)) ? Va : Ref_Word(Pa);
      if (IsVar(Va)) {
	sus_tbl_add(Pa);
	kernel_suspended();
      }
      kernel_failed();
    }
    reg(Ia) = Ref_Word(Pa);
    kernel_succeeded();

  case is_string:	  	/* "string",1 */
    Va = reg(Ia = pc_reg_offset());
    Pa = Ref_Val(Va);
    debug_ask_kernel_1("string", Va);
    deref(Va, Pa);
    if (!IsStr(Va)) {
      reg(Ia) = (IsInt(Va) || IsNil(Va)) ? Va : Ref_Word(Pa);
      if (IsVar(Va)) {
	sus_tbl_add(Pa);
	kernel_suspended();
      }
      kernel_failed();
    }
    reg(Ia) = Ref_Word(Pa);
    kernel_succeeded();

  case is_list:		/* "list", 1 */
    Va = reg(Ia = pc_reg_offset());
    Pa = Ref_Val(Va);
    debug_ask_kernel_1("list", Va);
    deref(Va, Pa);
    if (!IsList(Va)) {
      reg(Ia) = (IsInt(Va) || IsNil(Va)) ? Va : Ref_Word(Pa);
      if (IsVar(Va)) {
	sus_tbl_add(Pa);
	kernel_suspended();
      }
      kernel_failed();
    }
    reg(Ia) = Ref_Word(Pa);
    kernel_succeeded();

  case is_tuple:	  	/* "tuple", 1 */
    Va = reg(Ia = pc_reg_offset());
    Pa = Ref_Val(Va);
    debug_ask_kernel_1("tuple", Va);
    deref(Va, Pa);
    if (!IsTpl(Va)) {
      reg(Ia) = (IsInt(Va) || IsNil(Va)) ? Va : Ref_Word(Pa);
      if (IsVar(Va)) {
	sus_tbl_add(Pa);
	kernel_suspended();
      }
      kernel_failed();
    }
    reg(Ia) = Ref_Word(Pa);
    kernel_succeeded();

  case is_vector:	  	/* "vector", 1 */
    Va = reg(Ia = pc_reg_offset());
    Pa = Ref_Val(Va);
    debug_ask_kernel_1("vector", Va);
    deref(Va, Pa);
    if (!IsVctr(Va)) {
      reg(Ia) = (IsInt(Va) || IsNil(Va)) ? Va : Ref_Word(Pa);
      if (IsVar(Va)) {
	sus_tbl_add(Pa);
	kernel_suspended();
      }
      kernel_failed();
    }
    reg(Ia) = Ref_Word(Pa);
    kernel_succeeded();

  case is_module:		/* "module", 1 */
    Va = reg(Ia = pc_reg_offset());
    Pa = Ref_Val(Va);
    debug_ask_kernel_1("module", Va);
    deref(Va, Pa);
    if (!(IsStr(Va) && (Str_Type(Pa) == MdlType))) {
      reg(Ia) = (IsInt(Va) || IsNil(Va)) ? Va : Ref_Word(Pa);
      if (IsVar(Va)) {
	sus_tbl_add(Pa);
	kernel_suspended();
      }
      kernel_failed();
    }
    reg(Ia) = Ref_Word(Pa);
    kernel_succeeded();

  case is_constant:		/* "constant", 1 */
    Va = reg(Ia = pc_reg_offset());
    Pa = Ref_Val(Va);
    Pa = Ref_Val(Va);
    debug_ask_kernel_1("constant", Va);
    deref(Va, Pa);
    if (!(IsInt(Va) || IsReal(Va) || IsStr(Va) || IsNil(Va))) {
      reg(Ia) = Ref_Word(Pa);
      if (IsVar(Va)) {
	sus_tbl_add(Pa);
	kernel_suspended();
      }
      kernel_failed();
    }
    reg(Ia) = (IsInt(Va) || IsInt(Va)) ? Va : Ref_Word(Pa);
    kernel_succeeded();

  case is_compound:		/* "compound", 1 */
    Va = reg(Ia = pc_reg_offset());
    Pa = Ref_Val(Va);
    debug_ask_kernel_1("compound", Va);
    deref(Va, Pa);
    if (!(IsList(Va) || IsTpl(Va) || IsVctr(Va))) {
      reg(Ia) = (IsInt(Va) || IsNil(Va)) ? Va : Ref_Word(Pa);
      if (IsVar(Va)) {
	sus_tbl_add(Pa);
	kernel_suspended();
      }
      kernel_failed();
    }
    reg(Ia) = Ref_Word(Pa);
    kernel_succeeded();

  case is_number:		/* "number", 1 */
    Va = reg(Ia = pc_reg_offset());
    Pa = Ref_Val(Va);
    debug_ask_kernel_1("number", Va);
    deref(Va, Pa);
    if (!(IsInt(Va) || IsReal(Va))) {
      reg(Ia) = Ref_Word(Pa);
      if (IsVar(Va)) {
	sus_tbl_add(Pa);
	kernel_suspended();
      }
      kernel_failed();
    }
    reg(Ia) = (IsInt(Va) || IsNil(Va)) ? Va : Ref_Word(Pa);
    kernel_succeeded();
    
  case exceptions:		/* "exceptions", 1 */
    Va = pc_reg();
    debug_ask_kernel_1("exceptions", Va);
    Pa = STP;
    if (!do_exceptions()) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    Pa = STP;
    if (!ask_unify(Va, KOutA)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();

  case grounded:		/* "grounded", 1 */
    Va = pc_reg();
    debug_ask_kernel_1("grounded", Va);
    Pa = STP;
    if (!do_grounded(Va)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();
    
    /* Ask/2 */
    
  case wait_equals:		/* "=?=", 2 */
    Va = pc_reg();
    Vb = pc_reg();
    debug_ask_kernel_2("=?=", Va, Vb);
    Pa = STP;
    if (!ask_unify(Va, Vb)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();
    
  case wait_not_equals:		/* "=\=", 2 */
    Va = pc_reg();
    Vb = pc_reg();
    debug_ask_kernel_2("=\\=", Va, Vb);
    Pa = STP;
    if (ask_unify(Va, Vb)) {
      kernel_failed();
    }
    if (Pa != STP) {
      kernel_suspended();
    }
    kernel_succeeded();
    
  case is_less:		/* "@<", 2 */
    Va = pc_reg();
    Vb = pc_reg();
    debug_ask_kernel_2("@<", Va, Vb);
    Pa = STP;
    TRPa = ChngTRP;
    if (compare(Va, Vb) != Less) {
      /* No Assignments In Compare */
      chng_trl_undo(TRPa);
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    /* No Assignments In Compare */
    chng_trl_undo(TRPa);
    kernel_succeeded();

  case not_identical:		/* "\=", 2 */
    Va = pc_reg();
    Vb = pc_reg();
    debug_ask_kernel_2("\\=", Va, Vb);
    deref(Va, Pa);
    deref(Vb, Pb);
    if (!(IsVar(Va) && IsVar(Vb))) {
      kernel_failed();
    }
    if (IsWrt(Va) && !IsZeroed(Va)) {
      Pa = Var_Val(Va);
    }
    if (IsWrt(Vb) && !IsZeroed(Vb)) {
      Pb = Var_Val(Vb);
    }
    if (Pa == Pb) {
      kernel_failed();
    }
    kernel_succeeded();

  case lt:			/* "<", 2 */
    Va = pc_reg();
    Vb = pc_reg();
    debug_ask_kernel_2("<", Va, Vb);
    deref(Va, Pa);
    if (IsVar(Va)) {
      sus_tbl_add(Pa);
      kernel_suspended();
    }
    deref(Vb, Pb);
    if (IsVar(Vb)) {
      sus_tbl_add(Pb);
      kernel_suspended();
    }
    switch (Tag_of(Va)) {
    case IntTag:
      switch (Tag_of(Vb)) {
      case IntTag:
	if ((int) Va >= (int) Vb) {
	  kernel_failed();
	}
	kernel_succeeded();
      case RealTag:
	if ((realT) Int_Val(Va) >= real_val((Pb+1))) {
	  kernel_failed();
	}
	kernel_succeeded();
      default:
	kernel_failed();
      }
    case RealTag:
      switch (Tag_of(Vb)) {
      case IntTag:
	if (real_val((Pa+1)) >= (realT) Int_Val(Vb)) {
	  kernel_failed();
	}
	kernel_succeeded();
      case RealTag:
	if (real_val((Pa+1)) >= real_val((Pb+1))) {
	  kernel_failed();
	}
	kernel_succeeded();
      default:
	kernel_failed();
      }
    default:
      kernel_failed();
    }

  case le:			/* "=<", 2 */
    Va = pc_reg();
    Vb = pc_reg();
    debug_ask_kernel_2("=<", Va, Vb);
    deref(Va, Pa);
    if (IsVar(Va)) {
      sus_tbl_add(Pa);
      kernel_suspended();
    }
    deref(Vb, Pb);
    if (IsVar(Vb)) {
      sus_tbl_add(Pb);
      kernel_suspended();
    }
    switch (Tag_of(Va)) {
    case IntTag:
      switch (Tag_of(Vb)) {
      case IntTag:
	if ((int) Va > (int) Vb) {
	  kernel_failed();
	}
	kernel_succeeded();
      case RealTag:
	if ((realT) Int_Val(Va) > real_val((Pb+1))) {
	  kernel_failed();
	}
	kernel_succeeded();
      default:
	kernel_failed();
      }
    case RealTag:
      switch (Tag_of(Vb)) {
      case IntTag:
	if (real_val((Pa+1)) > (realT) Int_Val(Vb)) {
	  kernel_failed();
	}
	kernel_succeeded();
      case RealTag:
	if (real_val((Pa+1)) > real_val((Pb+1))) {
	  kernel_failed();
	}
	kernel_succeeded();
      default:
	kernel_failed();
      }
    default:
      kernel_failed();
    }

  case bitwise_not:		/* "bitwise_not", 2 */
    Va = pc_reg();
    Vb = pc_reg();
    debug_ask_kernel_2("bitwise_not", Va, Vb);
    deref(Va, Pa);
    if (!IsInt(Va)) {
      if (IsVar(Va)) {
	sus_tbl_add(Pa);
	kernel_suspended();
      }
      kernel_failed();
    }
    Pa = STP;
    if (!ask_unify(Vb, On_Tag(Off_Tag(~(Va)), IntTag))) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();

  case string_length:		/* "string_length", 2 */
    Va = pc_reg();
    Vb = pc_reg();
    debug_ask_kernel_2("string_length", Va, Vb);
    deref(Va, Pa);
    if (!IsStr(Va)) {
      if (IsVar(Va)) {
	sus_tbl_add(Pa);
	kernel_suspended();
      }
      kernel_failed();
    }
    Pb = STP;
    if (!ask_unify(Vb, Word(Str_Length(Pa), IntTag))) {
      if (Pb != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();

  case string_hash:		/* "string_hash", 2 */
    Va = pc_reg();
    Vb = pc_reg();
    debug_ask_kernel_2("string_hash", Va, Vb);
    deref(Va, Pa);
    if (!IsStr(Va)) {
      if (IsVar(Va)) {
	sus_tbl_add(Pa);
	kernel_suspended();
      }
      kernel_failed();
    }
    Pb = STP;
    if (!ask_unify(Vb, Word(Str_Hash(Pa), IntTag))) {
      if (Pb != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();

  case cnv_to_integer:		/* "cnvrt_to_integer", 2 */
    Va = pc_reg();
    Vb = pc_reg();
    debug_ask_kernel_2("cnvrt_to_integer", Va, Vb);
    Pa = STP;
    if (!do_cnv_to_integer(Va)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    Pa = STP;
    if (!ask_unify(Vb, KOutA)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();

  case cnv_to_real:		/* "cnvrt_to_real", 2 */
    Va = pc_reg();
    Vb = pc_reg();
    debug_ask_kernel_2("cnvrt_to_real", Va, Vb);
    Pa = STP;
    if (!do_cnv_to_real(Va)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    Pa = STP;
    if (!ask_unify(Vb, KOutA)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();

  case cnv_to_string:	/* "cnvrt_to_string", 2 */
    Va = pc_reg();
    Vb = pc_reg();
    debug_ask_kernel_2("cnvrt_to_string", Va, Vb);
    Pa = STP;
    if (!do_cnv_to_string(Va)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    Pa = STP;
    if (!ask_unify(Vb, KOutA)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();

  case arity:		/* "arity", 2 */
    Va = pc_reg();
    Vb = pc_reg();
    debug_ask_kernel_2("arity", Va, Vb);
    deref(Va, Pa);
    if (!(IsTpl(Va) || IsVctr(Va))) {
      if (IsVar(Va)) {
	sus_tbl_add(Pa);
	kernel_suspended();
      }
      kernel_failed();
    }
    Pa = STP;
    if (!ask_unify(Vb, On_Tag(Off_Tag(Va), IntTag))) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();
    
  case make_tuple:		/* "make_tuple", 2 */	
    Va = pc_reg();
    Vb = pc_reg();
    debug_ask_kernel_2("make_tuple", Va, Vb);
    Pa = STP;
    if (!do_make_tuple(Va)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    Pa = STP;
    if (!ask_unify(Vb, KOutA)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();
    
  case info:			/* "info", 2 */
    Va = pc_reg();
    Vb = pc_reg();
    debug_ask_kernel_2("info", Va, Vb);
    Pa = STP;
    if (!do_info(Va)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    Pa = STP;
    if (!ask_unify(Vb, KOutA)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();

  case code_info:		/* "code_info", 2 */
    Va = pc_reg();
    Vb = pc_reg();
    debug_ask_kernel_2("code_info", Va, Vb);
    Pa = STP;
    if (!do_code_info(Va)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    Pa = STP;
    if (!ask_unify(Vb, KOutA)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();

  case invalid_g:		/* "invalid", 2 */
    Va = pc_reg();
    Vb = pc_reg();
    debug_ask_kernel_2("invalid", Va, Vb);
    Pa = STP;
    if (!do_invalid(Va, Vb)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();
    
  case var_info_g:		/* "var_info", 2 */
    Va = pc_reg();
    Vb = pc_reg();
    debug_ask_kernel_2("var_info", Va, Vb);
    Pa = STP;
    if (!do_var_info(Va, Vb)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();
    
    /* Ask/3 */
    
  case plus:			/* "+", 3 */
    Va = pc_reg();
    Vb = pc_reg();
    Vc = pc_reg();
    debug_ask_kernel_3("+", Va, Vb, Vc);
    deref(Va, Pa);
    deref(Vb, Pb);
    if (IsInt(Va)) {
      if (IsInt(Vb)) {
	Va = ((heapT) ((int) Va + (int) Off_Tag(Vb)));
      }
      else {
	if (IsReal(Vb)) {
	  *HP++ = Word(0, RealTag);
	  real_copy_val(((realT) Int_Val(Va) + real_val((Pb+1))), HP);
	  HP += realTbits/heapTbits;
	  Va = Ref_Word((HP - (1+(realTbits/heapTbits))));
	}
	else {
	  if (IsVar(Vb)) {
	    sus_tbl_add(Pb);
	    kernel_suspended();
	  }
	  kernel_failed();
	}
      }
    }
    else {
      if (IsReal(Va)) {
	if (IsInt(Vb)) {
	  Va = Ref_Word(HP);
	  *HP++ = Word(0, RealTag);
	  real_copy_val((real_val((Pa+1)) + (realT) Int_Val(Vb)), HP);
	  HP += realTbits/heapTbits;
	}
	else {
	  if (IsReal(Vb)) {
	    Va = Ref_Word(HP);
	    *HP++ = Word(0, RealTag);
	    real_copy_val((real_val((Pa+1)) + real_val((Pb+1))), HP);
	    HP += realTbits/heapTbits;
	  }
	  else {
	    if (IsVar(Vb)) {
	      sus_tbl_add(Pb);
	      kernel_suspended();
	    }
	    kernel_failed();
	  }
	}
      }
      else {
	if (IsVar(Va)) {
	  sus_tbl_add(Pa);
	  kernel_suspended();
	}
	kernel_failed();
      }
    }
    Pa = STP;
    if (!ask_unify(Vc, Va)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();

  case diff:			/* "-", 3 */
    Va = pc_reg();
    Vb = pc_reg();
    Vc = pc_reg();
    debug_ask_kernel_3("-", Va, Vb, Vc);
    deref(Va, Pa);
    deref(Vb, Pb);
    if (IsInt(Va)) {
      if (IsInt(Vb)) {
	Va = ((heapT) ((int) Va - (int) Off_Tag(Vb)));
      }
      else {
	if (IsReal(Vb)) {
	  *HP++ = Word(0, RealTag);
	  real_copy_val(((realT) Int_Val(Va) - real_val((Pb+1))), HP);
	  HP += realTbits/heapTbits;
	  Va = Ref_Word((HP - (1+(realTbits/heapTbits))));
	}
	else {
	  if (IsVar(Vb)) {
	    sus_tbl_add(Pb);
	    kernel_suspended();
	  }
	  kernel_failed();
	}
      }
    }
    else {
      if (IsReal(Va)) {
	if (IsInt(Vb)) {
	  Va = Ref_Word(HP);
	  *HP++ = Word(0, RealTag);
	  real_copy_val((real_val((Pa+1)) - (realT) Int_Val(Vb)), HP);
	  HP += realTbits/heapTbits;
	}
	else {
	  if (IsReal(Vb)) {
	    Va = Ref_Word(HP);
	    *HP++ = Word(0, RealTag);
	    real_copy_val((real_val((Pa+1)) - real_val((Pb+1))), HP);
	    HP += realTbits/heapTbits;
	  }
	  else {
	    if (IsVar(Vb)) {
	      sus_tbl_add(Pb);
	      kernel_suspended();
	    }
	    kernel_failed();
	  }
	}
      }
      else {
	if (IsVar(Va)) {
	  sus_tbl_add(Pa);
	  kernel_suspended();
	}
	kernel_failed();
      }
    }
    Pa = STP;
    if (!ask_unify(Vc, Va)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();

  case times:			/* "*", 3 */
    Va = pc_reg();
    Vb = pc_reg();
    Vc = pc_reg();
    debug_ask_kernel_3("*", Va, Vb, Vc);
    deref(Va, Pa);
    deref(Vb, Pb);
    if (IsInt(Va)) {
      if (IsInt(Vb)) {
	Va = On_Tag((Int_Val(Va) * (int) Off_Tag(Vb)), IntTag);
      }
      else {
	if (IsReal(Vb)) {
	  *HP++ = Word(0, RealTag);
	  real_copy_val(((realT) Int_Val(Va) * real_val((Pb+1))), HP);
	  HP += realTbits/heapTbits;
	  Va = Ref_Word((HP - (1+(realTbits/heapTbits))));
	}
	else {
	  if (IsVar(Vb)) {
	    sus_tbl_add(Pb);
	    kernel_suspended();
	  }
	  kernel_failed();
	}
      }
    }
    else {
      if (IsReal(Va)) {
	if (IsInt(Vb)) {
	  Va = Ref_Word(HP);
	  *HP++ = Word(0, RealTag);
	  real_copy_val((real_val((Pa+1)) * (realT) Int_Val(Vb)), HP);
	  HP += realTbits/heapTbits;
	}
	else {
	  if (IsReal(Vb)) {
	    Va = Ref_Word(HP);
	    *HP++ = Word(0, RealTag);
	    real_copy_val((real_val((Pa+1)) * real_val((Pb+1))), HP);
	    HP += realTbits/heapTbits;
	  }
	  else {
	    if (IsVar(Vb)) {
	      sus_tbl_add(Pb);
	      kernel_suspended();
	    }
	    kernel_failed();
	  }
	}
      }
      else {
	if (IsVar(Va)) {
	  sus_tbl_add(Pa);
	  kernel_suspended();
	}
	kernel_failed();
      }
    }
    Pa = STP;
    if (!ask_unify(Vc, Va)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();

  case div:			/* "/", 3 */
    Va = pc_reg();
    Vb = pc_reg();
    Vc = pc_reg();
    debug_ask_kernel_3("/", Va, Vb, Vc);
    deref(Va, Pa);
    deref(Vb, Pb);
    if (IsInt(Va)) {
      if (IsInt(Vb)) {
	if (Vb == Word(0, IntTag)) {
	  kernel_failed();
	}
	Va = Word(((int) Off_Tag(Va) / (int) Off_Tag(Vb)), IntTag);
      }
      else {
	if (IsReal(Vb)) {
	  if (real_val((Pb+1)) == 0.0) {
	    kernel_failed();
	  }
	  *HP++ = Word(0, RealTag);
	  real_copy_val((Int_Val(Va) / real_val((Pb+1))), HP);
	  HP += realTbits/heapTbits;
	  Va = Ref_Word((HP - (1+(realTbits/heapTbits))));
	}
	else {
	  if (IsVar(Vb)) {
	    sus_tbl_add(Pb);
	    kernel_suspended();
	  }
	  kernel_failed();
	}
      }
    }
    else {
      if (IsReal(Va)) {
	if (IsInt(Vb)) {
	  if (Vb == Word(0, IntTag)) {
	    kernel_failed();
	  }
	  Va = Ref_Word(HP);
	  *HP++ = Word(0, RealTag);
	  real_copy_val((real_val((Pa+1)) / Int_Val(Vb)), HP);
	  HP += realTbits/heapTbits;
	}
	else {
	  if (IsReal(Vb)) {
	    if (real_val((Pb+1)) == 0.0) {
	      kernel_failed();
	    }
	    Va = Ref_Word(HP);
	    *HP++ = Word(0, RealTag);
	    real_copy_val((real_val((Pa+1)) / real_val((Pb+1))), HP);
	    HP += realTbits/heapTbits;
	  }
	  else {
	    if (IsVar(Vb)) {
	      sus_tbl_add(Pb);
	      kernel_suspended();
	    }
	    kernel_failed();
	  }
	}
      }
      else {
	if (IsVar(Va)) {
	  sus_tbl_add(Pa);
	  kernel_suspended();
	}
	kernel_failed();
      }
    }
    Pa = STP;
    if (!ask_unify(Vc, Va)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();

  case mod:			/* "mod", 3 */
    Va = pc_reg();
    Vb = pc_reg();
    Vc = pc_reg();
    debug_ask_kernel_3("mod", Va, Vb, Vc);
    deref(Va, Pa);
    if (!IsInt(Va)) {
      if (IsVar(Va)) {
	sus_tbl_add(Pa);
	kernel_suspended();
      }
      kernel_failed();
    }
    deref(Vb, Pa);
    if (!IsInt(Vb)) {
      if (IsVar(Vb)) {
	sus_tbl_add(Pa);
	kernel_suspended();
      }
      kernel_failed();
    }
    if (Vb == Word(0, IntTag)) {
      kernel_failed();
    }
    Pa = STP;
    if (!ask_unify(Vc, On_Tag((Off_Tag(Va) % Off_Tag(Vb)), IntTag))) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();

  case bitwise_and:		/* "bitwise_and", 3 */
    Va = pc_reg();
    Vb = pc_reg();
    Vc = pc_reg();
    debug_ask_kernel_3("bitwise_and", Va, Vb, Vc);
    deref(Va, Pa);
    if (!IsInt(Va)) {
      if (IsVar(Va)) {
	sus_tbl_add(Pa);
	kernel_suspended();
      }
      kernel_failed();
    }
    deref(Vb, Pa);
    if (!IsInt(Vb)) {
      if (IsVar(Vb)) {
	sus_tbl_add(Pa);
	kernel_suspended();
      }
      kernel_failed();
    }
    Pa = STP;
    if (!ask_unify(Vc, (Va & Vb))) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();

  case bitwise_or:		/* "bitwise_or", 3 */
    Va = pc_reg();
    Vb = pc_reg();
    Vc = pc_reg();
    debug_ask_kernel_3("bitwise_or", Va, Vb, Vc);
    deref(Va, Pa);
    if (!IsInt(Va)) {
      if (IsVar(Va)) {
	sus_tbl_add(Pa);
      }
      kernel_failed();
    }
    deref(Vb, Pa);
    if (!IsInt(Vb)) {
      if (IsVar(Vb)) {
	sus_tbl_add(Pa);
	kernel_suspended();
      }
      kernel_failed();
    }
    Pa = STP;
    if (!ask_unify(Vc, (Va | Vb))) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();

  case list_to_string:		/* "list_to_string", 3 */
    Va = pc_reg();
    Vb = pc_reg();
    Vc = pc_reg();
    debug_ask_kernel_3("list_to_string", Va, Vb, Vc);
    Pa = STP;
    if (!do_list_to_string(Va, Vb)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    Pa = STP;
    if (!ask_unify(Vc, KOutA)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();

  case string_to_dlist:		/* "string_to_dlist", 3 */
    Va = pc_reg();
    Vb = pc_reg();
    Vc = pc_reg();
    debug_ask_kernel_3("string_to_dlist", Va, Vb, Vc);
    Pa = STP;
    if (!do_string_to_dlist(Va, Vc)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    Pa = STP;
    if (!ask_unify(Vb, KOutA)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();

  case arg:			/* "arg", 3 */
    Va = pc_reg();
    Vb = pc_reg();
    Vc = pc_reg();
    debug_ask_kernel_3("arg", Va, Vb, Vc);
    deref(Va, Pa);
    if (!IsInt(Va) || ((int) Va <= IntTag)) {
      if (IsVar(Va)) {
	sus_tbl_add(Pa);
	kernel_suspended();
      }
      kernel_failed();
    }
    deref(Vb, Pa);
    if (!IsTpl(Vb) || (Off_Tag(Va) > Off_Tag(Vb))) {
      if (IsVar(Vb)) {
	sus_tbl_add(Pa);
	kernel_suspended();
      }
      kernel_failed();
    }
    Va = Ref_Word(Pa+Int_Val(Va));
    Pa = STP;
    if (!ask_unify(Vc, Va)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();

  case nth_char:		       /* "nth_char", 3 */
    Va = pc_reg();
    Vb = pc_reg();
    Vc = pc_reg();
    debug_ask_kernel_3("nth_char", Va, Vb, Vc);
    deref(Va, Pa);
    if (!IsInt(Va) || ((int) Va <= IntTag)) {
      if (IsVar(Va)) {
	sus_tbl_add(Pa);
	kernel_suspended();
      }
      kernel_failed();
    }
    deref(Vb, Pa);
    if (!IsStr(Vb) || (Int_Val(Va) > Str_Length(Pa))) {
      if (IsVar(Vb)) {
	sus_tbl_add(Pa);
	kernel_suspended();
      }
      kernel_failed();
    }
    Vb = Word(*(((char *) (Pa + StrHdrWords)) + Int_Val(Va) - 1), IntTag);
    Pa = STP;
    if (!ask_unify(Vc, Vb)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();
    
  case read_vector:		/* "read_vector", 3 */
    Va = pc_reg();
    Vb = pc_reg();
    Vc = pc_reg();
    debug_ask_kernel_3("read_vector", Va, Vb, Vc);
    Pa = STP;
    if (!do_read_vector(Va, Vb)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    Pa = STP;
    if (!ask_unify(Vc, KOutA)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();
    
    /* Ask/4 */

  case freeze:			/* "freeze", 4 */
    Va = pc_reg();
    Vb = pc_reg();
    Vc = pc_reg();
    Vd = pc_reg();
    debug_ask_kernel_4("freeze", Va, Vb, Vc, Vd);
    Pa = STP;
    if (!do_freeze(Va, Vb)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    Pa = STP;
    if (!ask_unify(Vc, KOutA)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    Pa = STP;
    if (!ask_unify(Vd, KOutB)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();
    
    /* Tell/0 Kernels */
    
  case deschedule:		/* "deschedule", 0 */
    debug_tell_kernel_0("deschedule");
    TS = 0;
    kernel_succeeded();
    
    /* Tell/1 */
    
  case machine_output:		/* "machine_output", 1 */
    Va = pc_reg();
    debug_tell_kernel_1("machine_output", Va);
    deref_ptr(McnOutP);
    Pa = STP;
    if (!unify(Va, Ref_Word(Var_Val(*McnOutP)))) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();
    
  case ttyput_byte:		/* "ttyput_byte", 1 */
    Va = pc_reg();
    debug_tell_kernel_1("ttyput_byte", Va);
    Pa = STP;
    if (!do_ttyput_byte(Va)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();
    
  case ttyput_integer:		/* "ttyput_integer", 1 */
    Va = pc_reg();
    debug_tell_kernel_1("ttyput_integer", Va);
    Pa = STP;
    if (!do_ttyput_integer(Va)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();
    
  case ttyput_string:		/* "ttyput_string", 1 */
    Va = pc_reg();
    debug_tell_kernel_1("ttyput_string", Va);
    Pa = STP;
    if (!do_ttyput_string(Va)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();
    
  case make_unshared_g:		/* "make_unshared", 1 */
    Va = pc_reg();
    debug_tell_kernel_1("make_unshared", Va);
    Pa = STP;
    if (!do_make_unshared(Va)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();

    /* Tell/2 */
    
  case debug:			/* "debug", 2 */
    Va = pc_reg();
    Vb = pc_reg();
    debug_tell_kernel_2("debug", Va, Vb);
    Pa = STP;
    if (!do_debug(Va, Vb)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();

  case equals:		/* "=", 2 */
    Va = pc_reg();
    Vb = pc_reg();
    debug_tell_kernel_2("=", Va, Vb);
    Pa = STP;
    if (!unify(Va, Vb)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();
    
  case do_execute:		/* "execute", 2 */
    Va = pc_reg();
    Vb = pc_reg();
    debug_tell_kernel_2("execute", Va, Vb);
    deref(Va, Pa);
    if (!IsInt(Va)) {
      if (IsVar(Va)) {
	sus_tbl_add(Pa);
	kernel_suspended();
      }
      kernel_failed();
    }
    deref(Vb, Pa);
    if (!IsTpl(Vb)) {
      if (IsVar(Vb)) {
	sus_tbl_add(Pa);
	kernel_suspended();
      }
      kernel_failed();
    }
    Ia = Int_Val(Va);
    Pb = STP;
    if (Ia < 0) {
      if (!link_execute(-Ia, Pa)) {
	if (Pb != STP) {
	  kernel_suspended();
	}
	kernel_failed();
      }
      kernel_succeeded();
    }
    if (Ia < (LP - LinkBase)) {
      if (!((*((int (*)()) (LinkBase + Ia)))(Pa))) {
	if (Pb != STP) {
	  kernel_suspended();
	}
	kernel_failed();
      }
      kernel_succeeded();
    }
    kernel_failed();
    
  case close_vector:		/* "close_vector", 2 */
    Va = pc_reg();
    Vb = pc_reg();
    debug_tell_kernel_2("close_vector", Va, Vb);
    Pa = STP;
    if (!do_close_vector(Va, Vb)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();
    
  case priority:		/* "priority", 2 */
    Va = pc_reg();
    Vb = pc_reg();
    debug_tell_kernel_2("priority", Va, Vb);
    Pa = STP;
    if (!do_priority(Va, Vb)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();
    
  case request:			/* "request", 2 */
    Va = pc_reg();
    Vb = pc_reg();
    debug_tell_kernel_2("request", Va, Vb);
    Pa = STP;
    if (!do_request(Va)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    Pa = STP;
    if (!unify(Vb, KOutA)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();
    
  case link:			/* "link", 2 */
    Va = pc_reg();
    Vb = pc_reg();
    debug_tell_kernel_2("link", Va, Vb);
    Pa = STP;
    if (!do_link(Va)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    Pa = STP;
    if (!unify(Vb, KOutA)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();

  case bind_ro_g:	       /* "bind_ro", 2 */
    Va = pc_reg();
    Vb = pc_reg();
    debug_tell_kernel_2("bind_ro", Va, Vb);
    Pa = STP;
    if (!do_bind_ro(Va, Vb)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();

  case make_invalid_g:	       /* "make_invalid", 2 */
    Va = pc_reg();
    Vb = pc_reg();
    debug_tell_kernel_2("make_invalid", Va, Vb);
    Pa = STP;
    if (!do_make_invalid(Va, Vb)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();

    /* Tell/3 */
    
  case activate:		/* "activate", 3 */
    Va = pc_reg();
    Vb = pc_reg();
    Vc = pc_reg();
    debug_tell_kernel_3("activate", Va, Vb, Vc);
    deref(Va, Pa);
    if (IsVar(Va)) {
      sus_tbl_add(Pa);
      kernel_suspended();
    }
    if (IsStr(Va)) {
      if (Str_Type(Pa) != MdlType) {
	kernel_failed();
      }
      Va = Ref_Word(Pa+MdlHdrWords);
      FLs_allocate((PR_Header + 2), Pa, HP);
      pr_init(2, Pa);
      *(Prcdr_PR(Pa)) = Va;
      pr_enqueue(Pa, HighQ);
      Pa = Args_PR(Pa);
    }
    else {
      if (!IsInt(Va) || ((int) Va < IntTag)) {
	kernel_failed();
      }
      FLs_allocate((PR_Header + 3), Pa, HP);
      CG_pr_init(2, Pa);
      *(Prcdr_PR(Pa)) = Va;
      *Index_PR(Pa) = Word(0, IntTag);
      pr_enqueue(Pa, HighQ);
      Pa = Native_Args_PR(Pa);
    }
    Creations++;
    *Pa++ = Vb;
    *Pa = Vc;
    kernel_succeeded();
    
  case melt:			/* "melt", 3 */
    Va = pc_reg();
    Vb = pc_reg();
    Vc = pc_reg();
    debug_tell_kernel_3("melt", Va, Vb, Vc);
    Pa = STP;
    if (!do_melt(Va)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    Pa = STP;
    if (!unify(Vb, KOutA)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    Pa = STP;
    if (!unify(Vc, KOutB)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded(); 
    
  case make_vector:		/* "make_vector", 3 */
    Va = pc_reg();
    Vb = pc_reg();
    Vc = pc_reg();
    debug_tell_kernel_3("make_vector", Va, Vb, Vc);
    Pa = STP;
    if (!do_make_vector(Va)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    Pa = STP;
    if (!unify(Vb, KOutA)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    Pa = STP;
    if (!unify(Vc, KOutB)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded(); 

  case make_shared_g:	       /* "make_shared", 2 */
    Va = pc_reg();
    Vb = pc_reg();
    Vc = pc_reg();
    debug_tell_kernel_3("make_shared", Va, Vb, Vc);
    Pa = STP;
    if (!do_make_shared(Va, Vb, Vc)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded();

    /* Tell/4 */
    
  case store_vector:		/* "store_vector", 4 */
    Va = pc_reg();
    Vb = pc_reg();
    Vc = pc_reg();
    Vd = pc_reg();
    debug_tell_kernel_3("store_vector", Va, Vb, Vc);
    Pa = STP;
    if (!do_store_vector(Va, Vb, Vc)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    Pa = STP;
    if (!unify(Vd, KOutA)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded(); 

  case write_vector:		/* "write_vector", 4 */
    Va = pc_reg();
    Vb = pc_reg();
    Vc = pc_reg();
    Vd = pc_reg();
    debug_tell_kernel_3("write_vector", Va, Vb, Vc);
    Pa = STP;
    if (!do_write_vector(Va, Vb, Vc)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    Pa = STP;
    if (!unify(Vd, KOutA)) {
      if (Pa != STP) {
	kernel_suspended();
      }
      kernel_failed();
    }
    kernel_succeeded(); 
    
  default:
    store_pr_args(Ia, Pa);
    fprintf(DbgFile, "Instructions Switch, Opcode = %x ", *(--PC));
    fprintf(DbgFile, "PC = %x\n", (unsigned int) PC);
    do_exit("Unknown Instruction Opcode", MACHINE, ErUDFDCD, True);
  }
}

int check_pr_retry()
{
  if (in_err_tbl(MACHINE, ErHPSPACE) &&
      ((Creations + Suspensions + Activations + Switches + Reductions +
	Terminations + Collections) != Last_Redo_GC)) {
    sus_tbl_reset();
    err_tbl_reset();
    do_gc();
    Last_Redo_GC = Creations + Suspensions + Activations + Switches + 
      Reductions + Terminations + Collections;
#ifdef DEBUG
    if (Debug_Process) {
      fprintf(DbgFile, "Retrying process after GC\n");
    }
#endif
    return(True);
  }
  return(False);
}

int in_err_tbl(Type, Value)
     int Type, Value;
{
  register heapP SavedERRP = ERRP;
  
  while (!err_tbl_empty()) {
    if ((((int) *ERRP) == Type) && (((int) *(ERRP+1)) == Value)) {
      ERRP = SavedERRP;
      return(True);
    }
    ERRP += 2;
  }
  ERRP = SavedERRP;
  return(False);
}

commited_asgn(Ptr, NewVal)
     register heapP Ptr;
     heapT NewVal;
{
  register heapT V;

  deref_ptr(Ptr);
  V = *Ptr;
  *Ptr = NewVal;
  if (IsZeroed(V)) {
    return 0;
  }
  Ptr = Var_Val(V);
  V = *Ptr;
  *Ptr = NewVal;
  if (IsList(*Var_Val(V))) {
    wakeup_queue(Var_Val(V));
  }
}

wakeup_queue(PQueue)
     heapP PQueue;
{
  if (PQueue != Nil) {
    register heapP P0 = PQueue, P1;
    
    while (IsList(*P0)) {
      P1 = L_Ref_Val(*Ref_SR(P0));
      pr_activate(P1);
      P1 = P0;
      P0 = Ref_Val(*Next_SR(P0));
      FLs_deallocate(SR_Size, P1);
    }
  }
}

#ifdef	DEBUG
set_debug_flags()
{
  Debug_Process = True;
  Debug_Clause = True;
  Debug_Guard = True;
  Debug_Outargs = True;
  Debug_Activation = True;
}

unset_debug_flags()
{
  Debug_Process = False;
  Debug_Clause = False;
  Debug_Guard = False;
  Debug_Outargs = False;
  Debug_Activation = False;
}
#endif

#ifdef DEBUG
print_kernel_0(Type, Name)
     register char *Type, *Name;
{
  if (Debug_Guard) {
    fprintf(DbgFile, "%s kernel: %s()\n", Type, Name);
  }
}

print_kernel_1(Type, Name, Arg1)
     register char *Type, *Name;
     register heapT Arg1;
{
  if (Debug_Guard) {
    fprintf(DbgFile, "%s kernel: %s(", Type, Name);
    print_term(Arg1, Null);
    fprintf(DbgFile, ")\n");
  }
}

print_kernel_2(Type, Name, Arg1, Arg2)
     register char *Type, *Name;
     register heapT Arg1, Arg2;
{
  if (Debug_Guard) {
    fprintf(DbgFile, "%s kernel: %s(", Type, Name);
    print_term(Arg1, Null);
    fprintf(DbgFile, ", ");
    print_term(Arg2, Null);
    fprintf(DbgFile, ")\n");
  }
}

print_kernel_3(Type, Name, Arg1, Arg2, Arg3)
     register char *Type, *Name;
     register heapT Arg1, Arg2, Arg3;
{
  if (Debug_Guard) {
    fprintf(DbgFile, "%s kernel: %s(", Type, Name);
    print_term(Arg1, Null);
    fprintf(DbgFile, ", ");
    print_term(Arg2, Null);
    fprintf(DbgFile, ", ");
    print_term(Arg3, Null);
    fprintf(DbgFile, ")\n");
  }
}

print_kernel_4(Type, Name, Arg1, Arg2, Arg3, Arg4)
     register char *Type, *Name;
     register heapT Arg1, Arg2, Arg3, Arg4;
{
  if (Debug_Guard) {
    fprintf(DbgFile, "%s kernel: %s(", Type, Name);
    print_term(Arg1, Null);
    fprintf(DbgFile, ", ");
    print_term(Arg2, Null);
    fprintf(DbgFile, ", ");
    print_term(Arg3, Null);
    fprintf(DbgFile, ", ");
    print_term(Arg4, Null);
    fprintf(DbgFile, ")\n");
  }
}

print_kernel_5(Type, Name, Arg1, Arg2, Arg3, Arg4, Arg5)
     register char *Type, *Name;
     register heapT Arg1, Arg2, Arg3, Arg4, Arg5;
{
  if (Debug_Guard) {
    fprintf(DbgFile, "%s kernel: %s(", Type, Name);
    print_term(Arg1, Null);
    fprintf(DbgFile, ", ");
    print_term(Arg2, Null);
    fprintf(DbgFile, ", ");
    print_term(Arg3, Null);
    fprintf(DbgFile, ", ");
    print_term(Arg4, Null);
    fprintf(DbgFile, ")\n");
    print_term(Arg5, Null);
    fprintf(DbgFile, ")\n");
  }
}

#endif

