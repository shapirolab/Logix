/*
** This module is part of EFCP.
**

     Copyright 2007 Avshalom Houri, Shmuel Kliger
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


/* Instructions - Codes */

#define	deref_2				0x001
#define	deref_3				0x002
#define deref_subarg_3			0x003
#define deref_subarg_2			0x004
#define	deref_car_3			0x005
#define	deref_car_2			0x006
#define deref_list			0x007
#define deref_car_list			0x008
#define deref_sub_list			0x009
#define deref_vars			0x00a
#define deref_var1			0x00b
#define deref_2_addr			0x00c
#define deref_integer1			0x00d
#define load_car			0x00e
#define deref_list3			0x00f

/* Assembler assumes order in opcodes of load_we_var & load_ref_to_we_var */
#define	load_we_var			0x010
#define	load_ref_to_we_var		0x011

/* Assembler assumes order in opcodes of load_ro_of_reg & 
   load_ref_to_ro_of_reg */
#define	load_ro_of_reg			0x012
#define	load_ref_to_ro_of_reg		0x013

/* Assembler assumes order in opcodes of load_ro_of_subarg & 
   load_ref_to_ro_of_subarg */
#define	load_ro_of_subarg		0x014
#define	load_ref_to_ro_of_subarg	0x015

#define	load_reg     			0x016
#define	load_reg_indirect    		0x017
#define	load_car_of_reg			0x018
#define	load_pr_arg    			0x019
#define	load_subarg    			0x01a
#define	load_ref_to_subarg		0x01b
#define load_nil			0x01c
#define	load_word			0x01d
#define	load_real			0x01e
#define	load_ref_to_real		0x01f
#define	load_ref_to_string		0x020

#define	allocate_var			0x021
#define	allocate_vars			0x022
#define	allocate_tuple			0x023
#define	allocate_list_cell		0x024
#define	allocate_list_we		0x025
#define	allocate_listN			0x026
#define	allocate_pr			0x027
#define list_assign_with_check		0x028
#define list_assign 			0x029

#define fetch				0x02a

/* Assembler assumes order in opcodes of copy_* */

#define	copy_Rs_Rd			0x02b
#define	copy_Rs_CpId			0x02c
#define	copy_Rs_SRd			0x02d

#define	copy_CpIs_Rd			0x02e
#define	copy_CpIs_CpId			0x02f
#define	copy_CpIs_SRd			0x030

#define	copy_SRs_Rd			0x031
#define	copy_SRs_CpId			0x032
#define	copy_SRs_SRd			0x033

#define	copy_RsIs_Rd			0x034
#define	copy_RsIs_CpId			0x035
#define	copy_RsIs_SRd			0x036

#define	copy_RRs_Rd			0x037
#define	copy_RRs_CpId			0x038
#define	copy_RRs_SRd			0x039

#define	copy_CRs_Rd			0x03a
#define	copy_CRs_CpId			0x03b
#define	copy_CRs_SRd			0x03c

#define	copy_ARsIs_Rd			0x03d
#define	copy_ARsIs_CpId			0x03e
#define	copy_ARsIs_SRd			0x03f

#define	copy_RARsIs_Rd			0x040
#define	copy_RARsIs_CpId		0x041
#define	copy_RARsIs_SRd			0x042

#define	copy_Nil_Rd	 		0x043
#define	copy_Nil_CpId			0x044
#define	copy_Nil_SRd	 		0x045

#define	copy_Word_Rd	 		0x046
#define	copy_Word_CpId			0x047
#define	copy_Word_SRd			0x048

#define	copy_Real_Rd	 		0x049
#define	copy_Real_CpId			0x04a
#define	copy_Real_SRd			0x04b

#define	copy_String_Rd			0x04c
#define	copy_String_CpId		0x04d
#define	copy_String_SRd			0x04e

#define	copy_WeVar_Rd			0x04f
#define	copy_WeVar_CpId			0x050
#define	copy_WeVar_SRd			0x051

/* Assembler assumes order in opcodes of asgn_* */

#define	asgn_Rs_SRd			0x052
#define	asgn_CpIs_SRd			0x053
#define	asgn_SRs_SRd			0x054
#define	asgn_RsIs_SRd			0x055
#define	asgn_RRs_SRd			0x056
#define	asgn_CRs_SRd			0x057
#define	asgn_ARsIs_SRd			0x058
#define	asgn_RARsIs_SRd			0x059
#define	asgn_Nil_SRd			0x05a
#define	asgn_Word_SRd			0x05b
#define	asgn_Real_SRd			0x05c
#define	asgn_String_SRd			0x05d
#define	asgn_WeVar_SRd			0x05e

#define	assign_Rs_SRd			0x05f
#define	assign_CpIs_SRd			0x060
#define	assign_SRs_SRd			0x061
#define	assign_RsIs_SRd			0x062
#define	assign_RRs_SRd			0x063
#define	assign_CRs_SRd			0x064
#define	assign_ARsIs_SRd		0x065
#define	assign_RARsIs_SRd		0x066
#define	assign_Nil_SRd			0x067
#define	assign_Word_SRd			0x068
#define	assign_Real_SRd			0x069
#define	assign_String_SRd		0x06a
#define	assign_WeVar_SRd		0x06b

#define	assign_com_Rs_SRd		0x06c
#define	assign_com_CpIs_SRd		0x06d
#define	assign_com_SRs_SRd		0x06e
#define	assign_com_RsIs_SRd		0x06f
#define	assign_com_RRs_SRd		0x070
#define	assign_com_CRs_SRd		0x071
#define	assign_com_ARsIs_SRd		0x072
#define	assign_com_RARsIs_SRd		0x073
#define	assign_com_Nil_SRd		0x074
#define	assign_com_Word_SRd		0x075
#define	assign_com_Real_SRd		0x076
#define	assign_com_String_SRd		0x077
#define	assign_com_WeVar_SRd		0x078

#define	assign_com_tr_Rs_SRd		0x079
#define	assign_com_tr_CpIs_SRd		0x07a
#define	assign_com_tr_SRs_SRd		0x07b
#define	assign_com_tr_RsIs_SRd		0x07c
#define	assign_com_tr_RRs_SRd		0x07d
#define	assign_com_tr_CRs_SRd		0x07e
#define	assign_com_tr_ARsIs_SRd		0x07f
#define	assign_com_tr_RARsIs_SRd	0x080
#define	assign_com_tr_Nil_SRd		0x081
#define	assign_com_tr_Word_SRd		0x082
#define	assign_com_tr_Real_SRd		0x083
#define	assign_com_tr_String_SRd	0x084
#define	assign_com_tr_WeVar_SRd		0x085

#define assign_and_inc       		0x086
#define assign_inc_com       		0x087
#define assign_inc_com_trail 		0x088

#define	goto_there			0x089

#define	if_not_reference		0x08a
#define	if_not_variable			0x08b
#define	if_not_writable			0x08c
#define	if_not_read_only		0x08d
#define	if_not_integer			0x08e
#define	if_not_real			0x08f
#define	if_not_string			0x090
#define	if_not_nil			0x091
#define	if_not_list			0x092
#define	if_not_tuple			0x093
#define	if_not_vector        		0x094

/* Assembler assumes order in opcodes of if_int_* */
#define	if_int_lt			0x095
#define	if_int_bt			0x096
#define	if_int_le			0x097
#define	if_int_be			0x098
#define	if_int_eq			0x099
#define	if_int_neq			0x09a

/* Assembler assumes order in opcodes of if_real_* */
#define	if_real_lt			0x09b
#define	if_real_bt			0x09c
#define	if_real_le			0x09d
#define	if_real_be			0x09e
#define	if_real_eq			0x09f
#define	if_real_neq			0x0a0

/* Assembler assumes order in opcodes of if_str_* */
#define	if_str_lt			0x0a1
#define	if_str_bt			0x0a2
#define	if_str_le			0x0a3
#define	if_str_be			0x0a4
#define	if_str_eq			0x0a5
#define	if_str_neq			0x0a6

/* Assembler assumes order in opcodes of if_tuple_* */
#define	if_tuple_lt			0x0a7
#define	if_tuple_bt			0x0a8
#define	if_tuple_le			0x0a9
#define	if_tuple_be			0x0aa
#define	if_tuple_eq			0x0ab
#define	if_tuple_neq			0x0ac

#define	switch_on_tag			0x0ad
#define unify_args			0x0ae

/* Assembler assumes order in opcodes of unify_reg_* */
#define unify_reg_reg			0x0af
#define unify_reg_xreg			0x0b0
#define unify_reg_axreg			0x0b1
#define unify_reg_roreg			0x0b2
#define unify_reg_roaxreg		0x0b3
#define unify_reg_carreg		0x0b4
#define unify_reg_word			0x0b5
#define unify_reg_string		0x0b6
#define unify_reg_real			0x0b7
#define unify_reg_nil			0x0b8

/* Assembler assumes order in opcodes of unify_xreg_* */
#define unify_xreg_reg			0x0b9
#define unify_xreg_xreg			0x0ba
#define unify_xreg_axreg		0x0bb
#define unify_xreg_roreg		0x0bc
#define unify_xreg_roaxreg		0x0bd
#define unify_xreg_carreg		0x0be
#define unify_xreg_word			0x0bf
#define unify_xreg_string		0x0c0
#define unify_xreg_real			0x0c1
#define unify_xreg_nil			0x0c2

#define	branch_integer			0x0c3
#define	branch_real			0x0c4
#define	branch_tuple			0x0c5

#define	case_hash_integer		0x0c6
#define	case_hash_string		0x0c7

/* Assembler assumes order in opcodes of cmp_int_* */
#define cmp_int_lt			0x0c8
#define cmp_int_bt			0x0c9
#define cmp_int_le			0x0ca
#define cmp_int_be			0x0cb
#define cmp_int_eq			0x0cc
#define cmp_int_ne			0x0cd

#define decrement_2_reg			0x0ce
#define decrement_2_xreg		0x0cf
#define decrement			0x0d0
#define decrement_pointer		0x0d1
#define decrement_2_pointer		0x0d2
#define increment_2_reg			0x0d3
#define increment_2_xreg		0x0d4
#define increment			0x0d5
#define increment_pointer		0x0d6
#define increment_2_pointer		0x0d7

#define increment_and_commit 		0x0d8
#define decrement_and_commit 		0x0d9

/* Assembler assumes order in opcodes of plus_* */
#define plus_reg_reg_reg		0x0da
#define plus_reg_reg_xreg		0x0db
#define plus_reg_int_reg		0x0dc
#define plus_reg_int_xreg		0x0dd
#define plus_int_reg_reg		0x0de
#define plus_int_reg_xreg		0x0df
#define plus_int_int_reg		0x0e0
#define plus_int_int_xreg		0x0e1

/* Assembler assumes order in opcodes of plusnum_* */
#define plusnum_reg_reg			0x0e2
#define plusnum_reg_int			0x0e3
#define plusnum_reg_real		0x0e4
#define plusnum_int_reg			0x0e5
#define plusnum_int_int			0x0e6
#define plusnum_int_real		0x0e7
#define plusnum_real_reg		0x0e8
#define plusnum_real_int		0x0e9
#define plusnum_real_real		0x0ea

/* Assembler assumes order in opcodes of diff_* */
#define diff_reg_reg_reg		0x0eb
#define diff_reg_reg_xreg		0x0ec
#define diff_reg_int_reg		0x0ed
#define diff_reg_int_xreg		0x0ee
#define diff_int_reg_reg		0x0ef
#define diff_int_reg_xreg		0x0f0
#define diff_int_int_reg		0x0f1
#define diff_int_int_xreg		0x0f2

/* Assembler assumes order in opcodes of diffnum_* */
#define diffnum_reg_reg			0x0f3
#define diffnum_reg_int			0x0f4
#define diffnum_reg_real		0x0f5
#define diffnum_int_reg			0x0f6
#define diffnum_int_int			0x0f7
#define diffnum_int_real     		0x0f8
#define diffnum_real_reg		0x0f9
#define diffnum_real_int		0x0fa
#define diffnum_real_real		0x0fb

/* Assembler assumes order in opcodes of cmt_plus_* */
#define cmt_plus_reg_reg_reg	        0x0fc
#define cmt_plus_reg_reg_xreg		0x0fd
#define cmt_plus_reg_int_reg	        0x0fe
#define cmt_plus_reg_int_xreg		0x0ff
#define cmt_plus_int_reg_reg	        0x100
#define cmt_plus_int_reg_xreg		0x101
#define cmt_plus_int_int_reg	        0x102
#define cmt_plus_int_int_xreg		0x103

/* Assembler assumes order in opcodes of cmt_plusnum_* */
#define cmt_plusnum_reg_reg	        0x104
#define cmt_plusnum_reg_int	        0x105
#define cmt_plusnum_reg_real	        0x106
#define cmt_plusnum_int_reg	        0x107
#define cmt_plusnum_int_int	        0x108
#define cmt_plusnum_int_real	        0x109
#define cmt_plusnum_real_reg	        0x10a
#define cmt_plusnum_real_int	        0x10b
#define cmt_plusnum_real_real		0x10c

/* Assembler assumes order in opcodes of cmt_diff_* */
#define cmt_diff_reg_reg_reg	        0x10d
#define cmt_diff_reg_reg_xreg		0x10e
#define cmt_diff_reg_int_reg	        0x10f
#define cmt_diff_reg_int_xreg		0x110
#define cmt_diff_int_reg_reg	        0x111
#define cmt_diff_int_reg_xreg		0x112
#define cmt_diff_int_int_reg	        0x113
#define cmt_diff_int_int_xreg		0x114

/* Assembler assumes order in opcodes of cmt_diffnum_* */
#define cmt_diffnum_reg_reg	        0x115
#define cmt_diffnum_reg_int	        0x116
#define cmt_diffnum_reg_real	        0x117
#define cmt_diffnum_int_reg	        0x118
#define cmt_diffnum_int_int	        0x119
#define cmt_diffnum_int_real	        0x11a
#define cmt_diffnum_real_reg	        0x11b
#define cmt_diffnum_real_int	        0x11c
#define cmt_diffnum_real_real		0x11d

#define	enqueue				0x11e
#define iterate1			0x11f
#define	iterate				0x120
#define execute				0x121
#define execute2			0x122
#define execute1			0x123
#define	halt				0x124
#define	commit1				0x125
#define commit_nolabel			0x126
#define	commit0				0x127
#define	set_cp_arity			0x128
#define	suspend2			0x129
#define	suspend1			0x12a
#define	suspend0			0x12b
#define	suspend_on			0x12c

#define	set_HBT				0x12d
#define	undo				0x12e

#define drf2_if_not_nil			0x12f
#define	drf2_if_not_list		0x130

#define drf2_if_int_neq			0x131
#define drf2_if_real_neq		0x132
#define drf2_if_str_neq			0x133
#define drf2_if_tuple_neq		0x134

#define	drf2_switch_on_tag		0x135

#define drf2_branch_integer		0x136
#define drf2_branch_real		0x137
#define drf2_branch_tuple		0x138

#define drf2_hash_integer		0x139
#define drf2_hash_string		0x13a

#define if_var_suspend			0x13b
#define	drf1_if_var_suspend		0x13c

#define alloc_pr_enqu			0x13d
#define alloc_pr_regs_enqu		0x13e

#define	mult_operation			0x13f

/*
** Kernels Codes
*/

/* Ask/0 */

#define	otherwise	0x140	/* "otherwise", 0 */

/* Ask/1 */

#define	is_nonvar	0x141	/* "nonvar", 1 */
#define	ask_unknown	0x142	/* "ask_unknown", 1 */
#define	is_known	0x143	/* "known", 1 */
#define	is_var		0x144	/* "var", 1 */
#define is_we		0x145	/* "we", 1 */
#define is_not_we	0x146	/* "not_we", 1 */
#define	is_ro		0x147	/* "ro", 1 */
#define	is_integer	0x148	/* "integer", 1 */
#define	is_real		0x149	/* "real", 1 */
#define	is_string	0x14a	/* "string",1 */
#define	is_list		0x14b	/* "list", 1 */
#define	is_tuple	0x14c	/* "tuple", 1 */
#define	is_vector	0x14d	/* "vector", 1 */
#define	is_module	0x14e	/* "module", 1 */
#define	is_constant	0x14f	/* "constant", 1 */
#define	is_compound	0x150	/* "compound", 1 */
#define	is_number	0x151	/* "number", 1 */
#define grounded	0x152	/* "grounded", 1 */

/* Ask/2 */

#define	wait_equals	0x153	/* "=?=", 2 */
#define	wait_not_equals	0x154	/* "=\=", 2 */
#define	is_less		0x155	/* "@<", 2 */
#define	not_identical	0x156	/* "\=", 2 */
#define	lt		0x157	/* "<", 2 */
#define	le		0x158	/* "=<", 2 */
#define	bitwise_not	0x159	/* "bitwise_not", 2 */
#define	string_length	0x15a	/* "string_length", 2 */
#define	string_hash	0x15b	/* "string_hash", 2 */
#define	cnv_to_integer	0x15c	/* "convert_to_integer", 2 */
#define	cnv_to_real	0x15d	/* "convert_to_real", 2 */
#define	cnv_to_string	0x15e	/* "convert_to_string", 2 */
#define	arity		0x15f	/* "arity", 2 */
#define	make_tuple	0x160	/* "make_tuple", 2 */
#define	info		0x161	/* "info", 2 */
#define	code_info	0x162	/* "code_info", 2 */
#define	invalid_g	0x163	/* "invalid",	2 */
#define var_info_g	0x164	/* "var_info",	2 */

/* Ask/3 */

#define	plus		0x165	/* "plus", 3 */
#define	diff		0x166	/* "diff", 3 */
#define	times		0x167	/* "times", 3 */
#define	div		0x168	/* "div", 3 */
#define	mod		0x169	/* "mod", 3 */
#define	bitwise_and	0x16a	/* "bitwise_and", 3 */
#define	bitwise_or	0x16b	/* "bitwise_or", 3 */
#define	list_to_string	0x16c	/* "list_to_string", 3 */
#define	string_to_dlist	0x16d	/* "string_to_dlist", 3 */
#define	arg		0x16e	/* "arg", 3 */
#define	read_vector	0x16f	/* "read_vector", 3 */

/* Ask/4 */

#define freeze		0x170	/* "freeze", 4 */

/* Tell/0 */

#define	deschedule	0x172	/* "deschedule", 0 */

/* Tell/1 */

#define	machine_output	0x173	/* "machine_output", 1 */
#define	ttyput_byte	0x174	/* "ttyput_byte", 1 */
#define	ttyput_integer	0x175	/* "ttyput_integer", 1 */
#define	ttyput_string	0x176	/* "ttyput_string", 1 */
#define make_unshared_g	0x177	/* "make_unshared", 1 */

/* Tell/2 */

#define	debug		0x178	/* "debug", 2 */
#define	equals		0x179	/* "=", 2 */
#define	do_execute	0x17a	/* "execute", 2 */
#define	close_vector	0x17b	/* "close_vector", 2 */
#define priority	0x17c	/* "priority", 2 */
#define	request		0x17d	/* "request", 2 */
#define	link		0x17e	/* "link", 2 */
#define bind_ro_g	0x17f	/* "bind_ro", 2 */
#define make_invalid_g	0x180	/* "make_invalid", 2 */

/* Tell/3 */

#define	activate	0x181	/* "activate", 3 */
#define melt		0x182	/* "melt", 3 */
#define	make_vector	0x183	/* "make_vector", 3 */
#define	make_shared_g	0x184	/* "make_shared", 3 */

/* Tell/4 */

#define	store_vector	0x185	/* "store_vector", 4 */
#define	write_vector	0x186	/* "write_vector", 4 */

/* Old kernels. To be removed */

#define	identical	0x187	/* "==", 2 */
#define exceptions	0x188	/* "exceptions", 1 */

/* New kernel - Ask/3 */

#define nth_char	0x189	/* "nth_char", 3 */
