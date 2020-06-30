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



       Free Software Foundation, Inc.
       51 Franklin Street, Fifth Floor
       Boston, MA 02110-1301 USA

       contact: bill@wisdom.weizmann.ac.il

**
*/

-language(nil).

/* FCP/Xview interface event masks. */

FCP_xe_keyboard_press_ascii	=>	1.		% 2**0
FCP_xe_keyboard_release_ascii	=>	2.		% 2**1
FCP_xe_keyboard_press_right	=>	4.		% 2**2
FCP_xe_keyboard_release_right	=>	8.		% 2**3
FCP_xe_keyboard_press_left	=>	16.		% 2**4
FCP_xe_keyboard_release_left	=>	32.		% 2**5
FCP_xe_keyboard_press_top	=>	64.		% 2**6
FCP_xe_keyboard_release_top	=>	128.		% 2**7

FCP_xe_key_press		=>	85.		% or key press
FCP_xe_key_release		=>	170.		% or key release
FCP_xe_keyboard			=>	255.		% or keys

FCP_xe_button_select_press	=>	256.		% 2**8
FCP_xe_button_select_release	=>	512.		% 2**9
FCP_xe_button_adjust_press	=>	1024.		% 2**10
FCP_xe_button_adjust_release	=>	2048.		% 2**11
FCP_xe_button_menu_press	=>	4096.		% 2**12
FCP_xe_button_menu_release	=>	8192.		% 2**13

FCP_xe_button_press		=>	5376.		% or button press
FCP_xe_button_release		=>	10752.		% or button release
FCP_xe_buttons			=>	16128.		% or buttons

FCP_xe_pointer_enter		=>	16384.		% 2**14
FCP_xe_pointer_exit		=>	32768.		% 2**15

FCP_xe_resize			=>	65536.		% 2**16
FCP_xe_repaint			=>	131072.		% 2**17

FCP_xe_window_open		=>	262144.		% 2**18
FCP_xe_window_close		=>	524288.		% 2**19

FCP_xe_notify			=>	1048576.	% 2**20

FCP_xe_button_doubleclick	=>	2097152.	% 2**21

FCP_xe_pointer_drag		=>	4194304.	% 2**22

FCP_xe_all			=>	8388607.	% all on
FCP_xe_none			=>	0.		% all off
