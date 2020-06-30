/*
** This module is part of EFCP.
**

     Copyright 2007 William Silverman
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
       Precompiler for Stochastic Pi Calculus procedures - symbol definitions.
       December 2001.
*/

/* Characters */

BIOSPI_COMMUNICATION_SUFFIX => 39. % Prime
SPIFCP_COMMUNICATION_SUFFIX => 95. % Underscore

/* Strings */

AMBIENT => "Ambient.".
CHOSEN => "Chosen.".
MESSAGE => "Message.".
READY => "Ready.".
SCHEDULER => "Scheduler.".
SCHEDULER_PRIME => "Scheduler.'".
SERVICE => "Service".

/* Capabilities */

ACCEPT => "accept".
ENTER => "enter".

EXIT => "exit".
EXPEL => "expel".
LOCAL => "local".
MERGE => "merge".

/* Directions */

C2P => "c2p".
P2C => "p2c".
S2S => "s2s".

/* Variables */

VAR_AMBIENT => `AMBIENT.
VAR_CHOSEN => `CHOSEN.
VAR_MESSAGE => `MESSAGE.
VAR_NULL => `"_".
VAR_READY => `READY.
VAR_SCHEDULER => `SCHEDULER.
VAR_SCHEDULER_PRIME => `SCHEDULER_PRIME.

