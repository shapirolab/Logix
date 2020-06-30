
    Copyright (C) 2007  William Silverman
    Weizmann Institute of Science - Rehovot, ISRAEL

    This document is part of EFCP.

    EFCP is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    EFCP is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see http://www.gnu.org/licenses/.

    Contact: bill@wisdom.weizmann.ac.il


The Logix system may be installed on cygwin, linux, maci386, macosx, redhat,
sgi, solaris and ubuntu platforms.  To (re)install the Logix system:

    % Prepare the Emulator (initial installation, or when some relevant file
      in EMULATOR has been changed) - see README in Emulator).

    % cd Logix
    % ./install [ <subsystem> [ <additional arguments> ] ]

    <subsystem> is one of logix, spifcp, biospi (default logix).
    <additional arguments> are described in the documentation.

To (re)install on a cygwin, linux, maci386, macosx, redhat, sgi, solaris 
or ubuntu platform as well as on your regular platform, make a clean copy
of EMULATOR with a different name (e.g. CYGWIN, LINUX, MACI386, MACOSX,
REDHAT, SGI, SOLARIS, UBUNTU), prepare the named emulator as above and then:

    % cd Logix
    % ./install <emulator name> [ <subsystem> [ <additional arguments> ] ]

The generated file may be used to start a session on the appropriate
platform and subsystem - e.g.s:

    % Logix/logix
    % Logix/sgi_spifcp
    % Logix/maci386_biospi -h40000

Multiple emulators can share the same Logix system binary.  Every emulator
can support all three subsystems.

However, do NOT run the system from within the Logix Directory, nor from
within any of its sub-directories, except when recompiling internal modules.

During a session, enter:

    @h

for Logix help, and when the spifcp or biospi subsystem is working:

    @ph

for (Ambient) Stochastic Pi Calculus help.

In any case, enter:

    @help

for a full list of available command predicates.

Documentation is available at: http://www.nongnu.org/efcp/ .
