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

/*
**	ctl.h - definitions for abstract machine code.
*/

#define	AnyEscape		0x00
#define	InfoOffsetEscape	0x01
#define	LabelOffsetEscape	0x02
#define	StringOffsetEscape	0x03
#define	IntegerWordValueEscape	0x04
#define	TupleWordArityEscape	0x05
#define	RegisterEscape		0x06
#define	ProcessArgEscape	0x07
#define	ModuleNameOffsetEscape	0x08
#define	PrcdrInfoEscape		0x09
#define	IndexedArgEscape	0x0a
#define	ProceduralEscape	0x0c
#define	IterativeEscape		0x0d
#define	RealValueEscape		0x10

#define MinorItemSize		2
#define RoundUp			2
#define BranchAddressSize	4
#define RescaleOffset		1
#define MinBranchAddress        -8388607
#define MaxBranchAddress	8388607
#define MajorAllignment		4
#define MajorItemSize		4

#define StringHdrSize		8
#define RealSize		8
#define TupleAritySize		4
#define IntegerSize		4
#define StringOffsetSize	4
