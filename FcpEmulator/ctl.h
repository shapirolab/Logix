/* $Header: /home/qiana/Repository/FcpEmulator/ctl.h,v 1.1.1.1 1999/07/01 07:15:09 bill Exp $ */
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
