/* $Header: /home/qiana/Repository/Logix/system/ndg/assemble/self.cp,v 1.1 1999/07/09 07:02:59 bill Exp $ */
-language(compound).
-export([ctl/2, ctl/4]).
-mode(trust).


/****************************** N O T E ***************************************

The best debugging aid for this service is the service decode.  That service
derives a list ( Pretty ) of assembler instructions similar to  Display  of
ctl/4  below, from  Contents .  Comparison of  Display  to  Pretty  is the
most effective means of detecting bugs in  assemble .

Therefore, it is important to maintain  decode  in parallel with  assemble .
If you dont, you'll be SORRY.  If you do, you may be irritated, but the day
will come when your efforts will be rewarded, and you will bless this advice.

			T R U S T   M E !

******************************************************************************/

CTLs ::= [CTL].
CTL ::= {ProcedureId, Instructions} .
ProcedureId ::= {ProcedureName, Integer, Integer} .
Instructions ::= [Instruction] .

Instruction ::= deref(Register, Register, Register) ;
		deref(IndexedRef, Register, Register) ;
		deref(Register, Register) ;	
		deref(IndexedRef, Register) ;	
		deref_car(Register, Register, Register) ;

		deref_value(Register, Register) ;
		deref_car(Register, Register) ;
		old_deref_list(Register, Register, LabelRef) ;
		deref_car_value(Register, Register);	
		deref_list(Register, Register, LabelRef) ;	
		deref_list(IndexedRef, Register, Register, Label); 
		deref_car_list(Register, Register, Register,LabelRef) ;	
		deref_vars(Regs, LabelRef) ;	

		deref_integer(Register, Register, Integer, LabelRef) ;
		deref_real(Register, Register, Real, LabelRef) ;
		deref_string(Register, Register, String, LabelRef) ;
		deref_nil(Register, Register, LabelRef) ;
		deref_tuple(Register, Register, Integer, LabelRef) ;
		load_car(Register, Register) ;

		if_integer(Register, "=\=", Integer, LabelRef) ;
		if_tuple(Register, "=\=", Count, LabelRef) ;
		if_real(Register, "=\=", Real, LabelRef) ;
		if_string(Register, "=\=", String, LabelRef) ;
		if_tag(Register, "=\=", Tag, LabelRef) ;

		switch_on_tag(Register, TagLabelList) ;

		branch_integer(Register, Low, High, LabelRefs) ; % High-Low+1
		branch_tuple(Register, Low, High, LabelRefs) ;	% #(LabelRefs)

		case_hash_integer(Register, LabelRefs) ;
		case_hash_string(Register, LabelRefs) ;
		compare_integer(IntArg, Pred, IntArg, LabelRef) ;
		
		call(GuardPredicateName, Arguments, LabelRef) ;
		unify(Asgns, Arguments, LabelRef) ;

		allocate_var(Register) ;
		allocate_vars(Regs) ;
		allocate_tuple(Arguments, Register) ;
		allocate_list(Argument, Argument, Register) ;
		allocate_listN(Arguments, Register) ;
		allocate_pr(Arguments, Register) ;

		multiple_copy(FromTerms, ToRegisters) ;
		%		#(FromTerms) =  #(ToRegisters)

		multiple_assign(FromTerms, ToRegArgs) ;
		multiple_assign_and_commit(FromTerms, ToRegArgs) ;
		multiple_assign_and_commit_trail(FromTerms, ToRegArgs) ;

		assign_and_increment(FromRegister, ToRegister) ;
		assign_increment_and_commit(FromRegister, ToRegister) ;
		assign_increment_and_commit_trail(FromRegister, ToRegister) ;


		goto(LabelRef) ;

		fetch(Compound, Register) ;

		execute(LabelRef, LabelRef) ;

		iterate(LabelRef) ;

		deschedule ;

		halt ;

		commit(LabelRef) ;

		decrement(Register, Register) ;
		decrement(Register, IndexedRef) ;
		decrement_and_commit(Register, Register) ;
		decrement(Register) ;
		derement_pointer(Register) ;
		decrement_pointer(Register, Register) ;
		increment(Register, Register) ;
		increment_and_commit(Register, Register) ;
		increment(Register, IndexedRef) ;
		increment(Register) ;
		increment_pointer(Register) ;
		increment_pointer(Register, Register) ;
		plus(IntArg1, IntArg2, RegArg) ;
	        plus_and_commit(Register, Register) ;
		plus_number(NumArg1, NumArg2, Register) ;
		plus_number_and_commit(NumArg1, NumArg2, Register) ;
		diff(IntArg1, IntArg2, RegArg) ;
		diff_and_commit(Register, Register) ;
		diff_number(NumArg1, NumArg2, Register) ;
		diff_number_and_commit(NumArg1, NumArg2, Register) ;

		enqueue(Label, Register) ;

		suspend(LabelRef, LabelRef) ;

		suspend_on(Register) ;

		trail(Register) ;

		set_HBT ;

		undo ;

		list_assign_with_check ;
		list_assign ;
		LabelDef .

LabelDef, LabelRef ::= label(Integer(Integer)).

Arg ::= Register ; IndexedRef ; car(Register) .
Register ::= a(Number) .
Regs ::= [Register] ; [Register | Regs] .
IntArg ::= Register ; Integer .
RegArg ::= Register ; {Register, OffsetP} .
Number, Index, Count, Arity, OpCode ::= SmallInteger .
SmallInteger ::= Integer .			% 0 =< SmallInteger < 65536
IndexedRef ::= "&"({Number, Index}) .
Indirect ::= "*"(Register).
ModuleName, ProcedureName, GuardPredicateName ::= String .

Argument ::= Arg ; RoReference ; AnonVar ; Constant ;
		integer(Integer) ; tuple(Integer) .
RoReference ::= ro(Register) ; ro(IndexedRef) .
AnonVar ::= {"_"} .

FromTerm, AssignTerm ::= Arg ; RoReference ; "PR"(Number) .

ToRegister ::= Register ; "PR"(Number) .

Tag ::= reference ; variable ; writable ; ro ; list ;
	private_we ; shared_we ; private_ro ; shared_ro ;
	integer ; real ; string ; nil ;
	list_reference ; list_integer ; list_nil ;
	tuple ; vector .

Arguments ::= [Argument] .
Asgns ::= [RegArg] ; [RegArg | Asgns] .
FromTerms ::= [FromTerm] .
ToRegisters ::= [ToRegister] .
TagLabelList ::= [TagLabel, LabelRef] ; [TagLabel | TagLabelList] .
TagLabel ::= {Tag, LabelRef} .

LabelRefs ::= [LabelRef] .
Low, High ::= Integer .


ArgCode ::= RegisterAC  ; IndirectAC   ; ReferenceAC   ; RoAC   ; CarAC   ;
	    RegisterXAC ; IndirectXAC  ; ReferenceXAC  ; RoXAC  ;%CarXAC  ;
	    PRAC ;       %IndirectPRAC ; ReferencePRAC ; RoPRAC ; CarPRAC ;
	    IntegerAC ; RealAC ; StringAC ; NilAC ; VariablAC .

% Dereference

Deref		::=  47.	% deref(a(I),a(J),a(K))		47,I,J,K
DerefVal	::=  48.	% deref_value(a(I),a(K))	48,I,K
DerefSubVal	::=  49.	% deref({a(I),IX},a(J),a(K))	49,I,IX,J,K
DerefSub	::=  58.	% deref({a(I),IX},a(K))		58,I,IX,J
DerefCar3	::=  63.	% deref_car(a(I),a(J),a(K))	63,I,J,K
DerefCar	::=  64.	% deref_car(a(I),a(J))		64,I,J
DerefList	::= 301.	% deref_list(a(I),a(J), Label)
%					301,I,J,LabelReference
DerefCarList	::= 302.	% deref_car_list(a(I),a(J), a(K), Label)
%					302,I,J, K,LabelReference
DerefSubList	::= 303.	% deref_list({a(I),IX},a(J),a(K), Label)
%					303,I,IX,J,K,LabelReference
DerefVars	::= 307.	%deref_vars(Regs, Label)
%					307,N,Reg1,Reg2...RegN,LabelReference
DerefVar1	::= 409.	%deref_vars(Reg, Label) (Optimization)
%					409,Reg,LabelReference
Deref2		::= 413.	%deref(a(I), a(J) )		413,I,J
DerefInteger1	::= 550.		%deref_integer(a(I), [Label1, Label2])
%					414,I,J,LabelReference,LabelReference
LoadCar		::= 415.	%load_car(a(I),a(J)) 		415,I,J
DerefList3	::= 420.	%deref_list(a(I),a(J),a(K),Label)
%					420,I,J,K,LabelReference

% Operands

RegisterAC	::=  65.	% a(I)				65,I
PRAC		::=  66.	% PR(I)				66,I
IndirectAC	::=  67.	% *(a(I))			67,I
RegisterXAC	::=  68.	% {a(I),IX}			68,I,IX
RoAC		::=  69.	% ro(a(I))			69,I
RoACref		::=  70.	% ro(a(I))			70,I
%							(cdr or last tuple arg)
CarAC		::=  76.	% car(a(I))			76,I
ReferenceXAC	::=  77.	% &({a(I),IX})			77,I,IX
RoXAC		::=  78.	% ro(&({a(I),IX}))		78,I,IX
RoXACref	::=  79.	% ro(&({a(I),IX}))		79,I,IX
%							(car or process arg)
NilAC		::=  80.	% []				80
IntegerAC	::=  88.	% Integer		88,IntegerEscape
TupleAC		::=  88.	% Arity			88,TupleEscape
RealAC		::=  89.	% Real			89,RealEscape
%							(cdr or last tuple arg)
RealACref	::=  93.	% Real			93,RealEscape
StringAC	::=  94.	% String		94,StringReference
VariableAC 	::=  95.	% {'_'}				95
VariableACref 	::=  96.	% {'_'}				96
%							(cdr or last tuple arg)

% Allocate

AllocateVar	::=  97.	% allocate_var(a(I))		97,I
AllocateVars	::= 410.	% allocate_vars(Regs)		410, N, Ii,...In
AllocateTuple	::=  98.	% allocate_tuple([A1...AN],a(I))
%								98,I,N, Args
AllocateListCell::=  99.	% allocate_list([A1|A2],a(I))
%							    99,I, Arg1, Arg2
AllocateListWe	::= 100.	% allocate_list([a(J)|{'_'}],a(I))
%								100,I,J
AllocateListN	::= 101.	% allocate_listN([A1...|AN],a(I))
%								101,I,N, Args
AllocatePR	::= 102.	% allocate_pr([A1...AN],a(I))	102,I,N, Args

Fetch		::= 103.	% fetch(Term,a(I))    97,I, FrozenTermReference

% Copy Source _ Destination

CopyRR		::= 104.	% a(J) <= a(I)			104,I,J
CopyRP		::= 105.	% PR(J) <= a(I)			105,I,J
CopyRI		::= 106.	% *(a(J)) <= a(I)		106,I,J

CopyPR		::= 107.	% a(J) <= PR(I)			107,I,J
CopyPP		::= 108.	% PR(J) <= PR(I) 		108,I,J
CopyPI		::= 109.	% *(a(J)) <= PR(I)		109,I,J

CopyIR		::= 110.	% a(J) <= *(a(I))		110,I,J
CopyIP		::= 111.	% PR(J) <= *(a(I))		111,I,J
CopyII		::= 112.	% *(a(J)) <= *(a(I))		112,I,J

CopyXR		::= 113.	% a(J) <= {a(I),IX}		113,I,IX,J
CopyXP		::= 114.	% PR(J) <= {a(I),IX}		114,I,IX,J
CopyXI		::= 115.	% *(a(J)) <= {a(I),IX}		115,I,IX,J

CopyQR		::= 116.	% a(J) <= ro(a(I))		116,I,J
CopyQP		::= 117.	% PR(J) <= ro(a(I))		117,I,J
CopyQI		::= 118.	% *(a(J)) <= ro(a(I))		118,I,J

CopyCR		::= 119.	% a(J) <= car(a(I))		119,I,J
CopyCP		::= 120.	% PR(J) <= car(a(I))		120,I,J
CopyCI		::= 121.	% *(a(J)) <= car(a(I))		121,I,J

CopyAR		::= 122.	% a(J) <= &({a(I),IX})		122,I,IX,J
CopyAP		::= 123.	% PR(J) <= &({a(I),IX})		123,I,IX,J
CopyAI		::= 124.	% *(a(J)) <= &({a(I),IX})	124,I,IX,J

CopyBR		::= 125.	% a(J) <= ro(&({a(I),IX}))	125,I,IX,J
CopyBP		::= 126.	% PR(J) <= ro(&({a(I),IX}))	126,I,IX,J
CopyBI		::= 127.	% *(a(J)) <= ro(&({a(I),IX}))	127,I,IX,J

CopyNR		::= 128.	% a(I) <= []			128, I
CopyNP		::= 129.	% PR(I) <= []			129, I
CopyNI		::= 130.	% *(a(I)) <= []			130, I

CopyWR		::= 131.	% a(I) <= WordArgument		131, Word,I
CopyWP		::= 132.	% PR(I) <= WordArgument		132, Word,I
CopyWI		::= 133.	% *(a(I)) <= WordArgument	133, Word,I

CopyFR		::= 134.	% a(I) <= Real			134, Real,I
CopyFP		::= 135.	% PR(I) <= Real			135, Real,I
CopyFI		::= 136.	% *(a(I)) <= Real		136, Real,I

CopySR		::= 137.	% a(I) <= String		137, String,I
CopySP		::= 138.	% PR(I) <= String		138, String,I
CopySI		::= 139.	% *(a(I)) <= String		139, String,I

CopyVR		::= 140.	% a(I) <= {"_"}			140, I
CopyVP		::= 141.	% PR(I) <= {"_"}		141, I
CopyVI		::= 142.	% *(a(I)) <= {"_"}		142, I

% New Assignment (multiple_assign) Instructions: No trailing done

AssignR		::= 422.	% *(a(J)) := a(I)		422,I,J
AssignP		::= 423.	% *(a(J)) := PR(I)		423,I,J
AssignI		::= 424.
AssignX		::= 425.
AssignQ		::= 426.
AssignC		::= 427.
AssignA		::= 428.
AssignB		::= 429.
AssignN		::= 430.
AssignW		::= 431.
AssignF		::= 432.
AssignS		::= 433.
AssignV		::= 434.

% multiple_assign_and_commit

ComAssignR      ::= 435.
ComAssignP      ::= 436.
ComAssignI	::= 437.
ComAssignX	::= 438.
ComAssignQ	::= 439.
ComAssignC	::= 440.
ComAssignA	::= 441.
ComAssignB	::= 442.
ComAssignN	::= 443.
ComAssignW	::= 444.
ComAssignF	::= 445.
ComAssignS	::= 446.
ComAssignV	::= 447.

% multiple_assign_and_commit_trail

TrComAssignR    ::= 448.
TrComAssignP    ::= 449.
TrComAssignI	::= 450.
TrComAssignX	::= 451.
TrComAssignQ	::= 452.
TrComAssignC	::= 453.
TrComAssignA	::= 454.
TrComAssignB	::= 455.
TrComAssignN	::= 456.
TrComAssignW	::= 457.
TrComAssignF	::= 458.
TrComAssignS	::= 459.
TrComAssignV	::= 460.

AssignInc       ::= 461.        % assign_and_increment(a(I),a(J)) 461,I,J
AssignIncCom    ::= 462.        % assign_increment_and_commit     462,I,J
AssignIncComTr  ::= 463.        % assign_increment_and_commit_trail 463,I,J

Goto		::= 156.	% goto(Label)		   156, LabelReference

IfNotReference	::= 157.	% if_tag(a(I),'=\=',"reference",Label)
%							 157,I, LabelReference
IfNotVariable	::= 158.	% if_tag(a(I),'=\=',"variable",Label)
%							 158,I, LabelReference
IfNotWritable	::= 159.	% if_tag(a(I),'=\=',"we",Label)
%							 159,I, LabelReference
IfNotPrivateWe	::= 160.	% if_tag(a(I),'=\=',"private_we",Label)
%							 160,I, LabelReference
IfNotSharedWe	::= 161.	% if_tag(a(I),'=\=',"shared_we",Label)
%							 161,I, LabelReference
IfNotRo		::= 162.	% if_tag(a(I),'=\=',"ro",Label)
%							 162,I, LabelReference
IfNotPrivateRo	::= 163.	% if_tag(a(I),'=\=',"private_ro",Label)
%							 163,I, LabelReference
IfNotSharedRo	::= 164.	% if_tag(a(I),'=\=',"shared_ro",Label)
%							 164,I, LabelReference
IfNotInteger	::= 165.	% if_tag(a(I),'=\=',"integer",Label)
%							 165,I, LabelReference
IfNotReal	::= 166.	% if_tag(a(I),'=\=',"real",Label)
%							 166,I, LabelReference
IfNotString	::= 167.	% if_tag(a(I),'=\=',"string",Label)
%							 167,I, LabelReference
IfNotNil	::= 168.	% if_tag(a(I),'=\=',"nil",Label)
%							 168,I, LabelReference
IfNotList	::= 169.	% if_tag(a(I),'=\=',"list",Label)
%							 169,I, LabelReference
IfNotTuple	::= 170.	% if_tag(a(I),'=\=',"tuple",Label)
%							 170,I, LabelReference
IfNotVector	::= 171.	% if_tag(a(I),'=\=',"vector",Label)
%							 171,I, LabelReference


IfIntegerLess	::= 172.	% if_integer(a(I),'<',a(J),Label)
%							172,I,J, LabelReference
IfIntegerGreater  ::= 173.	% if_integer(a(I),'>',a(J),Label)
%							173,I,J, LabelReference
IfIntegerLessEq	::= 174.	% if_integer(a(I),'=<',a(J),Label)
%							174,I,J, LabelReference
IfIntegerGreaterEq  ::= 175.	% if_integer(a(I),'>=',a(J),Label)
%							175,I,J, LabelReference
IfIntegerEqual	::= 176.	% if_integer(a(I),'=',a(J),Label)
%							176,I,J, LabelReference
IfIntegerNotEqual ::= 177.	% if_integer(a(I),'=\=',a(J),Label)
%							177,I,J, LabelReference

IfRealLess	::= 178.	% if_real(a(I),'<',a(J),Label)
%							178,I,J, LabelReference
IfRealGreater	::= 179.	% if_real(a(I),'>',a(J),Label)
%							179,I,J, LabelReference
IfRealLessEq	::= 180.	% if_real(a(I),'=<',a(J),Label)
%							180,I,J, LabelReference
IfRealGreaterEq	::= 181.	% if_real(a(I),'>=',a(J),Label)
%							181,I,J, LabelReference
IfRealEqual	::= 182.	% if_real(a(I),'=',a(J),Label)
%							182,I,J, LabelReference
IfRealNotEqual	::= 183.	% if_real(a(I),'=\=',a(J),Label)
%							183,I,J, LabelReference

IfStringLess	::= 184.	% if_string(a(I),'<',a(J),Label)
%							184,I,J, LabelReference
IfStringGreater	::= 185.	% if_string(a(I),'>',a(J),Label)
%							185,I,J, LabelReference
IfStringLessEq	::= 186.	% if_string(a(I),'=<',a(J),Label)
%							186,I,J, LabelReference
IfStringGreaterEq  ::= 187.	% if_string(a(I),'>=',a(J),Label)
%							187,I,J, LabelReference
IfStringEqual	::= 188.	% if_string(a(I),'=',a(J),Label)
%							188,I,J, LabelReference
IfStringNotEqual  ::= 189.	% if_string(a(I),'=\=',a(J),Label)
%							189,I,J, LabelReference

IfTupleLess	::= 190.	% if_tuple(a(I),'<',a(J),Label)
%							190,I,J, LabelReference
IfTupleGreater	::= 191.	% if_tuple(a(I),'>',a(J),Label)
%							191,I,J, LabelReference
IfTupleLessEq	::= 192.	% if_tuple(a(I),'=<',a(J),Label)
%							192,I,J, LabelReference
IfTupleGreaterEq  ::= 193.	% if_tuple(a(I),'>=',a(J),Label)
%							193,I,J, LabelReference
IfTupleEqual	::= 194.	% if_tuple(a(I),'=',a(J),Label)
%							194,I,J, LabelReference
IfTupleNotEqual	::= 195.	% if_tuple(a(I),'=\=',a(J),Label)
%							195,I,J, LabelReference


% Branches

SwitchOnTag	::= 196.	% switch_on_tag(a(I), TagLabelList)
%					196,I,N,TagBits, LabelReferences

BranchInteger	::= 197.	% branch_integer(a(I),Low,High, Labels)
%					197, ILow, IHigh, LabelReferences
BranchReal	::= 198.	% branch_real(a(I),Low,High, Labels)
%					198, RLow, RHigh, LabelReferences
BranchTuple	::= 199.	% branch_tuple(a(I),Low,High, Labels)
%					199, TLow, THigh, LabelReferences
CaseHashInteger	::= 200.	% case_hash_integer(a(I), Labels)
						% 200,I,N, LabelReferences**N**
CaseHashString	::= 201.	% case_hash_string(a(I), Labels)
						% 201,I,N, LabelReferences

CompareIntLT	::= 308.	% compare_integer(IntArg1, '<', IntArg2, Label)
%					308,I,J, LabelReference
% we currently put integers in registers if they are not already there
CompareIntGT	::= 309.	% compare_integer(IntArg1, '>', IntArg2, Label)
%					309,I,J, LabelReference
CompareIntLE	::= 310.	% compare_integer(IntArg1, '=<', IntArg2, Label)
%					310,I,J, LabelReference
CompareIntGE	::= 311.	% compare_integer(IntArg1, '>=', IntArg2, Label)
%					311,I,J, LabelReference
CompareIntEQ	::= 312.	% compare_integer(IntArg1, '=', IntArg2, Label)
%					312,I,J, LabelReference
CompareIntNE	::= 313.	% compare_integer(IntArg1, '=', IntArg2, Label)
%					313,I,J, LabelReference
Unify		::= 314.	% unify(Asgns, Args, Label)
%					314,N,LabelRef
% N is the number of unifications; Asgns and Args are put pairwise with
% one of the following instructions as appropriate.

% Unify opcodes:

Unify_reg_reg		::=315.	% unify a(I), a(J)		315,I,J
Unify_reg_xreg		::=316.	% unify a(I), {a(J),IX}		316,I,J,IX
Unify_reg_axreg		::=317.	% unify a(I), &({a(J),IX})	317,I,J,IX
Unify_reg_roreg		::=318.	% unify a(I), ro(a(J))		318,I,J
Unify_reg_roaxreg	::=319.	% unify a(I), ro(&({a(J),IX}))	319,I,J,IX
Unify_reg_carreg	::=320.	% unify a(I), car(a(J))		320,I,J
Unify_reg_word		::=321.	% unify a(I), WordArgument	321,I,Word
Unify_reg_string	::=322.	% unify a(I), String		322,I,String
Unify_reg_real		::=323.	% unify a(I), Real		323,I,Real
Unify_reg_nil		::=324.	% unify a(I), []		324,I
Unify_xreg_reg		::=325.	% unify {a(I),IX}, a(J)		325,I,IX,J
Unify_xreg_xreg		::=326.	% unify {a(I),IX}, {a(J),JX}	326,I,IX,J,JX
Unify_xreg_axreg	::=327.	% unify {a(I),IX}, &({a(J),JX})	327,I,IX,J,JX
Unify_xreg_roreg	::=328.	% unify {a(I),IX}, ro(a(J))	328,I,IX,J
Unify_xreg_roaxreg	::=329.	% unify {a(I),IX}, ro(&({a(J),JX})) 329,I,IX,J,IX
Unify_xreg_carreg	::=330.	% unify {a(I),IX}, car(a(J))	330,I,IX,J
Unify_xreg_word		::=331.	% unify {a(I),IX}, WordArgument	331,I,IX,Word
Unify_xreg_string	::=332.	% unify {a(I),IX}, String	332,I,IX,String
Unify_xreg_real		::=333.	% unify {a(I),IX}, Real		333,I,IX,Real
Unify_xreg_nil		::=334.	% unify {a(I),IX}, []		334,I,IX

% Arithmetic Instructions

Decrement_2_reg		::=335.	% store a(I) - 1 in a(J)	335,I,J
Decrement_2_xreg	::=336.	% store a(I) - 1 in {a(J), IX}	336,I,J,IX
Decrement		::=337.	% decrement a(I)		337,I
Decrement_pointer	::=416.	% decrement addr in a(I)	416,I
Decrement_2_pointer	::=417. % store a(I) - wordsize in a(J) 417,I,J
Increment_2_reg		::=338.	% store a(I) + 1 in a(J)	338,I,J
Increment_2_xreg	::=339. % store a(I) + 1 in {a(J), IX}	339,I,J,IX
Increment		::=340.	% increment a(I)		340,I
Increment_pointer	::=418.	% increment addr in a(I)	418,I
Increment_2_pointer	::=419. % store a(I) + wordsize in a(J) 419,I,J

Increment_and_commit    ::=548. % store a(I)+1 in *a(J);commit  548,I,J
Decrement_and_commit    ::=549. % store a(I)-1 in *a(J);commit  549,I,J

Plus_reg_reg_reg	::=341.
Plus_reg_reg_xreg	::=342.
Plus_reg_int_reg	::=343.
Plus_reg_int_xreg	::=344.
Plus_int_reg_reg	::=345.
Plus_int_reg_xreg	::=346.
Plus_int_int_reg	::=347.
Plus_int_int_xreg	::=348.
Plusnum_reg_reg		::=349.
Plusnum_reg_int		::=350.
Plusnum_reg_real	::=351.
Plusnum_int_reg		::=352.
Plusnum_int_int		::=353.
Plusnum_int_real	::=354.
Plusnum_real_reg	::=355.
Plusnum_real_int	::=356.
Plusnum_real_real	::=357.
Diff_reg_reg_reg	::=358.
Diff_reg_reg_xreg	::=359.
Diff_reg_int_reg	::=360.
Diff_reg_int_xreg	::=361.
Diff_int_reg_reg	::=362.
Diff_int_reg_xreg	::=363.
Diff_int_int_reg	::=364.
Diff_int_int_xreg	::=365.
Diffnum_reg_reg		::=366.
Diffnum_reg_int		::=367.
Diffnum_reg_real	::=368.
Diffnum_int_reg		::=369.
Diffnum_int_int		::=370.
Diffnum_int_real	::=371.
Diffnum_real_reg	::=372.
Diffnum_real_int	::=373.
Diffnum_real_real	::=374.
Multnum_reg_reg		::=375.
Multnum_reg_int		::=376.
Multnum_reg_real	::=377.
Multnum_int_reg		::=378.
Multnum_int_int		::=379.
Multnum_int_real	::=380.
Multnum_real_reg	::=381.
Multnum_real_int	::=382.
Multnum_real_real	::=383.
Divnum_reg_reg		::=384.
Divnum_reg_int		::=385.
Divnum_reg_real		::=386.
Divnum_int_reg		::=387.
Divnum_int_int		::=388.
Divnum_int_real		::=389.
Divnum_real_reg		::=390.
Divnum_real_int		::=391.
Divnum_real_real	::=392.

Cmt_Plus_reg_reg_reg	::=464.
Cmt_Plus_reg_reg_xreg	::=465.
Cmt_Plus_reg_int_reg	::=466.
Cmt_Plus_reg_int_xreg	::=467.
Cmt_Plus_int_reg_reg	::=468.
Cmt_Plus_int_reg_xreg	::=469.
Cmt_Plus_int_int_reg	::=470.
Cmt_Plus_int_int_xreg	::=471.
Cmt_Plusnum_reg_reg	::=472.
Cmt_Plusnum_reg_int	::=473.
Cmt_Plusnum_reg_real	::=474.
Cmt_Plusnum_int_reg	::=475.
Cmt_Plusnum_int_int	::=476.
Cmt_Plusnum_int_real	::=477.
Cmt_Plusnum_real_reg	::=478.
Cmt_Plusnum_real_int	::=479.
Cmt_Plusnum_real_real	::=480.
Cmt_Diff_reg_reg_reg	::=481.
Cmt_Diff_reg_reg_xreg	::=482.
Cmt_Diff_reg_int_reg	::=483.
Cmt_Diff_reg_int_xreg	::=484.
Cmt_Diff_int_reg_reg	::=485.
Cmt_Diff_int_reg_xreg	::=486.
Cmt_Diff_int_int_reg	::=487.
Cmt_Diff_int_int_xreg	::=488.
Cmt_Diffnum_reg_reg	::=489.
Cmt_Diffnum_reg_int	::=490.
Cmt_Diffnum_reg_real	::=491.
Cmt_Diffnum_int_reg	::=492.
Cmt_Diffnum_int_int	::=493.
Cmt_Diffnum_int_real	::=494.
Cmt_Diffnum_real_reg	::=495.
Cmt_Diffnum_real_int	::=496.
Cmt_Diffnum_real_real	::=497.

% Processes

Enqueue		::= 202.	% enqueue(ProcedureId,a(I))
					% 202,I, ProcedureLabelReference

Iterate		::= 203.	% iterate
					% 203, LabelReference
Iterate1	::= 394.	% iterate(ProcedureId)
					% 394, ProcedureLabelReference
Execute		::= 204.	% execute(ProcedureId)
					% 204, ProceduralLabelReference,
%					%     IterativeLabelReference
Execute2	::= 393.	% execute(ProcedureId1, ProcedureId2)
					% 393, ProcedureLabelReference,
%					%      ProcedureLabelReference
Execute1	::= 408.	% execute(ProcedureId)
					% 408, ProceduralLabelReference

Halt		::= 205.	% halt			% 205
CondCommit	::= 206.	% commit(Label)		% 206, LabelReference
Commit		::= 207.	% commit		% 207
CommitNoLabel	::= 421.	% commit(Label)		% 421
SetCPArity	::= 208.	% set_cp_arity(N)	% 208,N
FailSuspend	::= 209.	% suspend(Fail, Suspend)
					 % 209, FailReference, SuspendReference
CondSuspend	::= 210.	% suspend(Fail)		% 210, FailReference
Suspend		::= 211.	% suspend		% 211
SuspendOn	::= 212.	% suspend_on(a(I))	% 212,I
SetHBT		::= 213.	% set_HBT		% 213
Undo		::= 214.	% undo			% 214
ListAssignCheck ::= 411.	% list_assign_with_check	411,I,J
ListAssign	::= 412.	% list_assign		412,I,J

/* Peephole Optimization Emulator Compound Commands */

				%    deref(a(I),a(I),a(K))
		%	=>
%				  deref_value(a(I),a(K))
%					iterate

				%    deref_value(a(I),a(K))
				%    if_integer(a(K),=\=,Integer,Label)
DerefInt	::= 215.	% deref_integer(a(I),a(K),Integer,Label)
					% 215,I,K, Integer, LabelReference

				%    deref_value(a(I),a(K))
				%    if_real(a(I),=\=,Real,Label)
DerefReal	::= 217.	% deref_real(a(I),a(K),Real,Label)
					% 217,I,K, Real, LabelReference

				%    deref_value(a(I),a(K))
				%    if_string(a(K),=\=,String,Label)
DerefString	::= 219.	% deref_string(a(I),a(K),String,Label)
					% 219,I,K, String, LabelReference

				%    deref_value(a(I),a(K))
				%    if_tag(a(K),=\=,nil,Label)
DerefNil	::= 221.	% deref_nil(a(I),a(K),Label)

				%    deref_value(a(I),a(K))
				%    if_tag(a(K),=\=,list,Label)
OldDerefList	::= 223.	% old_deref_list(a(I),a(K),Label)
					% 223,I,K, LabelReference

				%    deref_value(a(I),a(K))
				%    if_tuple(a(K),=\=,Arity,Label)
DerefTuple	::= 225.	% deref_tuple(a(I),a(K),Arity,Label)
					% 225,I,K, Arity, LabelReference

				%    deref_value(a(I),a(K))
				%    switch_on_tag(a(K),TagLabelList)
DerefOnTag	::= 227.	% deref_on_tag(a(I),a(K),Labels)
%					% 227,I,K,N, TagBits, LabelReferences

				%    deref_value(a(I),a(K))
				%    branch_integer(a(K),Low,High, Labels)
DerefBranchInteger  ::= 229. % deref_branch_integer(a(I),a(K),Low,High,Labels)
%					% 229,I,K, ILow, IHigh, LabelReferences

				%    deref_value(a(I),a(K))
				%    branch_tuple(a(K),Low,High, Labels)
DerefBranchTuple  ::= 233.	% deref_branch_tuple(a(I),a(K),Min,Max,Labels)
%					% 233,I,K, TLow, THigh, LabelReferences

				%    deref_value(a(I),a(K))
				%    case_hash_integer(a(K), Labels)
DerefHashInteger  ::= 235.	% deref_hash_integer(a(I),a(K),Labels)
%					% 235,I,K,N, LabelReferences

				%    deref_value(a(I),a(K))
				%    case_hash_string(a(K), Labels)
DerefHashString  ::= 237.	% deref_hash_string(a(I),a(K),Labels)
%					% 237,I,K,N, LabelReferences

				%    allocate_pr([A1...An],a(X))
				%    enqueue(PLabel,a(X))
		%	=>
AllocateEnqueue	::= 19.		% allocate_enqueue([A1...An],PLabel)
				    % 19,N,A1,...,An,ProcedureLabelReference

				%    allocate_pr([a(R1)...a(RN)],a(X))
				%    enqueue(PLabel,a(X))
		%	=>
AllocateRegsEnqueue ::= 37.	% allocate_regs_enqueue([a(R1)...a(RN)],PLabel)
				    % 37,N,R1,...,RN,ProcedureLabelReference


MultipleOperation  ::= 11.	% multiple_operation([T1...TN],[M1...MN])
%							11,N,OpCode, Pairs

ReferenceBit	   ::=  0.
PrivateWritableBit ::=  1.
SharedWritableBit  ::=  2.
PrivateReadOnlyBit ::=  3.
SharedReadOnlyBit  ::=  4.
IntegerTagBit	   ::=  5.
RealTagBit	   ::=  6.
StringTagBit	   ::=  7.
NilTagBit	   ::=  8.
ListRefTagBit	   ::=  9.
ListIntTagBit	   ::= 10.
ListNilTagBit	   ::= 11.
TupleTagBit	   ::= 12.
VectorTagBit	   ::= 13.

TagBit ::= ReferenceBit ; PrivateWritableBit ; SharedWritableBit ;
	   PrivateReadOnlyBit ; SharedReadOnlyBit ; IntegerTagBit ;
	   RealTagBit ; StringTagBit ; NilTagBit ; ListRefTagBit ;
	   ListIntTagBit ; ListNilTagBit ; TupleTagBit ; VectorTagBit .

SItems ::= [Item | Items].


CodedModule ::= [ModuleInfo | Contents] .
ModuleInfo ::= Tuple.
Contents ::= Strings ; [Item | Contents] .
Strings ::= [String] .
Item ::= Byte ; ItemTuple.

TwoTuple ::= {NullEscape, InstructionCounter} ;
	     {ProcedureStartEscape, ProcedureInfoOffset} ;
	     {IterativeReference, IterativeOffset} ;
	     {StringEscape, StringOffset} ;
	     {IntegerEscape, Integer} ;
	     {TupleEscape, Arity} ;
	     {RegisterEscape, RegisterNumber} ;
	     {ProcedureArgumentEscape, Integer} ;
	     {ModuleEscape, ModuleNameOffset} ;
	     {ProcedureInfoEscape, ProcedureNameOffset, Arity, Index} ;
	     {RegisterOffsetEscape, Integer} ;
	     {ProcedureReferenceEscape, ProcedureStartOffset} ;
	     {IterativeReferenceEscape, IterativePointOffset} ;
	     {RealEscape, Real} .

NullEscape ::= 0.
ProcedureStartEscape ::= 1.
IterativeReference ::= 2.
StringEscape ::= 3.
IntegerEscape ::= 4.
TupleEscape ::= 5.
RegisterEscape ::= 6.
ProcedureArgumentEscape ::= 7.
ModuleEscape ::= 8.
ProcedureInfoEscape ::= 9.
RegisterOffsetEscape ::= 10.
ProcedureReferenceEscape ::= 12.
IterativeReferenceEscape ::= 13.
RealEscape ::= 16.

InstructionCounter, ProcedureInfoOffset, IterativeOffset, StringOffset,
Arity, ModuleNameOffset, ProcedureNameOffset, Index, ProcedureStartOffset,
IterativePointOffset ::= Integer.

/* * * * * * * * * * * * * * * * * * C T L * * * * * * * * * * * * * * * * * */

procedure ctl(ModuleName(CTLs), Module).
procedure ctl(ModuleName(CTLs), Module, Contents, [Any]).

ctl(Input, Module) + (Contents = _, Display = _) :-
    Input = ModuleName(CTLs) :
      Contents ! Info |
	offsets # dictionary(Offsets, Strings, Display, ModuleName, Info, Ng),
	procedures # assemble(CTLs, Contents', Strings, Offsets),
	ground_module(Contents, Grounded),
	create_module(Ng, Grounded, Module).


procedure ground_module(CodedModule, CodedModule).
procedure ground_module(CodedModule, CodedModule, CodedModule).

ground_module(CM, Right) + (Left = CM) :-

    CM ? Item,
    ground(Item) |
	self;

    CM = [] :
      Left = Right .

procedure create_module(Ng, Grounded, Any).

create_module(Ng, Grounded, Module) :-

    Ng = [],
    known(Grounded) |
	processor # link(execute(ctl, make_ctl_module(Grounded, Module)), _) ;

    list(Ng) : Grounded = _,
      Module = [] |
	computation # display(stream, Ng,
			      [prefix("Can't assemble"), type(ground)]).

