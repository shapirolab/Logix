#include        <stdio.h>
#include        <stdlib.h>
#include        <math.h>
#include	"fcp.h"
#include	"codes.h"
#include	"global.h"
#include	"macros.h"
#include	"emulate.h"
#include	"opcodes.h"

/* Requests */

#define PSI_POST            1
#define PSI_CLOSE           2
#define PSI_STEP            3
#define PSI_INDEX           4

/* Sub-Channel Indices */

#define PSI_BLOCKED	    1	// TRUE iff non-empty queue and
				// no transmission is possible
#define PSI_CHANNEL_TYPE    2	// see below
#define PSI_CHANNEL_RATE    3	// Real
#define PSI_CHANNEL_REFS    4	// Reference counter
#define PSI_SEND_ANCHOR     5	// Head of SendQueue
#define PSI_DIMER_ANCHOR    5	// Head of DimerQueue
#define PSI_SEND_WEIGHT     6	// Sum of Send Multipliers
#define PSI_DIMER_WEIGHT    6	// Sum of Dimer Multipliers
#define PSI_RECEIVE_ANCHOR  7	// Head of ReceiveQueue
#define PSI_RECEIVE_WEIGHT  8	// Sum of Receive Multipliers
#define PSI_WEIGHT_TUPLE    9	// (constant) Weight computation parameters
#define PSI_NEXT_CHANNEL   10	// (circular) Channel list
#define PSI_PREV_CHANNEL   11	// (circular) Channel list
#define PSI_CHANNEL_NAME   12	// (constant) Created Channel name

#define CHANNEL_SIZE       12

/* Channel Types */

#define PSI_CHANNEL_ANCHOR  0
#define PSI_UNKNOWN         1
#define PSI_BIMOLECULAR     2
#define PSI_HOMODIMERIZED   3
#define PSI_INSTANTANEOUS   4
#define PSI_SINK            5

/* Weight Index Computation Values */

#define PSI_DEFAULT_WEIGHT_INDEX 0
#define PSI_DEFAULT_WEIGHT_NAME "default"

/* Message Types */

#define PSI_MESSAGE_ANCHOR  0
#define PSI_SEND            1
#define PSI_RECEIVE         2
#define PSI_DIMER           3

/* Listed Operation tuple (1-5), Queued message tuple (1-8) */

#define PSI_MS_TYPE         1		/* One of Message Types */
#define PSI_MS_CID          2
#define PSI_MS_CHANNEL      3
#define PSI_MS_MULTIPLIER   4		/* Positive Integer */
#define PSI_MS_TAGS         5
#define PSI_SEND_TAG        5
#define PSI_RECEIVE_TAG     6
#define PSI_MS_COMMON       7		/* {PId, MsList, Value^, Chosen^} */
#define PSI_MESSAGE_LINKS   8		/* (circular) stored fcp 2-vector */

#define PSI_MESSAGE_SIZE    8

#define PSI_NEXT_MS         1
#define PSI_PREVIOUS_MS     2

/* Operation request tuple (1-5), Transmission common tuple (1-4) */

#define PSI_OP_PID          1
#define PSI_OP_MSLIST       2
#define PSI_OP_VALUE        3
#define PSI_OP_CHOSEN       4
#define PSI_OP_REPLY        5

#define PSI_COMMON_SIZE     4

#define QUEUE               2
#define BLOCKED             3

/* Index Request */

#define PSI_WEIGHTER_NAME   1
#define PSI_WEIGHTER_INDEX  2

/* Internal named values */

#define TUPLE               1
#define STRING              2 
#define LIST                3
heapP cdr_down_list();

// ****************** Argument Verification Functions *************************

int psi_post(heapP PId,heapP OpList,heapP Value,heapP Chosen,heapP Reply);
int psi_close(heapP Channels,heapP Reply);
int psi_step(heapP Now,heapP Anchor,heapP NowP,heapP Reply);
int psi_index(heapP Name,heapP Index,heapP Reply);

//*************************** Post Functions **********************************

int set_offset(heapP OpEntry);
int set_final_type(heapP ChP,int MsType,heapP Reply);

int vctr_var_s(heapP Ch,int Size);
int which_mode (heapP P);
int which_channel_type(heapP ChP);
int set_new_weight(heapP ChP,heapP OpEntry,int Offset,int Positive);
int transmit_instantaneous(heapP OpEntry,heapP Channel,heapP PId,
				heapP Value,heapP Chosen,heapP Reply);
int make_common_tuple(heapP PId,heapP MessageList,heapP Value,heapP Chosen);
int make_message_tuple(heapP OpEntry,heapP ChP,heapP ComShTpl);
int insert_message_at_end_queue(heapP ChP,heapP Link,heapP Message,int Offset);
heapP add_mess_to_messtail(heapP Message,heapP MessageTail);
heapP set_opentry(heapP OpList);
int set_reply_to(char *Arg,heapP Reply);
int set_reply_trans(heapP PId1,heapP CId1,heapP PId2,heapP CId2,heapP Reply);

int psi_weight_index(char *name);
/*
double psi_compute_bimolecular_weight(int method,
				      double rate, int sends, int receives);
double psi_compute_homodimerized_weight(int method, double rate, int dimers);
*/
double psi_compute_bimolecular_weight(int method,
				      double rate, int sends, int receives,
				      int argn, double argv[]);
double psi_compute_homodimerized_weight(int method, double rate, int dimers,
				      int argn, double argv[]);


//************************* Step Functions ************************************

int transmit_biomolecular(heapP ChP,heapP Reply);
int transmit_homodimerized(heapP ChP,heapP Reply);
int more_than_one_ms(heapP ChP);
int get_sum_weight(heapP ChP, int Type, double *Result, heapP Reply);
int get_selector(heapP ChP, int Type, double *Result);
int set_nowp(heapP Now,heapP NowP,double SumWeights);
int set_blocked(heapP ChP,heapP Reply);


psicomm(Tpl)   //The Tuple is {Request , <Arguments> }
heapP Tpl;  
{
 heapP Request , Arg; 
  deref_ptr(Tpl);  //the input
  if (!IsTpl(*Tpl)) {
    return(False);
  }
  Request=(Tpl+1);
  deref_ptr(Request);
  if (!IsInt(*Request)){
    return(False);
  }
  Arg=(Tpl+2);
  
   switch(Int_Val(*Request))
    {
    case PSI_POST  :if (!psi_post(Arg,Arg+1,Arg+2,Arg+3,Arg+4)){
			return(False);
		    }
		    return(True);
    case PSI_CLOSE :if (!psi_close(Arg,Arg+1)){
			return(False);
                    }
                    return(True);
    case PSI_STEP  :if (!psi_step(Arg,Arg+1,Arg+2,Arg+3)){
			return(False);
		    }
                    return(True);
    case PSI_INDEX :if (!psi_index(Arg,Arg+1,Arg+2)){
			return(False);
		    }
                    return(True);
    default : return(False);
    }
}


//************************* Post Action ***************************************


psi_post( PId ,OpList ,Value ,Chosen ,Reply) 
 heapP  PId ,OpList ,Value ,Chosen ,Reply;
{
 int MsType, ChannelType ,TrIn ,Offset;
 heapT V0 ; 
 heapP Pa ,Pb ,OpEntry ,ChP ,Tag ,Mult ,
       MessageList ,MessageTail ,ComShTpl ,Link ,Message ;
 
 deref_ptr(PId);
 deref_ptr(OpList);
 deref_ptr(Value);
 deref_ptr(Chosen);
 deref_ptr(Reply);
 
 if (!IsList(*OpList)) {
  return(False);
  }
 
 if (IsNil(*OpList)){
   if (!unify(Ref_Word(Value),Word(0,NilTag))){
        return(False);
   }
   if (!unify(Ref_Word(Chosen),Word(0,IntTag))){
       return(False);
   }
   return(set_reply_to("true",Reply));
 }
 
 Pa=Pb=OpList;
 Pa=cdr_down_list(Pa); 
 if (!IsNil(*Pa)) { //if it's not end with a Nil
   return(False);
 }
 do			/* Pass 1  */
   { 
     if ((OpEntry=set_opentry(OpList))==False){
       return(False);
     }
     ChP=OpEntry+PSI_MS_CHANNEL;  // the channel 
     deref_ptr(ChP);
     if(!vctr_var_s(ChP, CHANNEL_SIZE)){
       return(False);
     }
     MsType=which_mode(OpEntry+PSI_MS_TYPE);
     ChannelType=which_channel_type(ChP);

     if (ChannelType == PSI_UNKNOWN) {
       if (!set_final_type(ChP,MsType,Reply)){ //should check reason?
	 return(False);
       } 
       ChannelType=which_channel_type(ChP);
     }
     switch (ChannelType) {
          case   PSI_BIMOLECULAR:
          case PSI_INSTANTANEOUS:
		  if (!(MsType==PSI_SEND||MsType==PSI_RECEIVE))
		    return
		      set_reply_to("Error - Wrong Message Type",Reply);
		  break;
          case PSI_HOMODIMERIZED:
                  if (!(MsType==PSI_DIMER))
		    return
		      set_reply_to("Error - Wrong Message Type",Reply);
		  break; 
          case    PSI_SINK: 
	          break;
          default:
                return(set_reply_to("Error - Wrong channel type",Reply));
		
     } /* End Switch */
   
   if (ChannelType==PSI_INSTANTANEOUS)
     {
       Tag=(OpEntry+PSI_MS_TAGS);
       deref_ptr(Tag);
       if(!IsInt(*Tag)){
         return(False);
       }
       TrIn=transmit_instantaneous(OpEntry, ChP, PId, Value, Chosen ,Reply);
       if (TrIn==True)
	 return(True);
       if (TrIn==False)
	 return(False);
     }
   Mult=(OpEntry+PSI_MS_MULTIPLIER);
   deref_ptr(Mult);
   if (!IsInt(*Mult)){
     return(False);
   }
   Tag=(OpEntry+PSI_MS_TAGS);
   deref_ptr(Tag);
   if (MsType==PSI_DIMER) 
     if (!IsTpl(*Tag)||(!(Arity_of(*Tag)==2))) {
	 return(False);
     }
   if ((MsType==PSI_SEND)||(MsType==PSI_RECEIVE))
     if (!IsInt(*Tag)){
       return(False);
     }
   
   OpList=Cdr(OpList);
   deref_ptr(OpList);
   }while(!IsNil(*OpList));  /* End Pass 1 */
  
 MessageTail=HP;
 *HP=Word(0,NilTag); //The start of the list
 *HP++;
 MessageList=HP;
 *MessageList=Word(0,WrtTag);
 *HP++;

 if (!make_common_tuple(PId, MessageList, Value, Chosen)){
   return(False);
 } 

 deref(KOutA,ComShTpl);
 OpList=Pb;

 while(!IsNil(*OpList))     /* Pass 2 */
   {
     OpEntry=set_opentry(OpList);
     ChP=OpEntry+PSI_MS_CHANNEL;  // the channel 
     deref_ptr(ChP);
     ChannelType=which_channel_type(ChP);
     if (ChannelType!=PSI_SINK)
       {
	 if (!do_store_vector(Word(PSI_BLOCKED,IntTag),Word(False,IntTag),
			      Ref_Word(ChP))){
	   return(False);
	 }
	 if (!make_message_tpl(OpEntry,ChP,ComShTpl)){
	   return(False);
	 }
	 deref(KOutA,Message);
	 deref(KOutB,Link);
	 Offset=set_offset(OpEntry);
	 if (!set_new_weight(ChP,OpEntry,Offset,1)){
	   return(False);
	 }	  
	 if (!insert_message_at_end_queue(ChP,Link,Message,Offset)){
	   return(False);
	 }  
	 MessageTail=add_mess_to_messtail(Message,MessageTail);
       }
     OpList=Cdr(OpList);
     deref_ptr(OpList);
   }         /* End while(Pass 2) */

 if (!unify(Ref_Word(MessageList),Ref_Word(MessageTail))){
     return(False);
 }
 return(set_reply_to("true",Reply));
 
} /* End psi_post */

//*************************** Post Functions **********************************

heapP set_opentry(OpEntry)
heapP OpEntry;
{
 heapT V0;

  deref_ptr(OpEntry);
  V0=*OpEntry;
  set_to_car(V0);
  deref(V0,OpEntry);
  if (!IsTpl(*OpEntry)) {
    return(False);
  }
  return(OpEntry);
}

//************************************************************************

vctr_var_s(Ch,Size)
heapP Ch;
int Size;
{
  heapT P;
  heapP T_Save;
  int Arity;

  T_Save=(Ch);
  deref_ptr(T_Save);
  P=*T_Save;
  if (!IsVctr(P)) {
        if(IsVar(P)) {
	    sus_tbl_add(T_Save);
      }
    return(False);
  }
  Arity = Arity_of(P);  
  if (!(Size==Arity)){
    return(False);
  }
  return(True);
}

//*********************************************************************

set_reply_trans(PId,OpCId,MaCoPId,MaCId,T)
heapP PId,OpCId,MaCoPId,MaCId,T;
{
  heapP P;
 
  deref_ptr(PId);
  deref_ptr(OpCId);
  deref_ptr(MaCoPId);
  deref_ptr(MaCId);
  
  if (!do_make_tuple(Word(5,IntTag))){
    return(False);
  }
  deref(KOutA,P);
  
  built_fcp_str("true");
      
  if (!unify(Ref_Word((++P)),KOutA)){
    return(False);
  }
  if (!unify(Ref_Word((++P)),Ref_Word(PId))){
    return(False);
  }
  if (!unify(Ref_Word((++P)),Ref_Word(OpCId))){
    return(False);
  }
  if (!unify(Ref_Word((++P)),Ref_Word(MaCoPId))){
    return(False);
  }
  if (!unify(Ref_Word((++P)),Ref_Word(MaCId))){
    return(False);
  }
  if (!unify(Ref_Word(T),Ref_Word(P-5))){
    return(False);  
  }
  return(True);
}

//********************************************************************

set_reply_to(Arg,Reply)
char *Arg;
heapP Reply;
{
  heapP Pa;
  built_fcp_str(Arg);
  if (!unify(Ref_Word(Reply),KOutA)){
    return(False);
  }
  return(True);

}

//*********************************************************************

built_fcp_str(Arg)
 char *Arg;
 {
      register char *PChar = (char *) (HP+2);
      register int StrLen = 0;

      KOutA = Ref_Word(HP);
      sprintf(PChar, "%s", Arg);
      while (*PChar++ != '\0') {
	StrLen++;
      }
      *HP++ = Str_Hdr1(CharType, 0);
      *HP = Str_Hdr2(hash((char *)(HP+1), StrLen), StrLen);
      HP += 1 + Str_Words((HP-1));
      while (PChar < (char *) HP) {
	*PChar++ = '\0';
      }
      return(True);
 }

//**********************************************************************

int which_mode (P)
 heapP P;
{
 heapT V0;
 int MsType;  
 
 deref_ptr(P);
 return(Int_Val(*P));
}// end which 

//*********************************************************************

which_channel_type(ChP)
heapP ChP;
{
  if (!do_read_vector(Word(PSI_CHANNEL_TYPE,IntTag),Ref_Word(ChP))){
    return(-1);
  }
  return(Int_Val(KOutA));
}

//*********************************************************************

transmit_instantaneous(OpEntry, Channel, PId, Value, Chosen ,Reply)
heapP OpEntry, Channel, PId, Value, Chosen ,Reply; 
{
 int MsType;  // Maybe it will pass to the function? 
 heapT Index ;
 heapP ChP ,MessageAnchor ,Ma ,Common ,CmVal ,CmChos ,NextMes ,MessAn ,Mess;

 deref_ptr(OpEntry);
 MsType=which_mode(OpEntry+PSI_MS_TYPE);
 ChP=OpEntry+PSI_MS_CHANNEL;
 deref_ptr(ChP);
 if (MsType==PSI_SEND)
   {
     if (!do_read_vector(Word(PSI_RECEIVE_ANCHOR,IntTag),Ref_Word(ChP))){
       return(False);
     }
     deref(KOutA,MessageAnchor);
     Index=PSI_RECEIVE_TAG;
   }
 else
   {
     if (!do_read_vector(Word(PSI_SEND_ANCHOR,IntTag),Ref_Word(ChP))){
       return(False);
     }
     deref(KOutA,MessageAnchor);
     Index=PSI_SEND_TAG;
   }
 MessAn=MessageAnchor; 
 MessAn=MessAn+PSI_MESSAGE_LINKS;
 deref_ptr(MessAn);
 if (!do_read_vector(Word(PSI_NEXT_MS,IntTag),Ref_Word(MessAn))){
   return(False);
 }
 deref(KOutA,NextMes);
 while (NextMes!=MessageAnchor)
   {
     Ma=NextMes;
     Common=Ma+PSI_MS_COMMON;
     deref_ptr(Common);
     if (!IsTpl(*Common)){
       return(False);
     }
     CmVal=Common+PSI_OP_VALUE;
     deref_ptr(CmVal);
     if (!IsWrt(*CmVal)){
      return(False);
     }
     CmChos=Common+PSI_OP_CHOSEN;
     deref_ptr(CmChos);
     if (!IsWrt(*CmChos)){
      return(False);
      }
     deref_ptr(Chosen);
     deref_ptr(CmChos);
     if ((Chosen)!=(CmChos))
       {
	 if (!unify(Ref_Word(CmChos),Ref_Word(Ma+Index))){
          return(False);
	 }
       	 if (!unify(Ref_Word(CmVal),Ref_Word(Value))){
          return(False);
	 }
       	 if (!unify(Ref_Word(Chosen),Ref_Word(OpEntry+PSI_MS_TAGS))) {
          return(False);
	 }
	 if (!discount(Common+PSI_OP_MSLIST)){
	   return(False);
	 }
	 set_reply_trans(PId,(OpEntry+PSI_MS_CID),(Common+PSI_OP_PID)
                  ,(Ma+PSI_MS_CID),Reply);
	 return(True);
       }
     Mess=NextMes+PSI_MESSAGE_LINKS;
     deref_ptr(Mess);
     if (!do_read_vector(Word(PSI_NEXT_MS,IntTag),Ref_Word(Mess))){
         return(False);
     }
     deref(KOutA,NextMes);
     if (NextMes==Ma)
       break;
   }
 return(QUEUE);
}

//*********************************************************************

discount(MsList)
heapP MsList;
{
  int Offset;
  heapT V0;
  heapP Mes,ChP,NextLinkPrevious,PreviousLinkNext,Previous,Next,NextLink
        ,PreviousLink ,Pa ,Link;

  deref_ptr(MsList);
  while(!IsNil(*MsList))
    { 
      V0=*MsList;
      set_to_car(V0);
      deref(V0,Mes);
      if (!IsTpl(*Mes)) {
	return(False);
      }
      ChP=Mes+PSI_MS_CHANNEL;  // the channel 
      deref_ptr(ChP);
      
      if (!do_read_vector(Word(PSI_CHANNEL_RATE,IntTag),Ref_Word(ChP))){
	return(False);
      } 
     deref(KOutA,Pa); 
     if (!IsReal(*Pa)||real_val(Pa+1)<0) {
     //set_reply_to("Error-Base Rate not a Positive Real Number",Reply);
       return(False);
     }
     Offset=set_offset(Mes);	
     if (!set_new_weight(ChP,Mes,Offset,0)){
       return(False);
     }
     Link=Mes+PSI_MESSAGE_LINKS;
     deref_ptr(Link); 
     if (!do_read_vector(Word(PSI_NEXT_MS,IntTag),Ref_Word(Link))){
       return(False);
     }
     deref(KOutA,Next); 
     if (!do_read_vector(Word(PSI_PREVIOUS_MS,IntTag),Ref_Word(Link))){
       return(False);
     }
     deref(KOutA,Previous);
     if (!IsTpl(*Next)||Arity_of(*Next)!=PSI_MESSAGE_SIZE) {
       return(False);
     } 
     if (!IsTpl(*Previous)||Arity_of(*Previous)!=PSI_MESSAGE_SIZE) {
       return(False);
     }
     NextLink=Next+PSI_MESSAGE_LINKS;
     deref_ptr(NextLink);
     PreviousLink=Previous+PSI_MESSAGE_LINKS;
     deref_ptr(PreviousLink);
     if (!do_read_vector(Word(PSI_NEXT_MS,IntTag),Ref_Word(PreviousLink))){
       return(False);
     }
     deref(KOutA,PreviousLinkNext); 
     if (!do_read_vector(Word(PSI_PREVIOUS_MS,IntTag),Ref_Word(NextLink))){
       return(False);
     }
     deref(KOutA,NextLinkPrevious);
     
     if (!IsTpl(*NextLinkPrevious)||
	 Arity_of(*NextLinkPrevious)!=PSI_MESSAGE_SIZE) {
       return(False);
     } 
     if (!IsTpl(*PreviousLinkNext)||
	 Arity_of(*PreviousLinkNext)!=PSI_MESSAGE_SIZE) {
       return(False);
     }
     
     if (!do_store_vector(Word(PSI_PREVIOUS_MS,IntTag),Ref_Word(Previous),
			  Ref_Word(NextLink))){
       return(False);
     }
     
     if (!do_store_vector(Word(PSI_NEXT_MS,IntTag),Ref_Word(Next),
			  Ref_Word(PreviousLink))){
       return(False);
     }
     
     MsList=Cdr(MsList);
     deref_ptr(MsList);
   }
  
}

//*********************************************************************

make_common_tuple(Pd,ML,Va,Ch)

 heapP Pd, ML, Va, Ch;
{ 
 heapT V0;
 heapP CST;

 if (!do_make_tuple(Word(PSI_COMMON_SIZE,IntTag))){
  return(False);
 }
 V0=KOutA;
 deref(KOutA,CST); 
 if (!unify(Ref_Word(++CST),Ref_Word(Pd))){
     return(False);
 }
 if(!unify(Ref_Word(++CST),Ref_Word(ML))){
   return(False);
 }
 if(!unify(Ref_Word(++CST),Ref_Word(Va))){
   return(False);
 } 
 if(!unify(Ref_Word(++CST),Ref_Word(Ch))){
   return(False);
 }
 KOutA=V0;
 return(True);
}
//*********************************************************************

make_message_tpl(OpEntry,ChP,ComShTpl)
heapP OpEntry,ChP,ComShTpl;
{
  int MsType;
  heapP Tag,Pa,Mess,Message,Link;
  heapT TagS,TagR,V0,V1;
  
  if (!do_make_tuple(Word(PSI_MESSAGE_SIZE,IntTag))){
    return(False);
  }
  V0=KOutA;
  deref(KOutA,Mess)
    Message=Mess;
  if (!unify(Ref_Word(++Mess),Ref_Word(OpEntry+PSI_MS_TYPE))){/*Meesage Type */
    return(False);
  }
  if (!unify(Ref_Word(++Mess),Ref_Word(OpEntry+PSI_MS_CID))){   /* CId */
    return(False);
  }
  if (!unify(Ref_Word(++Mess),Ref_Word(ChP))){  /* Channel */
    return(False);
  }
  if (!unify(Ref_Word(++Mess),Ref_Word(OpEntry+PSI_MS_MULTIPLIER))){	 
    return(False);				/* Multiplier */
  }
  Tag=(OpEntry+PSI_MS_TAGS);
  deref_ptr(Tag);
  MsType=which_mode(OpEntry+PSI_MS_TYPE);
  if (MsType==PSI_SEND)
    { 
      TagS=*Tag;
      TagR=Word(0,IntTag);
    }  
  else if(MsType==PSI_RECEIVE)
    { 
      TagS=Word(0,IntTag);
      TagR=*Tag;
    }  
  else if (MsType==PSI_DIMER)
    {
      Pa=++Tag; 
      deref_ptr(Pa);
      TagS=*Pa;
      Pa=++Tag; 
      deref_ptr(Pa);
      TagR=*Pa;
    }       
  if (!unify(Ref_Word(++Mess),TagS)){  /* SendTag */
    return(False); 
  }
  if (!unify(Ref_Word(++Mess),TagR)){ /* ReceiveTag */
    return(False);
  }
  if (!unify(Ref_Word(++Mess),Ref_Word(ComShTpl))){ /* shared Common */
    return(False);
  }
  if (!do_make_vector(Word(2,IntTag))){
    return(False);
  }
  V1=KOutA;
  deref(KOutA,Link);
  if(!do_store_vector(Word(PSI_NEXT_MS,IntTag),
		      Ref_Word(Message),Ref_Word(Link))){
    return(False);   /* Next Message */
      } 
  if(!do_store_vector(Word(PSI_PREVIOUS_MS,IntTag),
		      Ref_Word(Message),Ref_Word(Link))){
    return(False);   /* Previous Message */
  }
  if (!unify(Ref_Word(++Mess),Ref_Word(Link))){  /* The Link Vector */
    return(False);
  }
  KOutA=V0;
  KOutB=V1;
  return(True);
}

//*********************************************************************

insert_message_at_end_queue(ChP,Link,Message,Offset)
heapP ChP,Link,Message;
int Offset;
{            
 heapT V0;
 heapP Pa, LinkAnchor ,Anchor; 

 if (!do_read_vector(Word((PSI_SEND_ANCHOR+Offset),IntTag)
		     ,Ref_Word(ChP))){
   return(False);
 }
 deref(KOutA,Anchor); 
 if(!do_store_vector(Word(PSI_NEXT_MS,IntTag),
		      Ref_Word(Anchor),Ref_Word(Link))){
    return(False);
  } 
  LinkAnchor=(Anchor+PSI_MESSAGE_LINKS);
  deref_ptr(LinkAnchor);
  if (!do_read_vector(Word(PSI_PREVIOUS_MS,IntTag),
		      Ref_Word(LinkAnchor))){
    return(False);
  }
  V0=KOutA;
  deref(V0,Pa);
  Pa=Pa+PSI_MESSAGE_LINKS;
  deref_ptr(Pa);
  if(!do_store_vector(Word(PSI_PREVIOUS_MS,IntTag),KOutA,Ref_Word(Link))){
    return(False);
  }
  if (!do_store_vector(Word(PSI_NEXT_MS,IntTag),Ref_Word(Message)
		       ,Ref_Word(Pa))){
    return(False);
  }
    if (!do_store_vector(Word(PSI_PREVIOUS_MS,IntTag),
			 Ref_Word(Message),Ref_Word(LinkAnchor))){
      return(False);
    }
    return(True);
}

//*********************************************************************

heapP add_mess_to_messtail(Message,MessageTail)
heapP Message,MessageTail;
{       
 heapP Pa;    
     Pa=HP; 
     *HP=L_Ref_Word((HP+2));//the car point to the delete channel
     *HP++;
     *HP=Ref_Word((HP+2));
     *HP++;
     *HP++=Ref_Word(Message);
     *HP++=Ref_Word(MessageTail);
     return(Pa);
}

//*********************************************************************

set_final_type(ChP,MsType,Reply) //should check reason`
heapP ChP,Reply;
int MsType;             
{
  heapP Pa;
  int FinalType;

  if(!do_read_vector(Word(PSI_CHANNEL_RATE,IntTag),Ref_Word(ChP))){
    return(False);
  }
  deref(KOutA,Pa); 
  if (!IsReal(*Pa)||real_val(Pa+1)<0) {
    set_reply_to("Error - Base Rate not a Positive Real Number",Reply);
    return(True);
  }
  if (MsType==PSI_SEND||MsType==PSI_RECEIVE)
    FinalType=PSI_BIMOLECULAR;
  else
    if (MsType==PSI_DIMER)
      FinalType=PSI_HOMODIMERIZED;
  if (!do_store_vector(Word(PSI_CHANNEL_TYPE,IntTag),
			 Word(FinalType,IntTag),Ref_Word(ChP))){
    return(False);
  }
 return(True); 
}

//*********************************************************************

set_new_weight(ChP,OpEntry,Offset,Positive)
heapP ChP,OpEntry;
int Offset,Positive;
{   
 int MsType,SendWeight,NewVal;
 heapT V0,V1;
 heapP Anchor,Mult,Pa;

 if (!do_read_vector(Word((PSI_SEND_WEIGHT+Offset),IntTag)
		     ,Ref_Word(ChP))){
   return(False);
 }
 V0=KOutA;
 deref_val(V0);
 Mult=OpEntry+PSI_MS_MULTIPLIER;
 deref_ptr(Mult);
 V1=*Mult;
 if (Positive)
  NewVal=(Int_Val(V0)+Int_Val(V1));
 else 
  NewVal=(Int_Val(V0)-Int_Val(V1));
 if (!do_store_vector(Word((PSI_SEND_WEIGHT+Offset),IntTag),
		      Word(NewVal,IntTag),Ref_Word(ChP))){
   return(False);
 }
 return(True);
} 


//*********************************************************************

set_offset(OpEntry)
heapP OpEntry;
{
 int MsType;

 MsType=which_mode(OpEntry+PSI_MS_TYPE);
 if (MsType==PSI_SEND||MsType==PSI_DIMER)
   return(0);
 else
   return(2); 
}

//*************************** Close Action ************************************


int set_r_to(heapP L,heapP T);

int psi_close(ChT ,Reply)  
 heapP ChT ,Reply;  
{
 int RefCount ,Arity;
 heapP ChP ,ChPrev ,ChNext ,Lp ,Pt ,ChNextPrev ,ChPrevNext ,Pa;
 heapT V0;
 
 
 deref_ptr(ChT);
 if (!IsTpl(*ChT)) {
    return(False);
 }
 
 *HP=Word(0,NilTag); //The start of the list
 Lp=HP;
 *HP++;
  
 deref_ptr(Reply);
 
 Pa=STP;

 Arity=Arity_of(*ChT);
 while ((Arity--)>0) //going over all the channels
   { 
    ChP=++ChT;
    deref_ptr(ChP);

    if (!vctr_var_s(ChP,CHANNEL_SIZE)){
       return(False);
     }
   
    if (!do_read_vector(Word(PSI_CHANNEL_REFS,IntTag),Ref_Word(ChP))){
       return(False);
      }

    V0=KOutA;
    deref_val(V0);
    RefCount=Int_Val(V0);
    
    if (!IsInt(V0)||(RefCount <= 0)){ 
      return(set_reply_to("Error - Problem In ReferenceCount",Reply));
    }

    if (!do_read_vector(Word(PSI_PREV_CHANNEL,IntTag),Ref_Word(ChP))){
       return(False);
     }

    deref(KOutA,ChPrev);

    if (!vctr_var_s(ChPrev,CHANNEL_SIZE)){
      return(set_reply_to("Error - PSI_PREV_CHANNEL - not a Channel",Reply));
    }
    
    if (!do_read_vector(Word(PSI_NEXT_CHANNEL,IntTag),Ref_Word(ChP))){
      return(False);
    }
    
    deref(KOutA,ChNext);
    
    if (!vctr_var_s(ChNext,CHANNEL_SIZE)){
      return(set_reply_to("Error - PSI_NEXT_CHANNEL - not a Channel",Reply));
    }
    RefCount--;
    if (!do_store_vector(Word(PSI_CHANNEL_REFS,IntTag),
			 Word(RefCount,IntTag),
			 Ref_Word(ChP))){
      return(False);
    }
    if (RefCount==0)
      {
       if (!do_read_vector(Word(PSI_NEXT_CHANNEL,IntTag),Ref_Word(ChPrev))){
	   return(False);
          }
         deref(KOutA,ChPrevNext)
	 if (!vctr_var_s(ChPrevNext,CHANNEL_SIZE)){
	   return(set_reply_to("Error - PSI_PREV_NEXT_CHANNEL - not a Channel",
			       Reply));
	 }
	 if (!do_read_vector(Word(PSI_PREV_CHANNEL,IntTag),Ref_Word(ChNext))){
	   return(False);
	 }
	 deref(KOutA,ChNextPrev);
	 if (!vctr_var_s(ChNextPrev,CHANNEL_SIZE)){
	   return(set_reply_to("Error - PSI_NEXT_PREV_CHANNEL - not a Channel",
			       Reply));
	 }
	 if (!do_store_vector(Word(PSI_PREV_CHANNEL,IntTag),
			      Ref_Word(ChPrev),
			      Ref_Word(ChNext))){
	   return(False);
	 }
	 if (!do_store_vector(Word(PSI_NEXT_CHANNEL,IntTag),
			      Ref_Word(ChNext),
			      Ref_Word(ChPrev))){
	   return(False);
	 }
	 if (!do_store_vector(Word(PSI_PREV_CHANNEL,IntTag),
			      Ref_Word(ChP),
			      Ref_Word(ChP))){
	   return(False);
	 }
	 if (!do_store_vector(Word(PSI_NEXT_CHANNEL,IntTag),
			      Ref_Word(ChP),
			      Ref_Word(ChP))){
	   return(False);
	 }
	 if (heap_space(sizeof(HP)) < 4) {
	   err_tbl_add(MACHINE, ErHPSPACE);
	   return(False);	
	 }
	 Pt=HP; 
	 *HP=L_Ref_Word((HP+2));//the car points to the deleted channel
	 *HP++;
	 *HP=Ref_Word((HP+2));
	 *HP++;
	 *HP++=Ref_Word(ChP);
	 *HP++=Ref_Word(Lp);
	 Lp=Pt;
	 
      } /* End If RefCount==0 */
  
   }  /* End While */
 
 if (Pa!=STP) {
   return(False); 
 }

 if (!set_r_to(Lp,Reply)){
  return(False);
 }
 return(True);

} /* End psi_close */

//*********************************************************************

set_r_to(L,T)
heapP L,T;
{
  heapT V0;
  heapP P;
 
  if (!do_make_tuple(Word(2,IntTag))){
    return(False);
   }
  V0=KOutA;
  deref(V0,P);
  
  built_fcp_str("true");
  
  if (!unify(Ref_Word((++P)),KOutA)){
    return(False);
   }	 
  if (!unify(Ref_Word((++P)),Ref_Word(L))){
    return(False);
  }

  if (!unify(Ref_Word(T),Ref_Word(P-2))){
   return(False);  
  }
 return(True);
}

//******************************** Step Action ********************************

int psi_step(Now ,Anchor ,NowP ,Reply ) 
               //the tuple contain 4 arguments -{Now ,Anchor , Now' ,Reply'}
heapP Now ,Anchor ,NowP ,Reply ;
{
 int  i,ChannelType,SendWeight ,ReceiveWeight ,DimerWeight ,TranS=False ,Ret;
 double SumWeights=0,Selector,NowVal,BaseRate,j=0,Val ; 
 heapP NextChannel ,ChP ,Pa ;
 heapT V0;

 deref_ptr(Now);		/* Assumed Real */
 deref_ptr(Anchor);		/* Assumed Vector */
 deref_ptr(NowP);		/* Assumed Var */
 deref_ptr(Reply);		/* Assumed Var */

 if (!do_read_vector(Word(PSI_NEXT_CHANNEL,IntTag),Ref_Word(Anchor))){
   return(False);
 }
 deref(KOutA,NextChannel);
 if (NextChannel!=Anchor) {
   do {
     ChP=NextChannel;
     if (!do_read_vector(Word(PSI_BLOCKED,IntTag),Ref_Word(ChP))){
       return(False);	
     }
     deref_val(KOutA); 
     if (!Int_Val(KOutA)) {                /* not blocked */
       double Result = 0.0;

       ChannelType=which_channel_type(ChP);
       switch(ChannelType)
	 {     
	 case PSI_BIMOLECULAR :
	   if (!get_sum_weight(ChP, PSI_BIMOLECULAR, &Result, Reply))
	     return(False);
	   if (!IsVar(*Reply))
	     return(True);
	   SumWeights += Result;
	   break;
	 case PSI_HOMODIMERIZED:
	   if (more_than_one_ms(ChP)) {  
	     if (!get_sum_weight(ChP, PSI_HOMODIMERIZED, &Result, Reply))
	       return(False);
	     if (!IsVar(*Reply))
	       return(True);
	     SumWeights += Result;
	   }
	   break;
	 case    PSI_UNKNOWN: 
	 case    PSI_SINK: 
	   break;
	 default: 
	   return(set_reply_to("Wrong Channel Type",Reply));  
	 }                  /* End Switch */ 
     }
     if (!do_read_vector(Word(PSI_NEXT_CHANNEL,IntTag),Ref_Word(ChP))){
       return(False);
     }
     deref(KOutA,NextChannel);
   } while(NextChannel!=Anchor);   /* End While - Pass 1 */

   if  (SumWeights != 0) {
     j=((random())/2147483647.0);
     Selector =j*SumWeights;

     if (!do_read_vector(Word(PSI_NEXT_CHANNEL,IntTag),Ref_Word(Anchor))){
       return(False);
     }
     deref(KOutA,NextChannel);

     do {                      /* Pass 2 */
       ChP=NextChannel;
       if (!do_read_vector(Word(PSI_BLOCKED,IntTag),Ref_Word(ChP)))
	 return(False);	
       deref_val(KOutA);
       if (!Int_Val(KOutA)) {                /* not blocked */

	 double Result = 0.0;

	 ChannelType=which_channel_type(ChP);
	 switch(ChannelType)
	   {     
	   case PSI_BIMOLECULAR :	 
	     if (!get_selector(ChP, PSI_BIMOLECULAR, &Result))
	       return(False);
	     Selector -= Result;
	     if (Selector <= 0) {
	       Ret=transmit_biomolecular(ChP,Reply);
	       TranS=True;
	     }
	     break;
	   case PSI_HOMODIMERIZED:
	     if (more_than_one_ms(ChP)) {
	       if (!get_selector(ChP, PSI_HOMODIMERIZED, &Result))
		 return(False);
	       Selector -= Result;
	       if (Selector <= 0) {
		 Ret=transmit_homodimerized(ChP,Reply);
		 TranS=True;
	       }
	     }               /* End if more_than_one */
	   }            /* End Switch  */
       }          /* End if Not Blocked */

       if (!do_read_vector(Word(PSI_NEXT_CHANNEL,IntTag),Ref_Word(ChP))){
	 return(False);
       }
       deref(KOutA,NextChannel);
     } while((NextChannel!=Anchor) && (!TranS));      /* End While  Pass 2 */

     if (TranS==True) {
       if (Ret==True) { 
	 if (!set_nowp(Now,NowP,SumWeights)){
	   return(False);
	 } 
	 return(True);
       }
       else
	 if (Ret==BLOCKED) {
	   if (!unify(Ref_Word(NowP),Ref_Word(Now))){
	     return(False);
	   }
	   return(set_blocked(ChP,Reply));
	 }
       return(False);//Ret==False
     }
   } /* End if (SumWeight != 0)*/
 } /* End if(NextChannel!=Anchor) */  

 if (!unify(Ref_Word(NowP),Ref_Word(Now))){
   return(False);
 }
 return(set_reply_to("true",Reply));
}    /* End psi_step function */ 
 

//************************** Step Functions ***********************************

int more_than_one_ms(ChP)
heapP ChP;
 {
   heapP MesLink,NextMes,PrevMes,MesAnchor;

   if (!do_read_vector(Word(PSI_DIMER_ANCHOR,IntTag),Ref_Word(ChP))){
     return(False);
   }
   deref(KOutA,MesAnchor);
   MesLink=MesAnchor+PSI_MESSAGE_LINKS;
   deref_ptr(MesLink);
   if (!do_read_vector(Word(PSI_NEXT_MS,IntTag),Ref_Word(MesLink))){
     return(False);
   }  
   deref(KOutA,NextMes);
return(MesAnchor!=NextMes); 
}
               
//********************************************************************

int get_sum_weight(heapP ChP, int Type, double *Result, heapP Reply)
{
  double BaseRate;
  int SendWeight,ReceiveWeight,DimerWeight,WeightIndex;
  heapP Pa;
  int argn;
  double argv[100];
  
  if (!do_read_vector(Word(PSI_CHANNEL_RATE,IntTag),Ref_Word(ChP))){
    return(False);
  }
  deref(KOutA,Pa);
  if(!IsReal(*Pa)||(BaseRate=real_val(Pa+1))<=0) {
    set_reply_to("Error - Base Rate not a Positive Real Number",Reply);
    return(False);
  }
  if (!do_read_vector(Word(PSI_SEND_WEIGHT,IntTag),Ref_Word(ChP))){
    return(False);
  }
  deref_val(KOutA);
  if (!IsInt(KOutA)||(SendWeight=Int_Val(KOutA))<0) {
    set_reply_to("Error - Send Weight not a Positive Integer",Reply);
    return(False);
  }
  if (!do_read_vector(Word(PSI_WEIGHT_TUPLE,IntTag),Ref_Word(ChP))) {
    return(False);
  }
  if (IsRef(KOutA)) {
    deref_ref(KOutA, Pa);
    if (IsTpl(KOutA)) {
      heapP Pb;
      int Arity = Arity_of(KOutA);
      if (Arity == 1)
	return(False);
      Pb = Pa += 2;
      deref_ptr(Pb);
      KOutA = *Pb;
      for (argn = 0; argn < Arity-2; argn++) {
	Pb = ++Pa;
	deref_ptr(Pb);
	if (IsReal(*Pb))
	  argv[argn] = real_val(Pb+1);
	else if (IsInt(*Pb))
	  argv[argn] = Int_Val(*Pb);
	else
	  return False;
      }
    }
    else
      argn = 0;
  }
  if (!IsInt(KOutA) || (WeightIndex=Int_Val(KOutA)) < 0)
    return set_reply_to("Error - Invalid Weight Index", Reply);
  if (SendWeight > 0)
    switch (Type)
      {
      case PSI_BIMOLECULAR:
	if (!do_read_vector(Word(PSI_RECEIVE_WEIGHT,IntTag),Ref_Word(ChP)))
	  return(False);
	deref_val(KOutA);
	if (!IsInt(KOutA) || (ReceiveWeight = Int_Val(KOutA))<0)
	  return set_reply_to("Error - Receive Weight not a Positive Integer",
			      Reply);
	if (WeightIndex != PSI_DEFAULT_WEIGHT_INDEX) {
	  if (ReceiveWeight > 0)
	    *Result = psi_compute_bimolecular_weight(WeightIndex, BaseRate,
						     SendWeight, ReceiveWeight,
                                                     argn, argv);
	  else
	    *Result = 0.0;
	}
	else
	  *Result = BaseRate*SendWeight*ReceiveWeight;
	return True;
      case PSI_HOMODIMERIZED:
	DimerWeight=SendWeight;         
	if (WeightIndex != PSI_DEFAULT_WEIGHT_INDEX)
	  *Result = psi_compute_homodimerized_weight(WeightIndex, BaseRate,
						     DimerWeight,
                                                     argn, argv);
	else
	  *Result = (BaseRate*DimerWeight*(DimerWeight-1))/2; 
	return True;
      }
  *Result = 0;
  return True;
}

//********************************************************************

int get_selector(heapP ChP, int Type, double *Result)
{  
  int SendWeight,ReceiveWeight,DimerWeight,WeightIndex;
  double BaseRate; 
  heapP Pa;
  int argn;
  double argv[100];

  if (!do_read_vector(Word(PSI_CHANNEL_RATE,IntTag),Ref_Word(ChP)))
    return(False);
  deref(KOutA,Pa);
  BaseRate=real_val(Pa+1);
  if (!do_read_vector(Word(PSI_SEND_WEIGHT,IntTag),Ref_Word(ChP)))
    return(False);
  deref_val(KOutA);
  SendWeight=Int_Val(KOutA); 
  if (SendWeight > 0) {
    if (!do_read_vector(Word(PSI_WEIGHT_TUPLE,IntTag),Ref_Word(ChP)))
      return(False);
    if (IsRef(KOutA)) {
      deref_ref(KOutA, Pa);
      if (IsTpl(KOutA)) {
	heapP Pb;
	int ix = 0;
	int Arity = Arity_of(KOutA);
	if (Arity == 1)
	  return(False);
	Pb = Pa += 2;
	deref_ptr(Pb);
	KOutA = *Pb;
	for (argn = 0; argn < Arity-2; argn++) {
	  Pb = ++Pa;
	  deref_ptr(Pb);
	  if (IsReal(*Pb))
	    argv[argn] = real_val(Pb+1);
	  else if (IsInt(*Pb))
	    argv[argn] = Int_Val(*Pb);
	  else
	    return False;
	}
      }
      else
	argn = 0;
    }
    WeightIndex = Int_Val(KOutA);

    switch (Type)
      {
      case PSI_BIMOLECULAR: 
	if (!do_read_vector(Word(PSI_RECEIVE_WEIGHT,IntTag),Ref_Word(ChP)))
	  return(False);
	deref_val(KOutA);
	ReceiveWeight = Int_Val(KOutA);
	if (ReceiveWeight > 0) {
	  if (WeightIndex != PSI_DEFAULT_WEIGHT_INDEX)
	    *Result = psi_compute_bimolecular_weight(WeightIndex, BaseRate,
						     SendWeight, ReceiveWeight,
                                                     argn, argv);
	  else
	    *Result = BaseRate*SendWeight*ReceiveWeight; 
	}
	break;
      case PSI_HOMODIMERIZED:
	DimerWeight = SendWeight;
	if (WeightIndex != PSI_DEFAULT_WEIGHT_INDEX)
	  *Result = psi_compute_homodimerized_weight(WeightIndex, BaseRate,
						     DimerWeight,
						     argn, argv);
	else
	  *Result = (BaseRate*DimerWeight*(DimerWeight-1))/2.0;
      }
  }
  return True;
}


//********************************************************************

int set_nowp(heapP Now, heapP NowP, double SumWeights)
{
  double NowVal, j;
  heapT V0;

  NowVal=real_val(Now+1); 
  j=((random())/2147483647.0);
  NowVal-=log(j)/SumWeights;
  V0 = Ref_Word(HP);
  *HP++ = Word(0, RealTag);
  real_copy_val(((realT) NowVal), HP);
  HP += realTbits/heapTbits;
  if (!unify(Ref_Word(NowP),V0)){
    return(False);
  }
  return(True);
}

//********************************************************************

set_blocked(ChP,Reply)
heapP ChP,Reply; 
{
  if (!do_store_vector(Word(PSI_BLOCKED,IntTag),
		       Word(True,IntTag),
		       Ref_Word(ChP))){
    return(False);
  }
  return(set_reply_to("done",Reply));
}

//********************************************************************

transmit_biomolecular(ChP,Reply)
heapP ChP,Reply;
{
 heapP ReceiveMessage,RMess,RCommon,RComValue,RComChos,RMesAnchor,SMesAnchor,
       SendMessage,SMess,SCommon,SComValue,SComChos,SMsList,RMsList,Pa;
 
 if (!do_read_vector(Word(PSI_RECEIVE_ANCHOR,IntTag),Ref_Word(ChP))){
  return(False);
 }
 deref(KOutA,ReceiveMessage);
 if(!IsTpl(*ReceiveMessage)){
   return(False);
 }
 Pa=ReceiveMessage+PSI_MS_TYPE;
 deref_ptr(Pa);
 if ((Int_Val(*Pa)!=PSI_MESSAGE_ANCHOR)){
   return(False);
 }
 RMesAnchor=ReceiveMessage;
 if (!do_read_vector(Word(PSI_NEXT_MS,IntTag),
		     Ref_Word(ReceiveMessage+PSI_MESSAGE_LINKS))){
   return(False);
 }
 deref(KOutA,ReceiveMessage);
 while (ReceiveMessage!=RMesAnchor) 
   {
     RMess=ReceiveMessage;
     RCommon=RMess+PSI_MS_COMMON;
     deref_ptr(RCommon);
     if (!IsTpl(*RCommon)){
       return(False);
     }
     RComValue=RCommon+PSI_OP_VALUE;
     deref_ptr(RComValue);
     if (!IsWrt(*RComValue)){
       return(False);
     }
     RComChos=RCommon+PSI_OP_CHOSEN;
     deref_ptr(RComChos);
     if (!IsWrt(*RComChos)){
       return(False);
     }
     if (!do_read_vector(Word(PSI_SEND_ANCHOR,IntTag),Ref_Word(ChP))){
       return(False);
     }
     deref(KOutA,SendMessage);
     Pa=SendMessage+PSI_MS_TYPE;
     deref_ptr(Pa);
     if ((Int_Val(*Pa)!=PSI_MESSAGE_ANCHOR)){
       return(False);
     }
     SMesAnchor=SendMessage;
     if (!do_read_vector(Word(PSI_NEXT_MS,IntTag),
			 Ref_Word(SendMessage+PSI_MESSAGE_LINKS))){
       return(False);
     }
     deref(KOutA,SendMessage);
     while (SendMessage!=SMesAnchor) 
       {	       
	 SMess=SendMessage;
	 SCommon=SMess+PSI_MS_COMMON;
	 deref_ptr(SCommon);
	 if (!IsTpl(*SCommon)){
	   return(False);
	 }
	 SComValue=SCommon+PSI_OP_VALUE;
	 deref_ptr(SComValue);
	 if (!IsWrt(*SComValue)){
	   return(False);
	 }
	 SComChos=SCommon+PSI_OP_CHOSEN;
	 deref_ptr(SComChos);
	 if (!IsWrt(*SComChos)){
	   return(False);
	 }
	 if (RComChos!=SComChos)
	   {
	     RMsList=RCommon+PSI_OP_MSLIST;
	     deref_ptr(RMsList);
	     if (!IsList(*RMsList)){
	       return(False);
	     }
	     SMsList=SCommon+PSI_OP_MSLIST;
	     deref_ptr(SMsList);
	     if (!IsList(*SMsList)){
	       return(False);
	     }
	     if (!unify(Ref_Word(SComChos),Ref_Word(SMess+PSI_SEND_TAG))){
	       return(False);
	     } 
	     if (!unify(Ref_Word(RComChos),Ref_Word(RMess+PSI_RECEIVE_TAG))){
	       return(False);
	     }
	     if (!unify(Ref_Word(RComValue),Ref_Word(SComValue))){
	       return(False);
	     }
	     if (!discount(SMsList)){
	       return(False);
	     }
	     if (!discount(RMsList)){
	       return(False);
	     }
	     return(set_reply_trans(SCommon+PSI_OP_PID,SMess+PSI_MS_CID,
				    RCommon+PSI_OP_PID,
				    RMess+PSI_MS_CID,Reply));
	   }
	 if (!do_read_vector(Word(PSI_NEXT_MS,IntTag),
			     Ref_Word(SendMessage+PSI_MESSAGE_LINKS))){
	   return(False);
	 }
	 deref(KOutA,SendMessage);
       }     
     if (!do_read_vector(Word(PSI_NEXT_MS,IntTag),
			 Ref_Word(ReceiveMessage+PSI_MESSAGE_LINKS))){
       return(False);
     }
     deref(KOutA,ReceiveMessage);
   }
 return (BLOCKED);
}  

//********************************************************************

transmit_homodimerized (ChP,Reply)
heapP ChP,Reply;
{
  heapP SendAnchor,ReceiveMessage,RMess,RCommon,RComValue,RComChos,
       DimerMessage,DMess,DCommon,DComValue,DComChos,RMsList,DMsList,Dm;
 
  if (!do_read_vector(Word(PSI_SEND_ANCHOR,IntTag),Ref_Word(ChP))){
    return(False);
  }
  deref(KOutA,SendAnchor);
  if (!do_read_vector(Word(PSI_NEXT_MS,IntTag),
		      Ref_Word(SendAnchor+PSI_MESSAGE_LINKS))){
    return(False);
  } 
  deref(KOutA,ReceiveMessage);
  RMess=ReceiveMessage;
  RCommon=ReceiveMessage+PSI_MS_COMMON;
  deref_ptr(RCommon);
  if (!IsTpl(*RCommon)){
    return(False);
  }
  RComValue=RCommon+PSI_OP_VALUE;
  deref_ptr(RComValue);
  if (!IsWrt(*RComValue)){
    return(False);
  }
  RComChos=RCommon+PSI_OP_CHOSEN;
  deref_ptr(RComChos);
  if (!IsWrt(*RComChos)){
    return(False);
  }
  if (!do_read_vector(Word(PSI_NEXT_MS,IntTag),
		      Ref_Word(ReceiveMessage+PSI_MESSAGE_LINKS))){
    return(False);
  }  
  deref(KOutA,DimerMessage);
  Dm=DimerMessage+PSI_MS_TYPE;
  deref_ptr(Dm);
  while(Int_Val(*Dm)!=PSI_MESSAGE_ANCHOR)
    {
      DMess=DimerMessage;
      DCommon=DMess+PSI_MS_COMMON;
      deref_ptr(DCommon);
      if (!IsTpl(*DCommon)){
	return(False);
      }
      DComValue=DCommon+PSI_OP_VALUE;
      deref_ptr(DComValue);
      if (!IsWrt(*DComValue)){
	return(False);
      }
      DComChos=DCommon+PSI_OP_CHOSEN;
      deref_ptr(DComChos);
      if (!IsWrt(*DComChos)){
	return(False);
      }
      if (DComChos!=RComChos)
	{
	  RMsList=RCommon+PSI_OP_MSLIST;
	  deref_ptr(RMsList);
	  if (!IsList(*RMsList)){
	    return(False);
	  }
	  DMsList=DCommon+PSI_OP_MSLIST;
	  deref_ptr(DMsList);
	  if (!IsList(*DMsList)){
	    return(False);
	  }
	  if (!unify(Ref_Word(RComChos),Ref_Word(RMess+PSI_RECEIVE_TAG))){
	    return(False);
	  } 
	  if (!unify(Ref_Word(DComChos),Ref_Word(DMess+PSI_SEND_TAG))){
	    return(False);
	  }
	  if (!unify(Ref_Word(DComValue),Ref_Word(RComValue))){  
	    return(False);
	  }
	  if (!discount(RMsList)){
	    return(False);
	  }
	  if (!discount(DMsList)){
	    return(False);
	  }
	  return(set_reply_trans(DCommon+PSI_OP_PID,DMess+PSI_MS_CID,
				 RCommon+PSI_OP_PID,RMess+PSI_MS_CID,Reply));
	}
      if (!do_read_vector(Word(PSI_NEXT_MS,IntTag),
			  Ref_Word(DMess+PSI_MESSAGE_LINKS))){
	return(False);
      }  
      deref(KOutA,DimerMessage);
      Dm=DimerMessage+PSI_MS_TYPE;
      deref_ptr(Dm);
    }
  return(BLOCKED);
}

//*************************** Index Action ************************************

int psi_index(Name, Index, Reply)
heapP Name, Index, Reply;
{
  char *NameString; 
  double result;

  deref_ptr(Name);
  if (!IsStr(*Name) || Str_Type(Name) != CharType) {
    return set_reply_to("Error - weighter name not string", Reply);
  }
  deref_ptr(Index);
  if (!IsVar(*Index)) {
    return set_reply_to("Error - weighter index not writable", Reply);
  }
  result = psi_weight_index((char *)(Name+2));
  if (!unify(Ref_Word(Index),Word(result,IntTag))){
    return(False);
  }
  return set_reply_to("true", Reply);
}

