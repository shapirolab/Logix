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

/* Sub-Channel Indices */

#define PSI_BLOCKED	    1		// TRUE iff non-empty queue and
                                        // no transmission is possible
#define PSI_CHANNEL_TYPE    2		// see below
#define PSI_CHANNEL_RATE    3		// Real
#define PSI_CHANNEL_REFS    4		// Reference counter
#define PSI_SEND_ANCHOR     5		// Head of SendQueue
#define PSI_DIMER_ANCHOR    5		// Head of DimerQueue
#define PSI_SEND_WEIGHT     6		// Sum of Send Multipliers
#define PSI_DIMER_WEIGHT    6		// Sum of Dimer Multipliers
#define PSI_RECEIVE_ANCHOR  7		// Head of ReceiveQueue
#define PSI_RECEIVE_WEIGHT  8	        // Sum of Receive Multipliers
#define PSI_NEXT_CHANNEL    9		// (circular) channel list
#define PSI_PREV_CHANNEL   10		// (circular) channel list
#define PSI_CHANNEL_NAME   11		// (constant) Created Channel name

#define CHANNEL_SIZE       11

/* Channel Types */

#define PSI_CHANNEL_ANCHOR  0
#define PSI_UNKNOWN         1
#define PSI_BIMOLECULAR     2
#define PSI_HOMODIMERIZED   3
#define PSI_INSTANTANEOUS   4
#define PSI_SINK            5

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

#define TUPLE               1
#define STRING              2 
#define LIST                3
heapP cdr_down_list();

int psi_post(heapP PId,heapP OpList,heapP Value,heapP Chosen,heapP Reply);
int psi_close(heapP Channels,heapP Reply);
int psi_step(heapP Now,heapP Anchor,heapP NowP,heapP Reply);
//***********************Psi_Post**Functions********************
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
int set_reply_to(int Type,char *Arg,heapP Reply);
int set_reply_trans(heapP PId1,heapP CId1,heapP PId2,heapP CId2,heapP Reply);

//*********************Psi_Step**Functions***********************************
int transmit_biomolecular(heapP ChP,heapP Reply);
int transmit_homodimerized(heapP ChP,heapP Reply);
int not_equale(heapP ChP);
float get_sum_weight(int Type,heapP ChP,heapP Reply);
float get_selector(heapP ChP,int Type);
int set_nowp(heapP Now,heapP NowP,float SumWeights);
int set_blocked(heapP ChP,heapP Reply);


psicomm(Tpl)   //The Tuple is {Request , <Areguments> }
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
    default : return(False);
    }
}


//*******************Post**Action*****************************************


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
   if (!set_reply_to(STRING,"true",Reply)){
      return(False);
   }   
 return(True);
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
     switch (ChannelType) {
          case PSI_UNKNOWN:
	    if (!set_final_type(ChP,MsType,Reply)){ //should check reason?
               return(False);
            } 
	    break;
          case   PSI_BIMOLECULAR:
          case PSI_INSTANTANEOUS:
		  if (!(MsType==PSI_SEND||MsType==PSI_RECEIVE)){
		    set_reply_to(TUPLE,"Error-Wrong Message Type",Reply);
		    return(True);
		  }
		  break;
          case PSI_HOMODIMERIZED:
                  if (!(MsType==PSI_DIMER)){
		    set_reply_to(TUPLE,"Error - Wrong Message Type",Reply);
		    return(True); 
		  }
		  break; 
          case    PSI_SINK: 
	          break;
         defult:
                set_reply_to(TUPLE,"Error- Wrong channel type",Reply);
                return(True);
		
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
 *HP=(0,NilTag); //The start of the list
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
 if (!set_reply_to(STRING,"true",Reply)){
   return(False);
 }
 return(True);
 
} /* End psi_post */

//*****************Functions*****Psi_Post*********************************

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

set_reply_to(Type,Arg,Reply)
int Type;
char *Arg;
heapP Reply;
{
  heapP Pa;

 switch (Type)
 {
  case TUPLE: 
    if (!do_make_tuple(Word(1,IntTag))){
      return(False);
    }
    deref(KOutA,Pa);
    built_fcp_str(Arg);
    if (!unify(Ref_Word((++Pa)),KOutA)){
      return(False);
    }
    if (!unify(Ref_Word(Reply),Ref_Word(Pa-1))){
      return(False);  
    }
    return(True);
  case STRING :
   built_fcp_str(Arg);
   if (!unify(Ref_Word(Reply),KOutA)){
     return(False);
   }
   return(True);
   //case LIST :
 }
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
       	 if (!unify(Ref_Word(Chosen),Ref_Word(OpEntry+5))){
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
       //set_reply_to(TUPLE,"Error-Base Rate Not A Positive RealNumber",Reply);
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
  LinkAnchor=(Anchor+8);
  deref_ptr(LinkAnchor);
  if (!do_read_vector(Word(PSI_PREVIOUS_MS,IntTag),
		      Ref_Word(LinkAnchor))){
    return(False);
  }
  V0=KOutA;
  deref(V0,Pa);
  Pa=Pa+8;
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
    set_reply_to(TUPLE,"Error - Base Rate Not A Positive RealNumber",Reply);
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

//*******************Close*****Action*****************************************


int set_r_to(heapP L,heapP T);

psi_close(ChT ,Reply)  
 heapP ChT ,Reply;  
{
 int RefCount ,Arity;
 heapP ChP ,ChPrev ,ChNext ,Lp ,Pt ,ChNextPrev ,ChPrevNext ,Pa;
 heapT V0;
 
 
 deref_ptr(ChT);
 if (!IsTpl(*ChT)) {
    return(False);
 }
 
 *HP=(0,NilTag); //The start of the list
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
      if (!set_reply_to(TUPLE,"Error - Problem In ReferenceCount",Reply)){
        return(False);
      }
      return(True);
     }

    if (!do_read_vector(Word(PSI_PREV_CHANNEL,IntTag),Ref_Word(ChP))){
       return(False);
     }

    deref(KOutA,ChPrev);

    if (!vctr_var_s(ChPrev,CHANNEL_SIZE)){
      if (!set_reply_to(TUPLE,"Error - PSI_PREV_CHANNEL - Not An 11-Vector",Reply)){
	return(False);
      }
      return(True);
    }
    
    if (!do_read_vector(Word(PSI_NEXT_CHANNEL,IntTag),Ref_Word(ChP))){
      return(False);
    }
    
    deref(KOutA,ChNext);
    
    if (!vctr_var_s(ChNext,CHANNEL_SIZE)){
      if (!set_reply_to(TUPLE,"Error - PSI_NEXT_CHANNEL - Not An 11-Vector",Reply)){
	return(False);
      }
      return(True);
    }
    RefCount--;
    if (!do_store_vector(Word(PSI_CHANNEL_REFS,IntTag),Word(RefCount,IntTag),Ref_Word(ChP))){
      return(False);
    }
    if (RefCount==0)
      {
       if (!do_read_vector(Word(PSI_NEXT_CHANNEL,IntTag),Ref_Word(ChPrev))){
	   return(False);
          }
         deref(KOutA,ChPrevNext)
	 if (!vctr_var_s(ChPrevNext,CHANNEL_SIZE)){
	   if (!set_reply_to(TUPLE,"Error - PSI_PREV_NEXT_CHANNEL - Not An 11-Vector",Reply)){
	     return(False);
	   }
	   return(True);
	 }
	 if (!do_read_vector(Word(PSI_PREV_CHANNEL,IntTag),Ref_Word(ChNext))){
	   return(False);
	 }
	 deref(KOutA,ChNextPrev);
	 if (!vctr_var_s(ChNextPrev,CHANNEL_SIZE)){
	   if (!set_reply_to(TUPLE,"Error - PSI_NEXT_PREV_CHANNEL - Not An 11-Vector",Reply)){
	     return(False);
	   }
	   return(True);
	 }
	 if (!do_store_vector(Word(PSI_PREV_CHANNEL,IntTag),Ref_Word(ChPrev),Ref_Word(ChNext))){
	   return(False);
	 }
	 if (!do_store_vector(Word(PSI_NEXT_CHANNEL,IntTag),Ref_Word(ChNext),Ref_Word(ChPrev))){
	   return(False);
	 }
	 if (!do_store_vector(Word(PSI_PREV_CHANNEL,IntTag),Ref_Word(ChP),Ref_Word(ChP))){
	   return(False);
	 }
	 if (!do_store_vector(Word(PSI_NEXT_CHANNEL,IntTag),Ref_Word(ChP),Ref_Word(ChP))){
	   return(False);
	 }
	 if (heap_space(sizeof(HP)) < 4) {
	   err_tbl_add(MACHINE, ErHPSPACE);
	   return(False);	
	 }
	 Pt=HP; 
	 *HP=L_Ref_Word((HP+2));//the car point to the delete channel
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


//*********************************Psi_Step*****************************

psi_step(Now ,Anchor ,NowP ,Reply ) 
               //the tuple contain 4 arguments -{Now ,Anchor , Now' ,Reply'}
heapP Now ,Anchor ,NowP ,Reply ;
{
 int  i,ChannelType,SendWeight ,ReceiveWeight ,DimerWeight ,TranS=0 ,Ret;
 float SumWeights=0,Selector,NowVal,BaseRate,j=0,Val,Set ; 
 heapP NextChannel ,ChP ,Pa ;
 heapT V0;

 deref_ptr(Now);
 deref_ptr(Anchor);
 deref_ptr(NowP);
 deref_ptr(Reply);

 if (!do_read_vector(Word(PSI_NEXT_CHANNEL,IntTag),Ref_Word(Anchor))){
   return(False);
 }
 deref(KOutA,NextChannel);
 if (NextChannel!=Anchor)
 {
  do
  {
    ChP=NextChannel;
    if (!do_read_vector(Word(PSI_BLOCKED,IntTag),Ref_Word(ChP))){
      return(False);	
    }
    deref_val(KOutA); 
    if (!Int_Val(KOutA))                 /* not blocked */
    {
      ChannelType=which_channel_type(ChP);
      switch(ChannelType)
      {     
       case PSI_BIMOLECULAR :
	 if ((Set=get_sum_weight(PSI_BIMOLECULAR,ChP,Reply))<0){
	   return(False);
	 }
	 SumWeights+=Set;
	 break;
       case PSI_HOMODIMERIZED:
	 if (not_equale(ChP))
         {  
	   if ((Set=get_sum_weight(PSI_HOMODIMERIZED,ChP,Reply))<0){
	     return(False);
	   }
	   SumWeights += Set;
	   break;
	 }
	 break;
       defult: set_reply_to(TUPLE,"Worng Channel Type",Reply);
	 return(False);  
      }                  /* End Switch */ 
    }
    if (!do_read_vector(Word(PSI_NEXT_CHANNEL,IntTag),Ref_Word(ChP))){
      return(False);
    }
    deref(KOutA,NextChannel);
  } while(NextChannel!=Anchor);   /* End While - Pass 1 */
  
  if  (SumWeights != 0)
  {
    j=((random())/2147483647.0);
    Selector =j*SumWeights;

    if (!do_read_vector(Word(PSI_NEXT_CHANNEL,IntTag),Ref_Word(Anchor))){
      return(False);
    }
    deref(KOutA,NextChannel);
    
    do                       /* Pass 2 */
    {
      ChP=NextChannel;
      if (!do_read_vector(Word(PSI_BLOCKED,IntTag),Ref_Word(ChP))){
	return(False);	
      }
      deref_val(KOutA);
      if (!Int_Val(KOutA))
      {
	ChannelType=which_channel_type(ChP);
	switch(ChannelType)
	{     
	 case PSI_BIMOLECULAR :	 
	   if ((Set=get_selector(ChP,PSI_BIMOLECULAR))<0){
	     return(False);
	   }        
	   Selector -=Set;
	   if (Selector<=0)
	   {
	     Ret=transmit_biomolecular(ChP,Reply);
	     TranS=1;
	   }
	   break;
	 case PSI_HOMODIMERIZED:
	   if (not_equale(ChP))
	   {  
	     if ((Set=get_selector(ChP,PSI_HOMODIMERIZED))<0){
	       return(False);
	     }        
	     Selector -=Set;
	     if (Selector<=0)
	     {
	       Ret=transmit_homodimerized(ChP,Reply);
	       TranS=1;
	     }
	   }               /* End if not_equal */
	}            /* End Switch  */
      }          /* End if Not Blocked */

      if (!do_read_vector(Word(PSI_NEXT_CHANNEL,IntTag),Ref_Word(ChP))){
	return(False);
      }
      deref(KOutA,NextChannel);
    }while((NextChannel!=Anchor)&&(!TranS));      /* End While  Pass 2 */

  }       /* End if (SumWeight != 0)*/
    
  if (TranS==True)
  {
    if (Ret==True)
    { 
      if (!set_nowp(Now,NowP,SumWeights)){
	return(False);
      }
      return(True);
    }
    else
      if (Ret==BLOCKED)
      {
	if (!set_blocked(ChP,Reply)){
	  return(False);
	}
	return(True);
      }
    return(False);//Ret==False
  }

 }         /* End if(NextChannel!=Anchor) */  

 set_reply_to(STRING,"true",Reply);
 return(True);
}    /* End psi_step function */ 
 

//****************************FUNCTION**Psi_Step******************************

int not_equale(ChP)
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

float get_sum_weight(Type,ChP,Reply)
int Type;
heapP ChP,Reply;
{
  float BaseRate;
  int SendWeight,ReceiveWeight,DimerWeight;
  heapP Pa;
  
  if (!do_read_vector(Word(PSI_CHANNEL_RATE,IntTag),Ref_Word(ChP))){
    return(-1);
  }
  deref(KOutA,Pa);
  if(!IsReal(*Pa)||(BaseRate=real_val(Pa+1))<=0) {
    set_reply_to(TUPLE,"Error - Base Rate Not A Positive RealNumber",Reply);
    return(-1);
  }
  if (!do_read_vector(Word(PSI_SEND_WEIGHT,IntTag),Ref_Word(ChP))){
    return(-1);
  }
  deref_val(KOutA);
  if (!IsInt(KOutA)||(SendWeight=Int_Val(KOutA))<0) {
    set_reply_to(TUPLE,"Error - Send Weight Not A Positive Iteger",Reply);
    return(-1);
  }
  switch (Type)
    {
     case PSI_BIMOLECULAR:
       if (!do_read_vector(Word(PSI_RECEIVE_WEIGHT,IntTag),Ref_Word(ChP))){
	 return(False);
       }
       deref_val(KOutA);
       if (!IsInt(KOutA)||(ReceiveWeight=Int_Val(KOutA))<0) {
	 set_reply_to(TUPLE,"Error - Receive Weight Not A Positive Integer",Reply);
	 return(-1);
       }
       return(BaseRate*SendWeight*ReceiveWeight); 
    case PSI_HOMODIMERIZED:
       DimerWeight=SendWeight;         
       return(BaseRate*((DimerWeight-1)/2)*DimerWeight); 
   }
}

//********************************************************************

float get_selector(ChP,Type)
heapP ChP;
int Type;
{  
  int SendWeight,ReceiveWeight,DimerWeight;
  float BaseRate; 
  heapP Pa;

  if (!do_read_vector(Word(PSI_CHANNEL_RATE,IntTag),Ref_Word(ChP))){
    return(False);
  }
  deref(KOutA,Pa);
  BaseRate=real_val(Pa+1);
  if (!do_read_vector(Word(PSI_SEND_WEIGHT,IntTag),Ref_Word(ChP))){
    return(-1);
  }
  deref_val(KOutA);
  SendWeight=Int_Val(KOutA); 
  if (SendWeight>0)
    {
      switch (Type)
	{
	case PSI_BIMOLECULAR: 
	  if (!do_read_vector(Word(PSI_RECEIVE_WEIGHT,IntTag),Ref_Word(ChP))){
	    return(-1);
	  }
	  deref_val(KOutA);
	  ReceiveWeight=Int_Val(KOutA);
	  if (ReceiveWeight>0)
	    return(BaseRate*SendWeight*ReceiveWeight);
          break;
	case PSI_HOMODIMERIZED:
	  DimerWeight=SendWeight;
	  return(BaseRate*(DimerWeight-1)*DimerWeight/2);		 
	}
    }
  return(0);
}

//********************************************************************

set_nowp(Now,NowP,SumWeights)
heapP Now,NowP;
float SumWeights;
{
  float NowVal, j;
  heapT V0;

  NowVal=real_val(Now+1); 
  /*if(!IsReal(*Now)) {
    set_reply_to(TUPLE, "Error - Base Rate Not A Positive RealNumber",Reply);
    return(False);
    }*/
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
  if (!do_store_vector(Word(PSI_BLOCKED,IntTag),Word(True,IntTag),Ref_Word(ChP))){
    return(False);
  }
  if (!set_reply_to(STRING,"done",Reply)){
    return(False);
  }
  return(True);
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
 if (!do_read_vector(Word(PSI_NEXT_MS,IntTag),Ref_Word(ReceiveMessage+PSI_MESSAGE_LINKS))){
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
     if (!do_read_vector(Word(PSI_NEXT_MS,IntTag),Ref_Word(SendMessage+PSI_MESSAGE_LINKS))){
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
	     if(!set_reply_trans(SCommon+PSI_OP_PID,SMess+PSI_MS_CID,
				 RCommon+PSI_OP_PID,RMess+PSI_MS_CID,Reply)){
	       return(False);
	     }
	     return(True);
	   }
	 if (!do_read_vector(Word(PSI_NEXT_MS,IntTag),Ref_Word(SendMessage+PSI_MESSAGE_LINKS))){
	   return(False);
	 }
	 deref(KOutA,SendMessage);
       }     
     if (!do_read_vector(Word(PSI_NEXT_MS,IntTag),Ref_Word(ReceiveMessage+PSI_MESSAGE_LINKS))){
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
  if (!do_read_vector(Word(PSI_NEXT_MS,IntTag),Ref_Word(SendAnchor+PSI_MESSAGE_LINKS))){
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
  if (!do_read_vector(Word(PSI_NEXT_MS,IntTag),Ref_Word(ReceiveMessage+PSI_MESSAGE_LINKS))){
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
	  if (!set_reply_trans(DCommon+PSI_OP_PID,DMess+PSI_MS_CID,
			       RCommon+PSI_OP_PID,RMess+PSI_MS_CID,Reply)){
	    return(False);
	  }
	  return(True);
	}
      if (!do_read_vector(Word(PSI_NEXT_MS,IntTag),Ref_Word(DMess+PSI_MESSAGE_LINKS))){
	return(False);
      }  
      deref(KOutA,DimerMessage);
      Dm=DimerMessage+PSI_MS_TYPE;
      deref_ptr(Dm);
    }
  return(BLOCKED);
}