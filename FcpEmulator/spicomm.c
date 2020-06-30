/*
** This module is part of EFCP.
**

     Copyright 2007 Yossi Goldberg, William Silverman
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

#include        <stdio.h>
#include        <stdlib.h>
#include        <math.h>
#include	"fcp.h"
#include	"codes.h"
#include	"global.h"
#include	"macros.h"
#include	"emulate.h"
#include	"opcodes.h"
#include	"spicomm.h"

dump_channel(heapP ChP) {
  
  fprintf(stderr, "dumping channel: ");
  if (!do_read_vector(Word(SPI_CHANNEL_NAME,IntTag),Ref_Word(ChP)))
    fprintf(stderr, "?\n");
  else {
    heapP name;
    deref(KOutA, name);
    if (IsStr(*name))
      fprintf(stderr, "%s\n", (char *) (name+2));
    else {
      if (IsTpl(*name))
	fprintf(stderr, " a tuple\n");
    }
  }
}

// ****************** Argument Verification Functions *************************

int spi_post(heapP PId,heapP OpList,heapP Value,heapP Chosen,heapP Reply);
int spi_close(heapP Channels,heapP Reply);
int spi_step(heapP Now,heapP Anchor,heapP NowP,heapP Reply);
int spi_index(heapP Name,heapP Index,heapP Reply);
int spi_rate(heapP Channels,heapP Weight,heapP Reply);

//*************************** Post Functions **********************************

int set_offset(heapP OpEntry);
int update_channel_type(heapP ChP, int MsType, int *ChannelType, heapP Reply);

int vctr_var_s(heapP Ch,int Size);
int which_mode (heapP P);
int channel_type(heapP ChP, int *ChannelType);
int positive_integer(heapT V);
int set_new_weight(heapP ChP,heapP OpEntry,int Offset,int Positive);
int transmit_instantaneous(heapP OpEntry, heapP Channel, heapP PId, int ChType,
			   heapP Value, heapP Chosen, heapP Reply);
int choose_random_start(heapP MessageAnchor, double select, heapP *Start);
int make_common_tuple(heapP PId,heapP MessageList,heapP Value,heapP Chosen);
int make_message_tuple(heapP OpEntry,heapP ChP,heapP ComShTpl);
int insert_message_at_end_queue(heapP ChP,heapP Link,heapP Message,int Offset);
heapP add_mess_to_messtail(heapP Message,heapP MessageTail);
heapP set_opentry(heapP OpList);
int set_reply_to(char *Arg,heapP Reply);
int set_reply_trans(heapP SendPId, heapP SendCId,heapP SendCh,
		    heapP ReceivePId, heapP ReceiveCId, heapP ReceiveCh,
		    heapP Reply);

int spi_weight_index(char *name);
double spi_compute_bimolecular_weight(int method,
				      double rate, int sends, int receives,
				      int argn, double argv[]);
double spi_compute_homodimerized_weight(int method, double rate, int dimers,
				      int argn, double argv[]);

//************************* Step Functions ************************************

int transmit_biomolecular(heapP ChP, int Random,
			  double Uniform1, double Uniform2, heapP Reply);
int transmit_homodimerized(heapP ChP, int Random,
			   double Uniform1, double Uniform2, heapP Reply);
int more_than_one_ms(heapP ChP);
int get_sum_weight(heapP ChP, int Type, double *Result, heapP Reply);
int get_selector(heapP ChP, int Type, double *Result);
int set_nowp(heapP Now,heapP NowP, double Uniform, double SumWeights);
int set_blocked(heapP ChP,heapP Reply);


spicomm(Tpl)   //The Tuple is {Request , <Arguments> }
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
    case SPI_POST  :if (!spi_post(Arg,Arg+1,Arg+2,Arg+3,Arg+4)){
			return(False);
		    }
		    return(True);
    case SPI_CLOSE :if (!spi_close(Arg,Arg+1)){
			return(False);
                    }
                    return(True);
    case SPI_STEP  :if (!spi_step(Arg,Arg+1,Arg+2,Arg+3)){
			return(False);
		    }
                    return(True);
    case SPI_INDEX :if (!spi_index(Arg,Arg+1,Arg+2)){
			return(False);
		    }
                    return(True);
    case SPI_RATE  :if (!spi_rate(Arg,Arg+1,Arg+2)){
			return(False);
		    }
                    return(True);
    default : return(False);
    }
}


//************************* Post Action ***************************************


spi_post( PId ,OpList ,Value ,Chosen ,Reply) 
 heapP  PId ,OpList ,Value ,Chosen ,Reply;
{
 int MsType, ChannelType ,TrIn ,Offset;
 heapT V0 ; 
 heapP Pa ,Pb ,OpEntry ,ChP ,Tag ,Mult ,
       MessageList ,MessageTail ,ComShTpl ,Link ,Message ;
 int IC, InstCount = 0;
 
 deref_ptr(PId);
 deref_ptr(OpList);
 deref_ptr(Value);
 deref_ptr(Chosen);
 deref_ptr(Reply);

 if (IsNil(*OpList)){
   if (!unify(Ref_Word(Value),Word(0,NilTag))){
     /*fprintf(stderr,"post 0\n");*/
     return(False);
   }
   if (!unify(Ref_Word(Chosen),Word(0,IntTag))){
     /*fprintf(stderr,"post 1\n");*/
     return(False);
   }
   return(set_reply_to("true",Reply));
 }

 if (!IsList(*OpList)) {
   /*fprintf(stderr,"post 2\n");*/
   return(False);
 }
  
 Pa=Pb=OpList;
 Pa=cdr_down_list(Pa); 
 if (!IsNil(*Pa)) { //if it's not end with a Nil
   /*fprintf(stderr,"post 3\n");*/
   return(False);
 }
 do			/* Pass 0  */
   { 
     if ((OpEntry=set_opentry(OpList))==False){
       /*fprintf(stderr,"post 4\n");*/
       return(False);
     }
     ChP=OpEntry+SPI_MS_CHANNEL;  // the channel 
     deref_ptr(ChP);
     if(!vctr_var_s(ChP, CHANNEL_SIZE)){
       /*fprintf(stderr,"post 5\n");*/
       return(False);
     }
     MsType=which_mode(OpEntry+SPI_MS_TYPE);
     if (!channel_type(ChP, &ChannelType)){
       /*fprintf(stderr,"post 6\n");*/
       return(False);
     }
     if ((ChannelType & SPI_TYPE_MASK) == SPI_UNKNOWN) {
       if (!update_channel_type(ChP, MsType, &ChannelType, Reply)){
	 /*fprintf(stderr,"post 7\n");*/
	 return(False);
       } 
     }
     switch (ChannelType & SPI_TYPE_MASK) {
          case SPI_BIMOLECULAR:
          case SPI_INSTANTANEOUS:
		  if (!(MsType==SPI_SEND||MsType==SPI_RECEIVE))
		    return
		      set_reply_to("Error - Wrong Message Type",Reply);
		  break;
          case SPI_HOMODIMERIZED:
                  if (!(MsType==SPI_DIMER))
		    return
		      set_reply_to("Error - Wrong Message Type",Reply);
		  break;
	  case SPI_DELAY:
		  if (!(MsType==SPI_RECEIVE))
		    return
		      set_reply_to("Error - Wrong Message Type",Reply);
		  break;
          case SPI_SINK: 
	          break;
          default:
                return(set_reply_to("Error - Wrong channel type",Reply));
		
     } /* End Switch */

   if (!positive_integer(*(OpEntry+SPI_MS_MULTIPLIER))) {
     /*fprintf(stderr,"post 8\n");*/
     return(False);
   }
   
   Tag = (OpEntry+SPI_MS_TAGS);
   deref_ptr(Tag);

   if (MsType==SPI_DIMER) {
     if (!IsTpl(*Tag)||(!(Arity_of(*Tag)==2))) {
       /*fprintf(stderr,"post 9\n");*/
       return(False);
     }
   }
   else {
     if ((MsType!=SPI_SEND) && (MsType!=SPI_RECEIVE)) {
       /*fprintf(stderr,"post 10\n");*/
       return(False);
     }
     if(!IsInt(*Tag)) {
       /*fprintf(stderr,"post 11\n");*/
       return(False);
     }
     if ((ChannelType & SPI_TYPE_MASK) == SPI_INSTANTANEOUS) {
       /*
       TrIn = transmit_instantaneous(OpEntry, ChP, PId,
				     ChannelType, Value, Chosen ,Reply);
       if (TrIn != QUEUE)
	 return(TrIn);
       */
       InstCount++;
     }
   }

   OpList=Cdr(OpList);
   deref_ptr(OpList);
   }while(!IsNil(*OpList));  /* End Pass 0 */

 if (InstCount) {
   heapP *InstList = NULL;
   int IC = 0;

   OpList = Pb;
   InstList = malloc(sizeof(heapP));
   do			/* Pass 1 */
     {
       OpEntry = set_opentry(OpList);
       ChP=OpEntry+SPI_MS_CHANNEL;  // the channel
       deref_ptr(ChP);
       MsType=which_mode(OpEntry+SPI_MS_TYPE);
       channel_type(ChP, &ChannelType);
       if ((ChannelType & SPI_TYPE_MASK) == SPI_INSTANTANEOUS) {
	 InstList[IC] = OpEntry;
	 IC++;
       }
       OpList=Cdr(OpList);
       deref_ptr(OpList);
     } while(IC < InstCount && !IsNil(*OpList));  /* End Pass 1 */
   /* Kluge - just walk through them */
   do
     {
       if (InstCount > 1) {
	 IC = (random()/2147483648.0)*InstCount;
	 if (IC == InstCount)
	   IC--;
       }
       else
	 IC = 0;
       OpEntry = InstList[IC];
       ChP=OpEntry+SPI_MS_CHANNEL;  // the channel
       deref_ptr(ChP);
       TrIn = transmit_instantaneous(OpEntry, ChP, PId,
				     SPI_INSTANTANEOUS, Value, Chosen ,Reply);
       if (TrIn != QUEUE) {
	 free(InstList);
	 return(TrIn);
       }
       /* Couldn't transmit - remove this entry from the list. */
       InstCount--;
       while (IC < InstCount) {
	 InstList[IC] = InstList[IC+1];
	 IC++;
       }
     } while (InstCount > 0);
   free(InstList);
 }  
  
 MessageTail=HP;
 *HP=Word(0,NilTag); //The start of the list
 *HP++;
 MessageList=HP;
 *MessageList=Word(0,WrtTag);
 *HP++;

 if (!make_common_tuple(PId, MessageList, Value, Chosen)){
   /*fprintf(stderr,"post 12\n");*/
   return(False);
 } 
 deref(KOutA,ComShTpl);
 OpList=Pb;

 while(!IsNil(*OpList))     /* Pass 2 */
   {
     OpEntry=set_opentry(OpList);
     ChP=OpEntry+SPI_MS_CHANNEL;  // the channel 
     deref_ptr(ChP);
     channel_type(ChP, &ChannelType);
     if (ChannelType != SPI_SINK)
       {
	 if (!do_store_vector(Word(SPI_BLOCKED,IntTag),Word(False,IntTag),
			      Ref_Word(ChP))){
	   /*fprintf(stderr,"post 13\n");*/
	   return(False);
	 }
	 if (!make_message_tuple(OpEntry,ChP,ComShTpl)){
	   /*fprintf(stderr,"post 14\n");*/
	   return(False);
	 }
	 deref(KOutA,Message);
	 deref(KOutB,Link);
	 Offset=set_offset(OpEntry);
	 if (!set_new_weight(ChP,OpEntry,Offset,1)){
	   /*fprintf(stderr,"post 15\n");*/
	   return(False);
	 }	  
	 if (!insert_message_at_end_queue(ChP,Link,Message,Offset)){
	   /*fprintf(stderr,"post 16\n");*/
	   return(False);
	 }  
	 MessageTail=add_mess_to_messtail(Message,MessageTail);
       }
     OpList=Cdr(OpList);
     deref_ptr(OpList);
   }         /* End while(Pass 2) */

 if (!unify(Ref_Word(MessageList),Ref_Word(MessageTail))){
   /*fprintf(stderr,"post 17\n");*/
   return(False);
 }
 return(set_reply_to("true",Reply));
 
} /* End spi_post */

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

channel_type(ChP, ChannelType)
heapP ChP;
int *ChannelType;
{
  heapP P;
  heapT V;

  if (!do_read_vector(Word(SPI_CHANNEL_TYPE,IntTag),Ref_Word(ChP))){
    return(False);
  }
  V = KOutA;
  if (IsRef(V)) {
    deref(V,P);
    if (!IsInt(*P)) {
      if(IsVar(*P)) {
	sus_tbl_add(P);
      }
      return(False);
    }
    V = *P;
  }
  if(IsInt(V)) {
    *ChannelType = Int_Val(V);
    return(True);
  }
  else {
    *ChannelType = -1;
    return(False);
  }
}

positive_integer(V)
heapT V;
{
  heapP P;

  if (IsRef(V)) {
    deref(V,P);
    if (!IsInt(*P)) {
      if(IsVar(*P)) {
	sus_tbl_add(P);
      }
      return(False);
    }
    V = *P;
  }	
  return (IsInt(V) && Int_Val(V) >= 0);
}
  
//*********************************************************************

int set_reply_trans(heapP SendPId, heapP SendCId, heapP SendCh,
		    heapP ReceivePId, heapP ReceiveCId, heapP ReceiveCh,
		    heapP Reply)
{
  heapP P;
 
  deref_ptr(SendPId);
  deref_ptr(SendCId);
  deref_ptr(SendCh);
  deref_ptr(ReceivePId);
  deref_ptr(ReceiveCId);
  deref_ptr(ReceiveCh);
  
  if (!do_make_tuple(Word(7,IntTag))){
    return(False);
  }
  deref(KOutA,P);
  
  built_fcp_str("true");
      
  if (!unify(Ref_Word((++P)),KOutA)){
    return(False);
  }
  if (!unify(Ref_Word((++P)),Ref_Word(SendPId))){
    return(False);
  }
  if (!unify(Ref_Word((++P)),Ref_Word(SendCId))){
    return(False);
  }
  if (!unify(Ref_Word((++P)),Ref_Word(SendCh))){
    return(False);
  }
  if (!unify(Ref_Word((++P)),Ref_Word(ReceivePId))){
    return(False);
  }
  if (!unify(Ref_Word((++P)),Ref_Word(ReceiveCId))){
    return(False);
  }
  if (!unify(Ref_Word((++P)),Ref_Word(ReceiveCh))){
    return(False);
  }
  if (!unify(Ref_Word(Reply),Ref_Word(P-7))){
    return(False);  
  }
  return(True);
}

int set_reply_delay(heapP ReceivePId, heapP ReceiveCId, heapP ReceiveCh,
		    heapP Reply)
{
  heapP P;
 
  deref_ptr(ReceivePId);
  deref_ptr(ReceiveCId);
  deref_ptr(ReceiveCh);
  
  if (!do_make_tuple(Word(4,IntTag))){
    return(False);
  }
  deref(KOutA,P);
  
  built_fcp_str("true");
      
  if (!unify(Ref_Word((++P)),KOutA)){
    return(False);
  }
  if (!unify(Ref_Word((++P)),Ref_Word(ReceivePId))){
    return(False);
  }
  if (!unify(Ref_Word((++P)),Ref_Word(ReceiveCId))){
    return(False);
  }
  if (!unify(Ref_Word((++P)),Ref_Word(ReceiveCh))){
    return(False);
  }
  if (!unify(Ref_Word(Reply),Ref_Word(P-4))){
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
}// end which_mode 

//*********************************************************************

transmit_instantaneous(OpEntry, Channel, PId, ChType, Value, Chosen ,Reply)
heapP OpEntry, Channel, PId, Value, Chosen ,Reply; int ChType; 
{
 int MsType;
 heapT Index, WeightIx ;
 heapP ChP ,MessageAnchor ,ThisMs ,Common ,CmVal ,CmChos ,NextMs ,Start;

 heapP InAmbient = 0;
 heapP ThisMsAmbient;

 /*fprintf(stderr,"transmit 0\n");*/
 deref_ptr(OpEntry);
 MsType=which_mode(OpEntry+SPI_MS_TYPE);
 if (Arity_of(*OpEntry) == SPI_AMBIENT_MS_SIZE) {
   InAmbient = OpEntry + SPI_MS_AMBIENT;
   deref_ptr(InAmbient);
 }
 ChP=OpEntry+SPI_MS_CHANNEL;
 deref_ptr(ChP);
 if (MsType==SPI_SEND)
   {
     if (!do_read_vector(Word(SPI_RECEIVE_ANCHOR,IntTag),Ref_Word(ChP))){
       /*fprintf(stderr,"transmit 1\n");*/
       return(False);
     }
     deref(KOutA,MessageAnchor);
     Index=SPI_RECEIVE_TAG;
     WeightIx = SPI_RECEIVE_WEIGHT;
   }
 else
   {
     if (!do_read_vector(Word(SPI_SEND_ANCHOR,IntTag),Ref_Word(ChP))){
       /*fprintf(stderr,"transmit 2\n");*/
       return(False);
     }
     deref(KOutA,MessageAnchor);
     Index=SPI_SEND_TAG;
     WeightIx = SPI_SEND_WEIGHT;
   }
 Start = MessageAnchor;
 if (ChType & SPI_RANDOM_FLAG) {
   int weight;

   if (!do_read_vector(Word(WeightIx,IntTag),Ref_Word(ChP))) {
     /*fprintf(stderr,"transmit 3\n");*/
     return(False);
   }
   deref_val(KOutA);
   if (!IsInt(KOutA)) {
     /*fprintf(stderr,"transmit 4\n");*/
     return(False);
   }
   weight = Int_Val(KOutA);
   if (weight) {
     if (!choose_random_start(MessageAnchor,
			      (random()/2147483647.0)*weight,
			      &Start)) {
       /*fprintf(stderr,"transmit 5\n");*/
       return(False);
     }
   }
 }
 NextMs = Start;
 do {
  ThisMs = NextMs;
  if (NextMs != MessageAnchor) {
      Common=ThisMs+SPI_MS_COMMON;
     deref_ptr(Common);
     if (!IsTpl(*Common)){
       /*fprintf(stderr,"transmit 7\n");*/
       return(False);
     }
     CmVal=Common+SPI_OP_VALUE;
     deref_ptr(CmVal);
     if (!IsWrt(*CmVal)){
       /*fprintf(stderr,"transmit 8\n");*/
      return(False);
     }
     CmChos=Common+SPI_OP_CHOSEN;
     deref_ptr(CmChos);
     if (!IsWrt(*CmChos)){
       /*fprintf(stderr,"transmit 9\n");*/
      return(False);
      }
     deref_ptr(Chosen);
     deref_ptr(CmChos);
     ThisMsAmbient = ThisMs + SPI_AMBIENT_CHANNEL;
     deref_ptr(ThisMsAmbient);
     if ((Chosen != CmChos) &&
	 (!InAmbient ||
	  unify(Ref_Word(InAmbient), Word(0,NilTag)) ||
	  (InAmbient != ThisMsAmbient)))
       {
	 if (!unify(Ref_Word(CmChos),Ref_Word(ThisMs+Index))){
	   /*fprintf(stderr,"transmit 10\n");*/
          return(False);
	 }
       	 if (!unify(Ref_Word(CmVal),Ref_Word(Value))){
	   /*fprintf(stderr,"transmit 11\n");*/
           return(False);
	 }
       	 if (!unify(Ref_Word(Chosen),Ref_Word(OpEntry+SPI_MS_TAGS))) {
	   /*fprintf(stderr,"transmit 12\n");*/
           return(False);
	 }
	 if (!discount(Common+SPI_OP_MSLIST)){
	   /*fprintf(stderr,"transmit 13\n");*/
	   return(False);
	 }
	 /*fprintf(stderr,"transmit x\n");*/
	 return (Index == SPI_RECEIVE_TAG) ?

	   set_reply_trans(
	     PId, (OpEntry+SPI_MS_CID), (OpEntry+SPI_MS_CHANNEL),
	     (Common+SPI_OP_PID), (ThisMs+SPI_MS_CID), (ThisMs+SPI_MS_CHANNEL),
	     Reply)
	   :
	   set_reply_trans(
	     (Common+SPI_OP_PID), (ThisMs+SPI_MS_CID), (ThisMs+SPI_MS_CHANNEL),
	     PId, (OpEntry+SPI_MS_CID), (OpEntry+SPI_MS_CHANNEL),
	     Reply);
       }
   }
   NextMs += SPI_MESSAGE_LINKS;
   deref_ptr(NextMs);
   if (!do_read_vector(Word(SPI_NEXT_MS,IntTag),Ref_Word(NextMs))){
     /*fprintf(stderr,"transmit 14\n");*/
     return(False);
   }
   deref(KOutA,NextMs);
   if (NextMs==ThisMs)
     break;
 } while (NextMs != Start);
 /*fprintf(stderr,"transmit q\n");*/
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
	/*fprintf(stderr,"discount 1\n");*/
	return(False);
      }
      ChP=Mes+SPI_MS_CHANNEL;  // the channel 
      deref_ptr(ChP);
      
      if (!do_read_vector(Word(SPI_CHANNEL_RATE,IntTag),Ref_Word(ChP))){
	/*fprintf(stderr,"discount 2\n");*/
	return(False);
      } 
     deref(KOutA,Pa); 
     if (!IsReal(*Pa)||real_val((Pa+1))<0) {
     //set_reply_to("Error-Base Rate not a Positive Real Number",Reply);
       /*fprintf(stderr,"discount 3\n");*/
       return(False);
     }
     Offset=set_offset(Mes);	
     if (!set_new_weight(ChP,Mes,Offset,0)){
       /*fprintf(stderr,"discount 4\n");*/
       return(False);
     }
     Link=Mes+SPI_MESSAGE_LINKS;
     deref_ptr(Link); 
     if (!do_read_vector(Word(SPI_NEXT_MS,IntTag),Ref_Word(Link))){
       /*fprintf(stderr,"discount 5\n");*/
       return(False);
     }
     deref(KOutA,Next); 
     if (!do_read_vector(Word(SPI_PREVIOUS_MS,IntTag),Ref_Word(Link))){
       /*fprintf(stderr,"discount 6\n");*/
       return(False);
     }
     deref(KOutA,Previous);
     if (!IsTpl(*Next)||Arity_of(*Next)!=SPI_MESSAGE_SIZE) {
       /*fprintf(stderr,"discount 7\n");*/
       return(False);
     } 
     if (!IsTpl(*Previous)||Arity_of(*Previous)!=SPI_MESSAGE_SIZE) {
       /*fprintf(stderr,"discount 8\n");*/
       return(False);
     }
     NextLink=Next+SPI_MESSAGE_LINKS;
     deref_ptr(NextLink);
     PreviousLink=Previous+SPI_MESSAGE_LINKS;
     deref_ptr(PreviousLink);
     if (!do_read_vector(Word(SPI_NEXT_MS,IntTag),Ref_Word(PreviousLink))){
       /*fprintf(stderr,"discount 9\n");*/
       return(False);
     }
     deref(KOutA,PreviousLinkNext); 
     if (!do_read_vector(Word(SPI_PREVIOUS_MS,IntTag),Ref_Word(NextLink))){
       /*fprintf(stderr,"discount 10\n");*/
       return(False);
     }
     deref(KOutA,NextLinkPrevious);
     
     if (!IsTpl(*NextLinkPrevious)||
	 Arity_of(*NextLinkPrevious)!=SPI_MESSAGE_SIZE) {
       /*fprintf(stderr,"discount 11\n");*/
       return(False);
     } 
     if (!IsTpl(*PreviousLinkNext)||
	 Arity_of(*PreviousLinkNext)!=SPI_MESSAGE_SIZE) {
       /*fprintf(stderr,"discount 12\n");*/
       return(False);
     }
     
     if (!do_store_vector(Word(SPI_PREVIOUS_MS,IntTag),Ref_Word(Previous),
			  Ref_Word(NextLink))){
       /*fprintf(stderr,"discount 13\n");*/
       return(False);
     }
     
     if (!do_store_vector(Word(SPI_NEXT_MS,IntTag),Ref_Word(Next),
			  Ref_Word(PreviousLink))){
       /*fprintf(stderr,"discount 14\n");*/
       return(False);
     }
     
     MsList=Cdr(MsList);
     deref_ptr(MsList);
   }
  return(True);  
}

//*********************************************************************

make_common_tuple(Pd,ML,Va,Ch)

 heapP Pd, ML, Va, Ch;
{ 
 heapT V0;
 heapP CST;

 if (!do_make_tuple(Word(SPI_COMMON_SIZE,IntTag))){
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

make_message_tuple(OpEntry,ChP,ComShTpl)
heapP OpEntry,ChP,ComShTpl;
{
  int MsType;
  heapP Tag,Pa,Ms,Message,Link;
  heapT TagS,TagR,V0,V1;
  
  if (Arity_of(*OpEntry) < SPI_MS_SIZE ||
      Arity_of(*OpEntry) > SPI_AMBIENT_MS_SIZE) {
    return(False);
  }
  if (!do_make_tuple(Word(SPI_MESSAGE_SIZE,IntTag))){
    return(False);
  }
  V0=KOutA;
  deref(KOutA,Ms)
    Message=Ms;
  if (!unify(Ref_Word(++Ms),Ref_Word(OpEntry+SPI_MS_TYPE))){/*Message Type */
    return(False);
  }
  if (!unify(Ref_Word(++Ms),Ref_Word(OpEntry+SPI_MS_CID))){   /* CId */
    return(False);
  }
  if (!unify(Ref_Word(++Ms),Ref_Word(ChP))){  /* Channel */
    return(False);
  }
  if (!unify(Ref_Word(++Ms),Ref_Word(OpEntry+SPI_MS_MULTIPLIER))){	 
    return(False);				/* Multiplier */
  }
  Tag=(OpEntry+SPI_MS_TAGS);
  deref_ptr(Tag);
  MsType=which_mode(OpEntry+SPI_MS_TYPE);
  if (MsType==SPI_SEND)
    { 
      TagS=*Tag;
      TagR=Word(0,IntTag);
    }  
  else if(MsType==SPI_RECEIVE)
    { 
      TagS=Word(0,IntTag);
      TagR=*Tag;
    }  
  else if (MsType==SPI_DIMER)
    {
      Pa=++Tag; 
      deref_ptr(Pa);
      TagS=*Pa;
      Pa=++Tag; 
      deref_ptr(Pa);
      TagR=*Pa;
    }       
  if (!unify(Ref_Word(++Ms),TagS)){  /* SendTag */
    return(False); 
  }
  if (!unify(Ref_Word(++Ms),TagR)){ /* ReceiveTag */
    return(False);
  }
  if (!unify(Ref_Word(++Ms),Ref_Word(ComShTpl))){ /* shared Common */
    return(False);
  }
  if (!do_make_vector(Word(2,IntTag))){
    return(False);
  }
  V1=KOutA;
  deref(KOutA,Link);
  if(!do_store_vector(Word(SPI_NEXT_MS,IntTag),
		      Ref_Word(Message),Ref_Word(Link))){
    return(False);   /* Next Message */
      } 
  if(!do_store_vector(Word(SPI_PREVIOUS_MS,IntTag),
		      Ref_Word(Message),Ref_Word(Link))){
    return(False);   /* Previous Message */
  }
  if (!unify(Ref_Word(++Ms),Ref_Word(Link))){  /* The Link Vector */
    return(False);
  }
  if (Arity_of(*OpEntry) == SPI_MS_SIZE) {
    if (!unify(Ref_Word(++Ms),Word(0,NilTag))) {
      return(False);
    }
  } else {
    if (!unify(Ref_Word(++Ms),Ref_Word(OpEntry+SPI_MS_AMBIENT))) {
      return(False);
    }
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

 if (!do_read_vector(Word((SPI_SEND_ANCHOR+Offset),IntTag)
		     ,Ref_Word(ChP))){
   return(False);
 }
 deref(KOutA,Anchor); 
 if(!do_store_vector(Word(SPI_NEXT_MS,IntTag),
		      Ref_Word(Anchor),Ref_Word(Link))){
    return(False);
  } 
  LinkAnchor=(Anchor+SPI_MESSAGE_LINKS);
  deref_ptr(LinkAnchor);
  if (!do_read_vector(Word(SPI_PREVIOUS_MS,IntTag),
		      Ref_Word(LinkAnchor))){
    return(False);
  }
  V0=KOutA;
  deref(V0,Pa);
  Pa=Pa+SPI_MESSAGE_LINKS;
  deref_ptr(Pa);
  if(!do_store_vector(Word(SPI_PREVIOUS_MS,IntTag),KOutA,Ref_Word(Link))){
    return(False);
  }
  if (!do_store_vector(Word(SPI_NEXT_MS,IntTag),Ref_Word(Message)
		       ,Ref_Word(Pa))){
    return(False);
  }
  if (!do_store_vector(Word(SPI_PREVIOUS_MS,IntTag),
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

update_channel_type(ChP, MsType, ChannelType, Reply)
heapP ChP,Reply;
int MsType;
int *ChannelType;            
{
  heapP Pa;
  int FinalType;
  int RandomFlag = *ChannelType & SPI_RANDOM_FLAG;

  if(!do_read_vector(Word(SPI_CHANNEL_RATE,IntTag),Ref_Word(ChP))){
    return(False);
  }
  deref(KOutA,Pa); 
  if (!IsReal(*Pa)||real_val((Pa+1))<0) {
    set_reply_to("Error - Base Rate not a Positive Real Number",Reply);
    *ChannelType = SPI_SINK;
    return(True);
  }
  if(!do_read_vector(Word(SPI_WEIGHT_TUPLE,IntTag),Ref_Word(ChP))){
    return(False);
  }
  deref(KOutA,Pa);
  if (IsTpl(KOutA)) {
    heapP Pb;
    int Arity = Arity_of(KOutA);
    if (Arity == 1)
      return(False);
    Pb = Pa += 2;
    deref_ptr(Pb);
    KOutA = *Pb;
  }
  switch (MsType) {
    case SPI_SEND:
    case SPI_RECEIVE:
      FinalType = (Int_Val(KOutA) == SPI_DEFAULT_WEIGHT_INDEX) ?
	SPI_BIMOLECULAR : SPI_BIMOLECULAR_PRIME;
      break;
    case SPI_DIMER:
      FinalType = (Int_Val(KOutA) == SPI_DEFAULT_WEIGHT_INDEX) ?
	SPI_HOMODIMERIZED : SPI_HOMODIMERIZED_PRIME;
      break;
    default:
      *ChannelType = SPI_SINK;
      set_reply_to("Error - Unrecognized message type", Reply);
      return(True);
  }
  FinalType |= RandomFlag;
  if (!do_store_vector(Word(SPI_CHANNEL_TYPE,IntTag),
		       Word(FinalType,IntTag),Ref_Word(ChP))){
    return(False);
  }
  *ChannelType = FinalType;
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

 if (!do_read_vector(Word((SPI_SEND_WEIGHT+Offset),IntTag)
		     ,Ref_Word(ChP))){
   return(False);
 }
 V0=KOutA;
 deref_val(V0);
 Mult=OpEntry+SPI_MS_MULTIPLIER;
 deref_ptr(Mult);
 V1=*Mult;
 if (Positive)
  NewVal=(Int_Val(V0)+Int_Val(V1));
 else 
  NewVal=(Int_Val(V0)-Int_Val(V1));
 if (!do_store_vector(Word((SPI_SEND_WEIGHT+Offset),IntTag),
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

 MsType=which_mode(OpEntry+SPI_MS_TYPE);
 if (MsType==SPI_SEND||MsType==SPI_DIMER)
   return(0);
 else
   return(2); 
}

//*************************** Close Action ************************************


int set_r_to(heapP L,heapP T);

int spi_close(ChT ,Reply)  
 heapP ChT ,Reply;  
{
 int RefCount, Arity, Index;
 heapP ChP ,ChPrev ,ChNext ,Lp ,Pt ,ChNextPrev ,ChPrevNext ,Pa;
 heapT V0;
 
 
 deref_ptr(ChT);
 if (!IsTpl(*ChT)) {
   if(IsVar(*ChT)) {
     sus_tbl_add(ChT);
   }
    return(False);
 }
 
 *HP=Word(0,NilTag); //The start of the list
 Lp=HP;
 *HP++;
  
 deref_ptr(Reply);
 
 Pa=STP;

 Index=Arity=Arity_of(*ChT);
 while ((Index--)>0) //going over all the channels
   { 
    ChP=++ChT;
    deref_ptr(ChP);

    if (!vctr_var_s(ChP,CHANNEL_SIZE)){
       return(False);
     }
   
    if (!do_read_vector(Word(SPI_CHANNEL_REFS,IntTag),Ref_Word(ChP))){
       return(False);
      }

    V0=KOutA;
    deref_val(V0);
    RefCount=Int_Val(V0);
    
    if (!IsInt(V0)||(RefCount <= 0)){
      return(set_reply_to("Error - Problem In ReferenceCount",Reply));
    }

    if (!do_read_vector(Word(SPI_PREV_CHANNEL,IntTag),Ref_Word(ChP))){
       return(False);
     }

    deref(KOutA,ChPrev);

    if (!vctr_var_s(ChPrev,CHANNEL_SIZE)){
      return(set_reply_to("Error - SPI_PREV_CHANNEL - not a Channel",Reply));
    }
    
    if (!do_read_vector(Word(SPI_NEXT_CHANNEL,IntTag),Ref_Word(ChP))){
      return(False);
    }
    
    deref(KOutA,ChNext);
    
    if (!vctr_var_s(ChNext,CHANNEL_SIZE)){
      return(set_reply_to("Error - SPI_NEXT_CHANNEL - not a Channel",Reply));
    }
    RefCount--;
    if (!do_store_vector(Word(SPI_CHANNEL_REFS,IntTag),
			 Word(RefCount,IntTag),
			 Ref_Word(ChP))){
      return(False);
    }
    if (RefCount==0)
      {
       if (!do_read_vector(Word(SPI_NEXT_CHANNEL,IntTag),Ref_Word(ChPrev))){
	   return(False);
          }
         deref(KOutA,ChPrevNext)
	 if (!vctr_var_s(ChPrevNext,CHANNEL_SIZE)){
	   return(set_reply_to("Error - SPI_PREV_NEXT_CHANNEL - not a Channel",
			       Reply));
	 }
	 if (!do_read_vector(Word(SPI_PREV_CHANNEL,IntTag),Ref_Word(ChNext))){
	   return(False);
	 }
	 deref(KOutA,ChNextPrev);
	 if (!vctr_var_s(ChNextPrev,CHANNEL_SIZE)){
	   return(set_reply_to("Error - SPI_NEXT_PREV_CHANNEL - not a Channel",
			       Reply));
	 }
	 if (!do_store_vector(Word(SPI_PREV_CHANNEL,IntTag),
			      Ref_Word(ChPrev),
			      Ref_Word(ChNext))){
	   return(False);
	 }
	 if (!do_store_vector(Word(SPI_NEXT_CHANNEL,IntTag),
			      Ref_Word(ChNext),
			      Ref_Word(ChPrev))){
	   return(False);
	 }
	 if (!do_store_vector(Word(SPI_PREV_CHANNEL,IntTag),
			      Ref_Word(ChP),
			      Ref_Word(ChP))){
	   return(False);
	 }
	 if (!do_store_vector(Word(SPI_NEXT_CHANNEL,IntTag),
			      Ref_Word(ChP),
			      Ref_Word(ChP))){
	   return(False);
	 }
	 if (heap_space(sizeof(HP)) < 4) {
	   err_tbl_add(MACHINE, ErHPSPACE);
	   return(False);	
	 }
	 Pt=HP; 
	 *HP=L_Ref_Word((HP+2));//the car is the deleted channel's index
	 *HP++;
	 *HP=Ref_Word((HP+2));
	 *HP++;
	 *HP++=Word(Arity-Index,IntTag);
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

} /* End spi_close */

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

int spi_step(Now ,Anchor ,NowP ,Reply ) 
               //the tuple contain 4 arguments -{Now ,Anchor , Now' ,Reply'}
heapP Now ,Anchor ,NowP ,Reply ;
{
 int  i,ChannelType,SendWeight ,ReceiveWeight ,DimerWeight ,TranS=False ,Ret;
 double SumWeights=0,Selector,NowVal,BaseRate,Uniform1=0,Uniform2=0,Val ; 
 heapP NextChannel ,ChP ,Pa ;
 heapT V0;

 deref_ptr(Now);		/* Assumed Real */
 deref_ptr(Anchor);		/* Assumed Vector */
 deref_ptr(NowP);		/* Assumed Var */
 deref_ptr(Reply);		/* Assumed Var */

 if (!do_read_vector(Word(SPI_NEXT_CHANNEL,IntTag),Ref_Word(Anchor))){
   return(False);
 }
 deref(KOutA,NextChannel);
 if (NextChannel!=Anchor) {
   do {
     ChP=NextChannel;
     if (!do_read_vector(Word(SPI_BLOCKED,IntTag),Ref_Word(ChP))){
       return(False);	
     }
     deref_val(KOutA); 
     if (!Int_Val(KOutA)) {                /* not blocked */
       double Result = 0.0;

       if (!channel_type(ChP, &ChannelType)) {
	 return(False);
       }
       switch(ChannelType & SPI_TYPE_MASK)
	 {     
	 case SPI_DELAY :
	 case SPI_BIMOLECULAR :
	   if (!get_sum_weight(ChP, ChannelType, &Result, Reply))
	     return(False);
	   if (!IsVar(*Reply))
	     return(True);
	   SumWeights += Result;
	   break;
	 case SPI_HOMODIMERIZED:
	   if (more_than_one_ms(ChP)) {  
	     if (!get_sum_weight(ChP, ChannelType, &Result, Reply))
	       return(False);
	     if (!IsVar(*Reply))
	       return(True);
	     SumWeights += Result;
	   }
	   break;
	 case    SPI_UNKNOWN: 
	 case    SPI_SINK: 
	   break;
	 default: 
	   return(set_reply_to("Wrong Channel Type",Reply));  
	 }                  /* End Switch */ 
     }
     if (!do_read_vector(Word(SPI_NEXT_CHANNEL,IntTag),Ref_Word(ChP))){
       return(False);
     }
     deref(KOutA,NextChannel);
   } while(NextChannel!=Anchor);   /* End While - Pass 2 */

   if  (SumWeights != 0) {
     Uniform1 = random()/2147483647.0;
     Selector = Uniform1*SumWeights;

     if (!do_read_vector(Word(SPI_NEXT_CHANNEL,IntTag),Ref_Word(Anchor))){
       return(False);
     }
     deref(KOutA,NextChannel);

     do {                      /* Pass 2 */
       ChP=NextChannel;
       if (!do_read_vector(Word(SPI_BLOCKED,IntTag),Ref_Word(ChP)))
	 return(False);	
       deref_val(KOutA);
       if (!Int_Val(KOutA)) {                /* not blocked */

	 double Result = 0.0;
	 int BasicType;

	 if (!channel_type(ChP, &ChannelType)) {
	   return(False);
	 }
	 BasicType = ChannelType & SPI_TYPE_MASK;
	 if (BasicType == SPI_BIMOLECULAR || BasicType == SPI_HOMODIMERIZED ||
	     BasicType == SPI_DELAY) {
	   if (!get_selector(ChP, BasicType, &Result))
	     return(False);
	   Selector -= Result;
	   if (Selector <= 0) {
	     int Random = ChannelType&SPI_RANDOM_FLAG;
	     if (BasicType == SPI_BIMOLECULAR) {
	       Uniform2 = random()/2147483647.0;
	       Ret = transmit_biomolecular(ChP, Random, Uniform1, Uniform2,
					   Reply);
	       TranS = True;
	     }
	     else if(BasicType == SPI_DELAY) {
	       Uniform2 = random()/2147483647.0;
	       Ret = transmit_delay(ChP, Random, Reply);
	       TranS = True;
	     }
	     else if (more_than_one_ms(ChP)) {
	       Uniform2 = random()/2147483647.0;
	       /*fprintf(stderr,"transmit\n");*/
	       Ret = transmit_homodimerized(ChP, Random, Uniform1, Uniform2,
					    Reply);
	       /*fprintf(stderr,"reply = %i\n", Reply);*/
	       TranS = True;
	     }          /* End if more_than_one */
	   }          /* End Selector <= 0  */
	 }          /* End Transmittable */
       }          /* End if Not Blocked */

       if (!do_read_vector(Word(SPI_NEXT_CHANNEL,IntTag),Ref_Word(ChP))){
	 return(False);
       }
       deref(KOutA,NextChannel);
     } while((NextChannel!=Anchor) && (!TranS));      /* End While  Pass 2 */

     if (TranS==True) {
       if (Ret==True) { 
	 if (!set_nowp(Now, NowP, Uniform2, SumWeights)){
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
}    /* End spi_step function */ 
 

//************************** Step Functions ***********************************

int
choose_random_start(OpEntry, select, Start)
 heapP OpEntry, *Start;
 double select;
{
  /*fprintf(stderr,"choose 0 - Anchor = %x\n", OpEntry);*/
  while (select > 0) {
    heapP multP;

    OpEntry += SPI_MESSAGE_LINKS;
    deref_ptr(OpEntry);
    if (!do_read_vector(Word(SPI_NEXT_MS,IntTag),Ref_Word(OpEntry))){
      /*fprintf(stderr,"choose 1 - Entry = %x\n", OpEntry);*/
      return(False);
    }
    deref(KOutA,OpEntry);
    multP = OpEntry+SPI_MS_MULTIPLIER;
    deref_ptr(multP);
    if (!IsInt(*multP)) {
      /*fprintf(stderr,"choose 2\n");*/
      return(False);
    }
    select -= (Int_Val(*multP));
  }
  *Start = OpEntry;
  /*fprintf(stderr,"choose 1 - Start = %x\n", OpEntry);*/
  return(True);
}

int more_than_one_ms(ChP)
heapP ChP;
 {
   heapP MesLink,NextMs,PrevMes,MsAnchor;

   if (!do_read_vector(Word(SPI_DIMER_ANCHOR,IntTag),Ref_Word(ChP))){
     return(False);
   }
   deref(KOutA,MsAnchor);
   MesLink=MsAnchor+SPI_MESSAGE_LINKS;
   deref_ptr(MesLink);
   if (!do_read_vector(Word(SPI_NEXT_MS,IntTag),Ref_Word(MesLink))){
     return(False);
   }  
   deref(KOutA,NextMs);
   return(MsAnchor!=NextMs); 
}
               
//********************************************************************

int get_sum_weight(heapP ChP, int Type, double *Result, heapP Reply)
{
  double BaseRate;
  int SendWeight,ReceiveWeight,DimerWeight,WeightIndex;
  heapP Pa;
  int argn;
  double argv[100];

  if (!do_read_vector(Word(SPI_CHANNEL_RATE,IntTag),Ref_Word(ChP))){
    return(False);
  }
  deref(KOutA,Pa);
  if(!IsReal(*Pa)||(BaseRate=real_val((Pa+1)))<=0) {
    set_reply_to("Error - Base Rate not a Positive Real Number",Reply);
    return(False);
  }
 if ((Type & SPI_TYPE_MASK) == SPI_DELAY) {
    SendWeight = 1 ;
  }
  else {
    if (!do_read_vector(Word(SPI_SEND_WEIGHT,IntTag),Ref_Word(ChP)))
      return(False);
    deref_val(KOutA);
    if (!IsInt(KOutA)||(SendWeight=Int_Val(KOutA))<0) {
      set_reply_to("Error - Send Weight not a Non-negative Integer",Reply);
      return(False);
    }
  }
  if (!validate_weighter(ChP, &argn, argv)) {
    return(False);
  }
  if (!IsInt(KOutA) || (WeightIndex=Int_Val(KOutA)) < 0)
    return set_reply_to("Error - Invalid Weight Index", Reply);
  if (SendWeight > 0)
    switch (Type & SPI_TYPE_MASK)
      {
      case SPI_DELAY:
      case SPI_BIMOLECULAR:
	if (!do_read_vector(Word(SPI_RECEIVE_WEIGHT,IntTag),Ref_Word(ChP))) {
	  return(False);
	}
	deref_val(KOutA);
	if (!IsInt(KOutA) || (ReceiveWeight = Int_Val(KOutA))<0)
	  return set_reply_to("Error - Receive Weight not a Positive Integer",
			      Reply);
	if (WeightIndex != SPI_DEFAULT_WEIGHT_INDEX) {
	  if (ReceiveWeight > 0)
	    *Result = spi_compute_bimolecular_weight(WeightIndex, BaseRate,
						     SendWeight, ReceiveWeight,
                                                     argn, argv);
	  else
	    *Result = 0.0;
	}
	else
	  *Result = BaseRate*SendWeight*ReceiveWeight;
	return True;
      case SPI_HOMODIMERIZED:
	DimerWeight=SendWeight;         
	if (WeightIndex != SPI_DEFAULT_WEIGHT_INDEX)
	  *Result = spi_compute_homodimerized_weight(WeightIndex, BaseRate,
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

int get_selector(heapP ChP, int BasicType, double *Result)
{  
  int SendWeight,ReceiveWeight,DimerWeight,WeightIndex;
  double BaseRate; 
  heapP Pa;
  int argn;
  double argv[100];

  if (!do_read_vector(Word(SPI_CHANNEL_RATE,IntTag),Ref_Word(ChP)))
    return(False);
  deref(KOutA,Pa);
  BaseRate=real_val((Pa+1));
  if(BasicType == SPI_DELAY)
    SendWeight = 1;
  else {
    if (!do_read_vector(Word(SPI_SEND_WEIGHT,IntTag),Ref_Word(ChP)))
      return(False);
    deref_val(KOutA);
    SendWeight=Int_Val(KOutA); 
  }

  if (SendWeight > 0) {
    if (!validate_weighter(ChP, &argn, argv))
      return(False);
    WeightIndex = Int_Val(KOutA);

    switch (BasicType)
      {
      case SPI_DELAY:
      case SPI_BIMOLECULAR: 
	if (!do_read_vector(Word(SPI_RECEIVE_WEIGHT,IntTag),Ref_Word(ChP)))
	  return(False);
	deref_val(KOutA);
	ReceiveWeight = Int_Val(KOutA);
	if (ReceiveWeight > 0) {
	  if (WeightIndex != SPI_DEFAULT_WEIGHT_INDEX)
	    *Result = spi_compute_bimolecular_weight(WeightIndex, BaseRate,
						     SendWeight, ReceiveWeight,
                                                     argn, argv);
	  else
	    *Result = BaseRate*SendWeight*ReceiveWeight; 
	}
	break;
      case SPI_HOMODIMERIZED:
	DimerWeight = SendWeight;
	if (WeightIndex != SPI_DEFAULT_WEIGHT_INDEX)
	  *Result = spi_compute_homodimerized_weight(WeightIndex, BaseRate,
						     DimerWeight,
						     argn, argv);
	else
	  *Result = (BaseRate*DimerWeight*(DimerWeight-1))/2.0;
      }
  }
  return True;
}


//********************************************************************

int set_nowp(heapP Now, heapP NowP, double Uniform, double SumWeights)
{
  double NowVal;
  heapT V0;

  NowVal = real_val((Now+1)); 
  NowVal -= log(Uniform)/SumWeights;
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
  if (!do_store_vector(Word(SPI_BLOCKED,IntTag),
		       Word(True,IntTag),
		       Ref_Word(ChP))){
    return(False);
  }
  return(set_reply_to("done",Reply));
}

//********************************************************************

transmit_biomolecular(ChP, Random, Uniform1, Uniform2, Reply)
heapP ChP, Reply;
int Random;
double Uniform1, Uniform2;
{
  heapP RMsAnchor,ReceiveMessage,RMs,RCommon,RComValue,RComChos,RMsList,
	SMsAnchor,SendMessage,SMs,SCommon,SComValue,SComChos,SMsList,Pa;
  heapP ReceiveStart,SendStart;

  heapP ReceiveAmbient, SendAmbient;

  if (!do_read_vector(Word(SPI_RECEIVE_ANCHOR,IntTag),Ref_Word(ChP))){
    return(False);
  }
  deref(KOutA,RMsAnchor);
  if(!IsTpl(*RMsAnchor)){
    return(False);
  }
  Pa = RMsAnchor+SPI_MS_TYPE;
  deref_ptr(Pa);
  if ((Int_Val(*Pa) != SPI_MESSAGE_ANCHOR)){
    return(False);
  }

  if (!do_read_vector(Word(SPI_SEND_ANCHOR,IntTag),Ref_Word(ChP))){
    return(False);
  }
  deref(KOutA,SMsAnchor);
  if(!IsTpl(*SMsAnchor)){
    return(False);
  }
  Pa = SMsAnchor+SPI_MS_TYPE;
  deref_ptr(Pa);
  if ((Int_Val(*Pa) != SPI_MESSAGE_ANCHOR)){
    return(False);
  }

  if (Random) {
    int ReceiveWeight, SendWeight;

    if (!do_read_vector(Word(SPI_SEND_WEIGHT, IntTag), Ref_Word(ChP)))
      return(False);
    deref_val(KOutA);
    if (!IsInt(KOutA) || (SendWeight = Int_Val(KOutA)) < 0) {
      set_reply_to("Error - Send Weight not a Non-negative Integer", Reply);
      return(False);
    }
    if (!do_read_vector(Word(SPI_RECEIVE_WEIGHT,IntTag),Ref_Word(ChP)))
      return(False);
    deref_val(KOutA);
    if (!IsInt(KOutA) || (ReceiveWeight = Int_Val(KOutA)) < 0) {
      set_reply_to("Error - Receive Weight not a Non-negative Integer", Reply);
      return(False);
    }
    if (!(choose_random_start(RMsAnchor,
			     Uniform1*ReceiveWeight,
			     &ReceiveStart)
		&&
	  choose_random_start(SMsAnchor,
			      (random()/2147483647.0)*SendWeight,
			      &SendStart))) {
       return(False);
     }
  }
  else {
    ReceiveStart = RMsAnchor;
    SendStart = SMsAnchor;
  }
  ReceiveMessage = ReceiveStart;

  do {
    if (ReceiveMessage != RMsAnchor) {
     RMs=ReceiveMessage;
     RCommon=RMs+SPI_MS_COMMON;
     deref_ptr(RCommon);
     if (!IsTpl(*RCommon)){
       return(False);
     }
     RComValue=RCommon+SPI_OP_VALUE;
     deref_ptr(RComValue);
     if (!IsWrt(*RComValue)){
       return(False);
     }
     RComChos=RCommon+SPI_OP_CHOSEN;
     deref_ptr(RComChos);
     if (!IsWrt(*RComChos)){
       return(False);
     }
     SendMessage = SendStart;

     ReceiveAmbient = ReceiveMessage+SPI_AMBIENT_CHANNEL;
     deref_ptr(ReceiveAmbient);

     do {
       if (SendMessage != SMsAnchor) {	       
	 SMs=SendMessage;
	 SCommon=SMs+SPI_MS_COMMON;
	 deref_ptr(SCommon);
	 if (!IsTpl(*SCommon)){
	   return(False);
	 }
	 SComValue=SCommon+SPI_OP_VALUE;
	 deref_ptr(SComValue);
	 if (!IsWrt(*SComValue)){
	   return(False);
	 }
	 SComChos=SCommon+SPI_OP_CHOSEN;
	 deref_ptr(SComChos);
	 if (!IsWrt(*SComChos)){
	   return(False);
	 }

	 SendAmbient = SendMessage+SPI_AMBIENT_CHANNEL;
	 deref_ptr(SendAmbient);
	 if (RComChos != SComChos &&
	     (IsNil(*SendAmbient) || ReceiveAmbient != SendAmbient))
	   {
	     RMsList=RCommon+SPI_OP_MSLIST;
	     deref_ptr(RMsList);
	     if (!IsList(*RMsList)){
	       return(False);
	     }
	     SMsList=SCommon+SPI_OP_MSLIST;
	     deref_ptr(SMsList);
	     if (!IsList(*SMsList)){
	       return(False);
	     }
	     if (!unify(Ref_Word(SComChos),Ref_Word(SMs+SPI_SEND_TAG))){
	       return(False);
	     } 
	     if (!unify(Ref_Word(RComChos),Ref_Word(RMs+SPI_RECEIVE_TAG))){
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
	     return
             set_reply_trans(SCommon+SPI_OP_PID,
                             SMs+SPI_MS_CID,
                             SMs+SPI_MS_CHANNEL,
                             RCommon+SPI_OP_PID,
                             RMs+SPI_MS_CID,
                             RMs+SPI_MS_CHANNEL,
		 Reply);
	   }
       }
       if (!do_read_vector(Word(SPI_NEXT_MS,IntTag),
			   Ref_Word(SendMessage+SPI_MESSAGE_LINKS))){
	 return(False);
       }
       deref(KOutA,SendMessage);
     } while (SendMessage != SendStart);
    }
    if (!do_read_vector(Word(SPI_NEXT_MS,IntTag),
			Ref_Word(ReceiveMessage+SPI_MESSAGE_LINKS))){
      return(False);
    }
    deref(KOutA,ReceiveMessage);
  } while (ReceiveMessage != ReceiveStart);
  return (BLOCKED);
}  

//********************************************************************

transmit_homodimerized (ChP, Random, Uniform1, Uniform2, Reply)
heapP ChP, Reply;
int Random;
double Uniform1, Uniform2;
{
  heapP DimerAnchor,
 	ReceiveMessage,RMs,RCommon,RComValue,RComChos,RMsList,
 	DimerMessage,DMs,DCommon,DComValue,DComChos,DMsList,Dm;
  heapP DimerStart;

  heapP ReceiveAmbient, DimerAmbient;
 
  if (!do_read_vector(Word(SPI_DIMER_ANCHOR,IntTag),Ref_Word(ChP))){
    return(False);
  }
  deref(KOutA,DimerAnchor);

  DMs = DimerAnchor+SPI_MS_TYPE;
  deref_ptr(DMs);
  if ((Int_Val(*DMs) != SPI_MESSAGE_ANCHOR)){
    return(False);
  }

  if (Random) {
    int DimerWeight;

    if (!do_read_vector(Word(SPI_DIMER_WEIGHT, IntTag), Ref_Word(ChP)))
      return(False);
    deref_val(KOutA);
    if (!IsInt(KOutA) || (DimerWeight = Int_Val(KOutA)) < 0) {
      set_reply_to("Error - Dimer Weight not a Non-negative Integer", Reply);
      return(False);
    }
    if (!(choose_random_start(DimerAnchor,
			     Uniform1*DimerWeight,
			     &ReceiveMessage)
		&&
	  choose_random_start(DimerAnchor,
			      (random()/2147483647.0)*DimerWeight,
			      &DimerStart))) {
       return(False);
     }
    DimerMessage = DimerStart;
    /*fprintf(stderr,"anchor = %x, receive = %x, start = %x\n",N
      DimerAnchor, ReceiveMessage, DimerStart);*/
  }
  else {
    if (!do_read_vector(Word(SPI_NEXT_MS,IntTag),
			Ref_Word(DimerAnchor+SPI_MESSAGE_LINKS))){
      return(False);
    } 
    deref(KOutA,ReceiveMessage);
    if (!do_read_vector(Word(SPI_NEXT_MS,IntTag),
			Ref_Word(ReceiveMessage+SPI_MESSAGE_LINKS))){
      return(False);
    } 
    deref(KOutA,DimerMessage);
    DimerStart = DimerAnchor;
  }
  RMs = ReceiveMessage;
  ReceiveAmbient = ReceiveMessage+SPI_AMBIENT_CHANNEL;
  deref_ptr(ReceiveAmbient);
  RCommon = ReceiveMessage+SPI_MS_COMMON;
  deref_ptr(RCommon);
  if (!IsTpl(*RCommon)){
    return(False);
  }
  RComValue = RCommon+SPI_OP_VALUE;
  deref_ptr(RComValue);
  if (!IsWrt(*RComValue)){
    return(False);
  }
  RComChos = RCommon+SPI_OP_CHOSEN;
  deref_ptr(RComChos);
  if (!IsWrt(*RComChos)){
    return(False);
  }

  do {
    DMs = DimerMessage;
    if(DimerMessage != DimerAnchor) {
      DCommon=DMs+SPI_MS_COMMON;
      deref_ptr(DCommon);
      if (!IsTpl(*DCommon)){
	return(False);
      }
      DComValue=DCommon+SPI_OP_VALUE;
      deref_ptr(DComValue);
      if (!IsWrt(*DComValue)){
	return(False);
      }
      DComChos=DCommon+SPI_OP_CHOSEN;
      deref_ptr(DComChos);
      if (!IsWrt(*DComChos)){
	return(False);
      }
      DimerAmbient = DCommon+SPI_AMBIENT_CHANNEL;
      deref_ptr(DimerAmbient);

      if (DComChos!=RComChos &&
	  (IsNil(*DimerAmbient) || ReceiveAmbient != DimerAmbient))
	{
	  RMsList=RCommon+SPI_OP_MSLIST;
	  deref_ptr(RMsList);
	  if (!IsList(*RMsList)){
	    return(False);
	  }
	  DMsList=DCommon+SPI_OP_MSLIST;
	  deref_ptr(DMsList);
	  if (!IsList(*DMsList)){
	    return(False);
	  }
	  if (!unify(Ref_Word(RComChos),Ref_Word(RMs+SPI_RECEIVE_TAG))){
	    return(False);
	  } 
	  if (!unify(Ref_Word(DComChos),Ref_Word(DMs+SPI_SEND_TAG))){
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
	  return
            set_reply_trans(DCommon+SPI_OP_PID,
                            DMs+SPI_MS_CID,
                            DMs+SPI_MS_CHANNEL,
                            RCommon+SPI_OP_PID,
                            RMs+SPI_MS_CID,
                            RMs+SPI_MS_CHANNEL,
	      Reply);
	}
    }
    if (!do_read_vector(Word(SPI_NEXT_MS,IntTag),
			Ref_Word(DMs+SPI_MESSAGE_LINKS))){
      return(False);
    }  
    deref(KOutA,DimerMessage);
  } while(DimerMessage != DimerStart);
  return(BLOCKED);
}

//********************************************************************

transmit_delay(ChP, Random, Reply)
heapP ChP, Reply;
{
  heapP RMsAnchor,ReceiveMessage,RMs,RCommon,RComValue,RComChos,RMsList,
	Pa;
  heapP ReceiveStart,SendStart;

  heapP ReceiveAmbient, SendAmbient;

  if (!do_read_vector(Word(SPI_RECEIVE_ANCHOR,IntTag),Ref_Word(ChP))){
    return(False);
  }
  deref(KOutA,RMsAnchor);
  if(!IsTpl(*RMsAnchor)){
    return(False);
  }
  Pa = RMsAnchor+SPI_MS_TYPE;
  deref_ptr(Pa);
  if ((Int_Val(*Pa) != SPI_MESSAGE_ANCHOR)){
    return(False);
  }

  ReceiveStart = RMsAnchor;
  ReceiveMessage = ReceiveStart;

  do {
    if (ReceiveMessage != RMsAnchor) {
     RMs=ReceiveMessage;
     RCommon=RMs+SPI_MS_COMMON;
     deref_ptr(RCommon);
     if (!IsTpl(*RCommon)){
       return(False);
     }
     RComValue=RCommon+SPI_OP_VALUE;
     deref_ptr(RComValue);
     if (!IsWrt(*RComValue)){
       return(False);
     }
     RComChos=RCommon+SPI_OP_CHOSEN;
     deref_ptr(RComChos);
     if (!IsWrt(*RComChos)){
       return(False);
     }

     ReceiveAmbient = ReceiveMessage+SPI_AMBIENT_CHANNEL;
     deref_ptr(ReceiveAmbient);

    if (!do_read_vector(Word(SPI_NEXT_MS,IntTag),
			Ref_Word(ReceiveMessage+SPI_MESSAGE_LINKS))){
      return(False);
    }
    deref(KOutA,ReceiveMessage);
     ReceiveAmbient = ReceiveMessage+SPI_AMBIENT_CHANNEL;
     deref_ptr(ReceiveAmbient);

     RMsList=RCommon+SPI_OP_MSLIST;
     deref_ptr(RMsList);
     if (!IsList(*RMsList))
       return(False);
     if (!unify(Ref_Word(RComChos),Ref_Word(RMs+SPI_RECEIVE_TAG)))
       return(False);
     if (!unify(Ref_Word(RComValue),Word(0,NilTag)))
       return(False);
     if (!discount(RMsList))
       return(False);
     return
       set_reply_delay(RCommon+SPI_OP_PID,
                       RMs+SPI_MS_CID,
                       RMs+SPI_MS_CHANNEL,
       Reply);
    }
    if (!do_read_vector(Word(SPI_NEXT_MS,IntTag),
			Ref_Word(ReceiveMessage+SPI_MESSAGE_LINKS))){
      return(False);
    }
    deref(KOutA,ReceiveMessage);
  } while (ReceiveMessage != ReceiveStart);
  return (BLOCKED);
}  

//*************************** Index Action ************************************

int spi_index(Name, Index, Reply)
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
  result = spi_weight_index((char *)(Name+2));
  if (!unify(Ref_Word(Index),Word(result,IntTag))){
    return(False);
  }
  return set_reply_to("true", Reply);
}

//**************************** Rate Function *********************************

int spi_rate(Channel, Weight, Reply ) 
               //the tuple contain  arguments -{Channel, Weight' ,Reply'}
heapP Channel, Weight, Reply ;
{
  heapP Pa;
  heapT V0;
  int  i, ChannelType, DimerWeight, ReceiveWeight, SendWeight, WeightIndex;
  double BaseRate, Result = 0.0;
  int argn;
  double argv[100];

  deref_ptr(Channel);		/* Assumed Vector */
  deref_ptr(Weight);		/* Assumed Var */
  deref_ptr(Reply);		/* Assumed Var */

  if (!do_read_vector(Word(SPI_CHANNEL_RATE, IntTag),Ref_Word(Channel))){
    return(False);
  }
  deref(KOutA,Pa);
  if(!IsReal(*Pa) || (BaseRate = real_val((Pa+1))) <= 0.0) {
    set_reply_to("Error - BaseRate not a Positive Real Number", Reply);
    return(False);
  }
  if (!do_read_vector(Word(SPI_SEND_WEIGHT, IntTag), Ref_Word(Channel)))
    return(False);
  deref_val(KOutA);
  if (!IsInt(KOutA) || (SendWeight = Int_Val(KOutA)) < 0) {
    set_reply_to("Error - Send Weight not a Non-negative Integer", Reply);
    return(False);
  }
  if (!do_read_vector(Word(SPI_RECEIVE_WEIGHT,IntTag),Ref_Word(Channel)))
    return(False);
  deref_val(KOutA);
  if (!IsInt(KOutA) || (ReceiveWeight = Int_Val(KOutA)) < 0) {
    set_reply_to("Error - Receive Weight not a Non-negative Integer", Reply);
    return(False);
  }
  if (!validate_weighter(Channel, &argn, argv))
    return(False);
  WeightIndex = Int_Val(KOutA);

  if (!channel_type(Channel, &ChannelType)) {
    return(False);
  }
  switch (ChannelType & SPI_TYPE_MASK)
    {
    case SPI_BIMOLECULAR: 
      {
	if (WeightIndex != SPI_DEFAULT_WEIGHT_INDEX)
	  Result = spi_compute_bimolecular_weight(WeightIndex, BaseRate,
						  SendWeight, ReceiveWeight,
						  argn, argv);
	else
	  Result = BaseRate*SendWeight*ReceiveWeight; 
      }
      break;
    case SPI_HOMODIMERIZED:
      DimerWeight = SendWeight;
      if (WeightIndex != SPI_DEFAULT_WEIGHT_INDEX)
	Result = spi_compute_homodimerized_weight(WeightIndex, BaseRate,
						   DimerWeight,
						   argn, argv);
      else
	Result = (BaseRate*DimerWeight*(DimerWeight-1))/2.0;
    }
  V0 = Ref_Word(HP);
  *HP++ = Word(0, RealTag);
  real_copy_val(((realT) Result), HP);
  HP += realTbits/heapTbits;
  if (!unify(Ref_Word(Weight), V0)){
    return(False);
  }
  return set_reply_to("true", Reply);
}

validate_weighter(Channel, argnr, argv)
heapP Channel;
int *argnr;
double *argv;
{
  heapP Pa;
  heapT Pi;
  int argn = 0;

  if (!do_read_vector(Word(SPI_WEIGHT_TUPLE,IntTag),Ref_Word(Channel)))
    return(False);
  Pi = KOutA;
  if (IsRef(Pi)) {
    deref_ref(Pi, Pa);
    if (IsTpl(Pi)) {
      heapP Pb;
      int Arity = Arity_of(Pi);
      if (Arity == 1)
	return(False);
      Pb = Pa += 2;
      deref_ptr(Pb);
      Pi = *Pb;		/* the weighter's index */
      for (/* argn = 0 */; argn < Arity-2; argn++) {
	Pb = ++Pa;
	deref_ptr(Pb);
	if (IsReal(*Pb))
	  argv[argn] = real_val((Pb+1));
	else if (IsInt(*Pb))
	  argv[argn] = Int_Val(*Pb);
	else if (IsVctr(*Pb)) {
	  if(!vctr_var_s(Pb, OBJECT_ARITY))
	    return False;
	  if (!do_read_vector(Word(OBJECT_VALUES,IntTag),Ref_Word(Pb)))
	    return(False);
	  deref(KOutA, Pb);
	  if (IsReal(*Pb))
	    argv[argn] = real_val((Pb+1));
	  else if (IsInt(*Pb))
	    argv[argn] = Int_Val(*Pb);
	  else
	    return False;
	}
	else
	  return False;
      }
    }
    KOutA = Pi;
  }
if (!IsInt(KOutA)) {
  /*fprintf(stderr, "*** KOutA = %x\n", KOutA);*/
}
  *argnr = argn;
  return True;
}
