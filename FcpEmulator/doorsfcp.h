/*
** This module is part of EFCP.
**

     Copyright 2007 Avraham Houri, Marilyn Safran
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


#define C_TH		1024*2

#define MaxFileDescriptors	(0xff+1)

#define DFCPE	100

#define DOORSFCP_NO_WORK_SPACE					(DFCPE+  1)
#define DOORSFCP_ADD_DOORS_WAKEUP_FILE				(DFCPE+  2)
#define DOORSFCP_REMOVE_DOORS_WAKEUP_FILE			(DFCPE+  3)

#define DOORSFCP_fromFcpTransBoolean				(DFCPE+ 11)
#define DOORSFCP_fromFcpTransFaceState				(DFCPE+ 12)
#define DOORSFCP_fromFcpTransConnectionType			(DFCPE+ 13)
#define DOORSFCP_fromFcpTransConversationType			(DFCPE+ 14)
#define DOORSFCP_fromFcpTransObjectType				(DFCPE+ 15)
#define DOORSFCP_fromFcpTransPositionType			(DFCPE+ 16)
#define DOORSFCP_fromFcpTransAnchorType				(DFCPE+ 17)
#define DOORSFCP_fromFcpTransHtmlAnchorType			(DFCPE+ 18)
#define DOORSFCP_fromFcpTransDestination			(DFCPE+ 19)
#define	DOORSFCP_fromFcpTransAudioOutputDevice			(DFCPE+ 20)
#define DOORSFCP_fromFcpTransChars				(DFCPE+ 21)
#define DOORSFCP_fromFcpTransString				(DFCPE+ 22)
#define DOORSFCP_fromFcpTransInteger				(DFCPE+ 23)
#define DOORSFCP_fromFcpTransLong				(DFCPE+ 24)
#define DOORSFCP_fromFcpTransUlong				(DFCPE+ 25)
#define DOORSFCP_fromFcpTransVersion				(DFCPE+ 26)
#define DOORSFCP_fromFcpTransNodeId				(DFCPE+ 27)
#define DOORSFCP_fromFcpTransPortId				(DFCPE+ 28)
#define DOORSFCP_fromFcpTransPlaceId				(DFCPE+ 29)
#define DOORSFCP_fromFcpTransMboneChannel			(DFCPE+ 30)
#define DOORSFCP_fromFcpTransPresenceId				(DFCPE+ 31)
#define DOORSFCP_fromFcpTransObjectId				(DFCPE+ 32)
#define DOORSFCP_fromFcpTransBytes				(DFCPE+ 33)
#define DOORSFCP_fromFcpTransFace				(DFCPE+ 34)
#define DOORSFCP_fromFcpTransDate				(DFCPE+ 35)
#define DOORSFCP_fromFcpTransDoor				(DFCPE+ 36)
#define DOORSFCP_fromFcpTransPresenceInfo			(DFCPE+ 37)
#define DOORSFCP_fromFcpTransHtmlAnchor				(DFCPE+ 38)
#define DOORSFCP_fromFcpTransPresenceAnchor			(DFCPE+ 39)
#define DOORSFCP_fromFcpTransRelativeAnchor			(DFCPE+ 40)
#define DOORSFCP_fromFcpTransAbsoluteOffset			(DFCPE+ 41)
#define DOORSFCP_fromFcpTransProportionalOffset			(DFCPE+ 42)
#define DOORSFCP_fromFcpTransPosition				(DFCPE+ 43)
#define DOORSFCP_fromFcpTransObjectState			(DFCPE+ 44)
#define DOORSFCP_fromFcpTransObjectInPresence			(DFCPE+ 45)
#define DOORSFCP_fromFcpTransConnection				(DFCPE+ 46)
#define DOORSFCP_fromFcpTransPresenceState			(DFCPE+ 47)
#define DOORSFCP_fromFcpTransPlaceState				(DFCPE+ 48)
#define DOORSFCP_fromFcpTransPresenceList			(DFCPE+ 49)
#define DOORSFCP_fromFcpTransHeader				(DFCPE+ 50)
#define DOORSFCP_fromFcpTransClientCapabilities			(DFCPE+ 51)
#define DOORSFCP_fromFcpTransClubTicketCapabilities		(DFCPE+ 52)

#define DOORSFCP_fromFcpTransErrorVal				(DFCPE+ 53)
#define DOORSFCP_fromFcpTransWarning				(DFCPE+ 54)

#define DOORSFCP_fromFcpTransErrorCategory			(DFCPE+ 55)
#define DOORSFCP_fromFcpTransPlaceConnectErrorType		(DFCPE+ 56)
#define DOORSFCP_fromFcpTransInterfaceErrorType			(DFCPE+ 57)
#define DOORSFCP_fromFcpTransAudioErrorType			(DFCPE+ 58)
#define DOORSFCP_fromFcpTransErrorSeverity			(DFCPE+ 59)
#define DOORSFCP_fromFcpTransInterfaceError			(DFCPE+ 60)
#define DOORSFCP_fromFcpTransError				(DFCPE+ 61)

#define DOORSFCP_fromFcpTransEventCategory			(DFCPE+ 62)
#define DOORSFCP_fromFcpTransPresenceEventType			(DFCPE+ 63)
#define DOORSFCP_fromFcpTransPlaceEventType			(DFCPE+ 64)
#define DOORSFCP_fromFcpTransTransientEventType			(DFCPE+ 65)
#define DOORSFCP_fromFcpTransAlertEventType			(DFCPE+ 66)
#define DOORSFCP_fromFcpTransMessageEventType			(DFCPE+ 67)
#define DOORSFCP_fromFcpTransOtherEventType			(DFCPE+ 68)
#define DOORSFCP_fromFcpTransAudioEventType			(DFCPE+ 69)
#define DOORSFCP_fromFcpTransCreateObjectFailure		(DFCPE+ 70)
#define DOORSFCP_fromFcpTransConnectFailure			(DFCPE+ 71)
#define DOORSFCP_fromFcpTransPresenceEvent			(DFCPE+ 72)
#define DOORSFCP_fromFcpTransPlaceEvent				(DFCPE+ 73)
#define DOORSFCP_fromFcpTransTransientEvent			(DFCPE+ 74)
#define DOORSFCP_fromFcpTransAlertEvent				(DFCPE+ 75)
#define DOORSFCP_fromFcpTransMessageEvent			(DFCPE+ 76)
#define DOORSFCP_fromFcpTransOtherEvent				(DFCPE+ 77)
#define DOORSFCP_fromFcpTransAudioEvent				(DFCPE+ 78)
#define DOORSFCP_fromFcpTransEvent				(DFCPE+ 79)

#define DOORSFCP_fromFcpTransBusinessCard			(DFCPE+ 80)

#define DOORSFCP_fromFcpTransRequestCategory			(DFCPE+ 81)
#define DOORSFCP_fromFcpTransUserRequestType			(DFCPE+ 82)
#define DOORSFCP_fromFcpTransPlaceRequestType			(DFCPE+ 83)
#define DOORSFCP_fromFcpTransServerRequestType			(DFCPE+ 84)
#define DOORSFCP_fromFcpTransUserObjectList			(DFCPE+ 85)
#define DOORSFCP_fromFcpTransEventCategoryList			(DFCPE+ 86)
#define DOORSFCP_fromFcpTransUserRequest			(DFCPE+ 87)
#define DOORSFCP_fromFcpTransPlaceRequest			(DFCPE+ 88)
#define DOORSFCP_fromFcpTransServerRequest			(DFCPE+ 89)
#define DOORSFCP_fromFcpTransRequest				(DFCPE+ 90)
#define DOORSFCP_fromFcpTransResponseCategory			(DFCPE+ 91)
#define DOORSFCP_fromFcpTransUserResponseType			(DFCPE+ 92)
#define DOORSFCP_fromFcpTransPlaceResponseType			(DFCPE+ 93)
#define DOORSFCP_fromFcpTransServerResponseType			(DFCPE+ 94)
#define DOORSFCP_fromFcpTransPlacePresenceList			(DFCPE+ 95)
#define DOORSFCP_fromFcpTransUserDataList			(DFCPE+ 96)
#define DOORSFCP_fromFcpTransUserFaceList			(DFCPE+ 97)
#define DOORSFCP_fromFcpTransUserAllList			(DFCPE+ 98)
#define DOORSFCP_fromFcpTransPresenceStateList			(DFCPE+ 99)
#define DOORSFCP_fromFcpTransPlaceSnapShot			(DFCPE+100)
#define DOORSFCP_fromFcpTransEventList				(DFCPE+101)
#define DOORSFCP_fromFcpTransUserResponse			(DFCPE+102)
#define DOORSFCP_fromFcpTransPlaceResponse			(DFCPE+103)
#define DOORSFCP_fromFcpTransServerResponse			(DFCPE+104)
#define DOORSFCP_fromFcpTransResponse				(DFCPE+105)

#define DOORSFCP_toFcpTransBoolean				(DFCPE+211)
#define DOORSFCP_toFcpTransFaceState				(DFCPE+212)
#define DOORSFCP_toFcpTransConnectionType			(DFCPE+213)
#define DOORSFCP_toFcpTransConversationType			(DFCPE+214)
#define DOORSFCP_toFcpTransObjectType				(DFCPE+215)
#define DOORSFCP_toFcpTransPositionType				(DFCPE+216)
#define DOORSFCP_toFcpTransAnchorType				(DFCPE+217)
#define DOORSFCP_toFcpTransHtmlAnchorType			(DFCPE+218)
#define DOORSFCP_toFcpTransDestination				(DFCPE+219)
#define	DOORSFCP_toFcpTransAudioOutputDevice			(DFCPE+220)
#define DOORSFCP_toFcpTransChars				(DFCPE+221)
#define DOORSFCP_toFcpTransString				(DFCPE+222)
#define DOORSFCP_toFcpTransInteger				(DFCPE+223)
#define DOORSFCP_toFcpTransLong					(DFCPE+224)
#define DOORSFCP_toFcpTransUlong				(DFCPE+225)
#define DOORSFCP_toFcpTransVersion				(DFCPE+226)
#define DOORSFCP_toFcpTransNodeId				(DFCPE+227)
#define DOORSFCP_toFcpTransPortId				(DFCPE+228)
#define DOORSFCP_toFcpTransPlaceId				(DFCPE+229)
#define DOORSFCP_toFcpTransMboneChannel				(DFCPE+230)
#define DOORSFCP_toFcpTransPresenceId				(DFCPE+231)
#define DOORSFCP_toFcpTransObjectId				(DFCPE+232)
#define DOORSFCP_toFcpTransBytes				(DFCPE+233)
#define DOORSFCP_toFcpTransFace					(DFCPE+234)
#define DOORSFCP_toFcpTransDate					(DFCPE+235)
#define DOORSFCP_toFcpTransDoor					(DFCPE+236)
#define DOORSFCP_toFcpTransPresenceInfo				(DFCPE+237)
#define DOORSFCP_toFcpTransHtmlAnchor				(DFCPE+238)
#define DOORSFCP_toFcpTransPresenceAnchor			(DFCPE+239)
#define DOORSFCP_toFcpTransRelativeAnchor			(DFCPE+240)
#define DOORSFCP_toFcpTransAbsoluteOffset			(DFCPE+241)
#define DOORSFCP_toFcpTransProportionalOffset			(DFCPE+242)
#define DOORSFCP_toFcpTransPosition				(DFCPE+243)
#define DOORSFCP_toFcpTransObjectState				(DFCPE+244)
#define DOORSFCP_toFcpTransObjectInPresence			(DFCPE+245)
#define DOORSFCP_toFcpTransConnection				(DFCPE+246)
#define DOORSFCP_toFcpTransPresenceState			(DFCPE+247)
#define DOORSFCP_toFcpTransPlaceState				(DFCPE+248)
#define DOORSFCP_toFcpTransPresenceList				(DFCPE+249)
#define DOORSFCP_toFcpTransHeader				(DFCPE+250)
#define DOORSFCP_toFcpTransClientCapabilities			(DFCPE+251)
#define DOORSFCP_toFcpTransClubTicketCapabilities		(DFCPE+252)

#define DOORSFCP_toFcpTransErrorVal				(DFCPE+253)
#define DOORSFCP_toFcpTransWarning				(DFCPE+254)

#define DOORSFCP_toFcpTransErrorCategory			(DFCPE+255)
#define DOORSFCP_toFcpTransPlaceConnectErrorType		(DFCPE+256)
#define DOORSFCP_toFcpTransInterfaceErrorType			(DFCPE+257)
#define DOORSFCP_toFcpTransAudioErrorType			(DFCPE+258)
#define DOORSFCP_toFcpTransErrorSeverity			(DFCPE+259)
#define DOORSFCP_toFcpTransInterfaceError			(DFCPE+260)
#define DOORSFCP_toFcpTransError				(DFCPE+261)

#define DOORSFCP_toFcpTransEventCategory			(DFCPE+262)
#define DOORSFCP_toFcpTransPresenceEventType			(DFCPE+263)
#define DOORSFCP_toFcpTransPlaceEventType			(DFCPE+264)
#define DOORSFCP_toFcpTransTransientEventType			(DFCPE+265)
#define DOORSFCP_toFcpTransAlertEventType			(DFCPE+266)
#define DOORSFCP_toFcpTransMessageEventType			(DFCPE+267)
#define DOORSFCP_toFcpTransOtherEventType			(DFCPE+268)
#define DOORSFCP_toFcpTransAudioEventType			(DFCPE+269)
#define DOORSFCP_toFcpTransCreateObjectFailure			(DFCPE+270)
#define DOORSFCP_toFcpTransConnectFailure			(DFCPE+271)
#define DOORSFCP_toFcpTransPresenceEvent			(DFCPE+272)
#define DOORSFCP_toFcpTransPlaceEvent				(DFCPE+273)
#define DOORSFCP_toFcpTransTransientEvent			(DFCPE+274)
#define DOORSFCP_toFcpTransAlertEvent				(DFCPE+275)
#define DOORSFCP_toFcpTransMessageEvent				(DFCPE+276)
#define DOORSFCP_toFcpTransOtherEvent				(DFCPE+277)
#define DOORSFCP_toFcpTransAudioEvent				(DFCPE+278)
#define DOORSFCP_toFcpTransEvent				(DFCPE+279)

#define DOORSFCP_toFcpTransBusinessCard				(DFCPE+280)

#define DOORSFCP_toFcpTransRequestCategory			(DFCPE+281)
#define DOORSFCP_toFcpTransUserRequestType			(DFCPE+282)
#define DOORSFCP_toFcpTransPlaceRequestType			(DFCPE+283)
#define DOORSFCP_toFcpTransServerRequestType			(DFCPE+284)
#define DOORSFCP_toFcpTransUserObjectList			(DFCPE+285)
#define DOORSFCP_toFcpTransEventCategoryList			(DFCPE+286)
#define DOORSFCP_toFcpTransUserRequest				(DFCPE+287)
#define DOORSFCP_toFcpTransPlaceRequest				(DFCPE+288)
#define DOORSFCP_toFcpTransServerRequest			(DFCPE+289)
#define DOORSFCP_toFcpTransRequest				(DFCPE+290)
#define DOORSFCP_toFcpTransResponseCategory			(DFCPE+291)
#define DOORSFCP_toFcpTransUserResponseType			(DFCPE+292)
#define DOORSFCP_toFcpTransPlaceResponseType			(DFCPE+293)
#define DOORSFCP_toFcpTransServerResponseType			(DFCPE+294)
#define DOORSFCP_toFcpTransPlacePresenceList			(DFCPE+295)
#define DOORSFCP_toFcpTransUserDataList				(DFCPE+296)
#define DOORSFCP_toFcpTransUserFaceList				(DFCPE+297)
#define DOORSFCP_toFcpTransUserAllList				(DFCPE+298)
#define DOORSFCP_toFcpTransPresenceStateList			(DFCPE+299)
#define DOORSFCP_toFcpTransPlaceSnapShot			(DFCPE+300)
#define DOORSFCP_toFcpTransEventList				(DFCPE+301)
#define DOORSFCP_toFcpTransUserResponse				(DFCPE+302)
#define DOORSFCP_toFcpTransPlaceResponse			(DFCPE+303)
#define DOORSFCP_toFcpTransServerResponse			(DFCPE+304)
#define DOORSFCP_toFcpTransResponse				(DFCPE+305)

#define doorsfcp_start() \
{ \
  AllocP = (int *) OtherHeap; \
  EndAllocP = (int *) (OtherHeapEnd - C_TH); \
  DoorsFcpError = 0; \
  HeapSpaceError = False; \
}

#define	doorsfcp_return(ReturnValue) \
{ \
  return((ReturnValue && (HeapSpaceError == False))); \
}

#define call_fromFcpTrans_routine(RoutineAndArgs, ErrorValVar) \
{ \
  if (!RoutineAndArgs) { \
    if (DoorsFcpError != 0) { \
      asgn(*ErrorValVar, ErrorValVar, Word(DoorsFcpError, IntTag)); \
      return(True); \
    } \
    return(False); \
  } \
}

#define doorsfcp_check_fcp_memory(Size) \
{ \
  if (ended_heap((HP+Size))) { \
    HeapSpaceError = True; \
    return(False); \
  } \
}

#define check_error_variable(ErrorVal, OKVal, Functor, FileDescriptor) \
{ \
  if (ErrorVal != OKVal) { \
    register heapP T = HP; \
\
    HP += 3; \
\
    *(T+0) = Word(2, TplTag); \
    *(T+1) = Ref_Word(produce_string(Functor)); \
    *(T+2) = Word(ErrorVal, IntTag); \
\
    add_to_device_stream(ReadC, FileDescriptor, T); \
  } \
}

#define drsfcptc_allocate(Size, Cast, Ptr) \
{ \
  register unsigned int Units = \
    (((Size % sizeof(*AllocP)) == 0) ? \
     (Size/sizeof(*AllocP)) : ((Size/sizeof(*AllocP)) + 1)); \
\
  if ((AllocP+Units) >= EndAllocP) { \
    DoorsFcpError = DOORSFCP_NO_WORK_SPACE; \
    return(False); \
  } \
  Ptr = Cast AllocP; \
  AllocP += Units; \
}


