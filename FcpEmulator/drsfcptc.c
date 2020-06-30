/*
** This module is part of EFCP.
**

     Copyright 2007 Avshalom Houri, Marilyn Safran
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

#include <string.h>

#include "doors.h"

#include "fcp.h"
#include "codes.h"
#include "global.h"
#include "macros.h"

#include "doorsfcp.h"
#include "doorsvar.h"

/* doorsTyp.h - start */

int fromFcpTransBoolean(Boolean, CBoolean)
     heapP Boolean;
     DoorsBoolean *CBoolean;
{
  register heapP P = Boolean;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransBoolean;
    }
    return(False);
  }

  {
    DoorsBoolean E = (DoorsBoolean) Int_Val(*P);

    switch (E) {
    case DOORS_FALSE:
    case DOORS_TRUE:
      *CBoolean = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransBoolean;
      return(False);
    }
  }
}

int fromFcpTransFaceState(FaceState, CFaceState)
     heapP FaceState;
     DoorsFaceState *CFaceState;
{
  register heapP P = FaceState;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransFaceState;
    }
    return(False);
  }

  {
    DoorsFaceState E = (DoorsFaceState) Int_Val(*P);

    switch (E) {
    case DOORS_FACE_PRESENT:
    case DOORS_FACE_CONVERSING:
    case DOORS_FACE_ICONIZED:
    case DOORS_FACE_DISABLED:
      *CFaceState = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransFaceState;
      return(False);
    }
  }
}

int fromFcpTransConnectionType(ConnectionType, CConnectionType)
     heapP ConnectionType;
     DoorsConnectionType *CConnectionType;
{
  register heapP P = ConnectionType;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransConnectionType;
    }
    return(False);
  }

  {
    DoorsConnectionType E = (DoorsConnectionType) Int_Val(*P);

    switch (E) {
    case DOORS_CONNECTION_NONE:
    case DOORS_CONNECTION_TO_OBJECT:
    case DOORS_CONNECTION_TO_PRESENCE:
      *CConnectionType = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransConnectionType;
      return(False);
    }
  }
}

int fromFcpTransConversationType(ConversationType, CConversationType)
     heapP ConversationType;
     DoorsConversationType *CConversationType;
{
  register heapP P = ConversationType;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransConversationType;
    }
    return(False);
  }

  {
    DoorsConversationType E = (DoorsConversationType) Int_Val(*P);

    switch (E) {
    case DOORS_CONVERSATION_NONE:
    case DOORS_CONVERSATION_CONFERENCE:
    case DOORS_CONVERSATION_LECTURE:
      *CConversationType = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransConversationType;
      return(False);
    }
  }
}

int fromFcpTransObjectType(ObjectType, CObjectType)
     heapP ObjectType;
     DoorsObjectType *CObjectType;
{
  register heapP P = ObjectType;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransObjectType;
    }
    return(False);
  }

  {
    DoorsObjectType E = (DoorsObjectType) Int_Val(*P);

    switch (E) {
    case DOORS_OBJECT_TRANSPARENT_BUS:
    case DOORS_OBJECT_OPAQUE_BUS:
      *CObjectType = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransObjectType;
      return(False);
    }
  }
}

int fromFcpTransPositionType(PositionType, CPositionType)
     heapP PositionType;
     DoorsPositionType *CPositionType;
{
  register heapP P = PositionType;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransPositionType;
    }
    return(False);
  }

  {
    DoorsPositionType E = (DoorsPositionType) Int_Val(*P);

    switch (E) {
    case DOORS_POSITION_NONE:
    case DOORS_POSITION_DOCUMENT:
    case DOORS_POSITION_GALLERY:
      *CPositionType = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransPositionType;
      return(False);
    }
  }
}

int fromFcpTransAnchorType(AnchorType, CAnchorType)
     heapP AnchorType;
     DoorsAnchorType *CAnchorType;
{
  register heapP P = AnchorType;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransAnchorType;
    }
    return(False);
  }

  {
    DoorsAnchorType E = (DoorsAnchorType) Int_Val(*P);

    switch (E) {
    case DOORS_ANCHOR_HTML:
    case DOORS_ANCHOR_RELATIVE:
    case DOORS_ANCHOR_PRESENCE:
      *CAnchorType = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransAnchorType;
      return(False);
    }
  }
}

int fromFcpTransHtmlAnchorType(HtmlAnchorType, CHtmlAnchorType)
     heapP HtmlAnchorType;
     DoorsHtmlAnchorType *CHtmlAnchorType;
{
  register heapP P = HtmlAnchorType;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransHtmlAnchorType;
    }
    return(False);
  }

  {
    DoorsHtmlAnchorType E = (DoorsHtmlAnchorType) Int_Val(*P);

    switch (E) {
    case DOORS_HTML_ANCHOR_IMAGE:
    case DOORS_HTML_ANCHOR_CHAR:
      *CHtmlAnchorType = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransHtmlAnchorType;
      return(False);
    }
  }
}

int fromFcpTransDestination(Destination, CDestination)
     heapP Destination;
     DoorsDestination *CDestination;
{
  register heapP P = Destination;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransDestination;
    }
    return(False);
  }

  {
    DoorsDestination D = Int_Val(*P);

    switch (D) {
    case DOORS_SERVER:
    case DOORS_PLACE:
    case DOORS_CONVERSATION:
    case DOORS_LIST:
      *CDestination = D;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransDestination;
      return(False);
    }
  }
}

int fromFcpTransAudioOutputDevice(AudioOutputDevice, CAudioOutputDevice)
     heapP AudioOutputDevice;
     DoorsAudioOutputDevice *CAudioOutputDevice;
{
  register heapP P = AudioOutputDevice;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransAudioOutputDevice;
    }
    return(False);
  }

  {
    DoorsAudioOutputDevice A = Int_Val(*P);

    switch (A) {
    case DOORS_LINE:
    case DOORS_HEADPHONE:
    case DOORS_DIGITAL:
    case DOORS_SPEAKER:
      *CAudioOutputDevice = A;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransAudioOutputDevice;
      return(False);
    }
  }
}

int fromFcpTransChars(Chars, CChars)
     heapP Chars;
     DoorsChars *CChars;
{
  register heapP P = Chars;
  
  deref_ptr(P);
  if (!(IsNil(*P) || IsStr(*P))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransChars;
    }
    return(False);
  }

  if (IsNil(*P)) {
    *CChars = (DoorsChars) Null;
    return(True);
  }

  *CChars = (DoorsChars) (P + StrHdrWords);
  return(True);
}

int fromFcpTransString(String, CString)
     heapP String;
     DoorsString *CString;
{
  register heapP P = String;
  
  deref_ptr(P);
  if (!(IsNil(*P) || IsStr(*P))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransString;
    }
    return(False);
  }

  if (IsNil(*P)) {
    *CString = (DoorsString) Null;
    return(True);
  }

  *CString = (DoorsString) (P + StrHdrWords);
  return(True);
}

int fromFcpTransInteger(Integer, CInteger)
     heapP Integer;
     DoorsInteger *CInteger;
{
  register heapP P = Integer;
  
  deref_ptr(P);
  if (!(IsInt(*P) || IsStr(*P))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransInteger;
    }
    return(False);
  }

  if (IsInt(*P)) {
    *CInteger = (DoorsInteger) Int_Val(*P);
  }
  else {
    register char *CharP = (char *) (P + StrHdrWords);
    int I;

    if (sscanf(CharP, "%d", &I) != 1) {
      return(False);
    }
    *CInteger = (DoorsInteger) I;
  }
  return(True);
}

int fromFcpTransLong(Long, CLong)
     heapP Long;
     DoorsLong *CLong;
{
  register heapP P = Long;
  
  deref_ptr(P);
  if (!(IsInt(*P) || IsStr(*P))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransLong;
    }
    return(False);
  }

  if (IsInt(*P)) {
    *CLong = (DoorsLong) Int_Val(*P);
  }
  else {
    register char *CharP = (char *) (P + StrHdrWords);
    unsigned long L;

    if (sscanf(CharP, "%ld", &L) != 1) {
      DoorsFcpError = DOORSFCP_fromFcpTransLong;
      return(False);
    }
    *CLong = (DoorsLong) L;
  }
  return(True);
}

int fromFcpTransUlong(Ulong, CUlong)
     heapP Ulong;
     DoorsUlong *CUlong;
{
  register heapP P = Ulong;
  
  deref_ptr(P);
  if (!(IsInt(*P) || IsStr(*P))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransUlong;
    }
    return(False);
  }

  if (IsInt(*P)) {
    *CUlong = (DoorsUlong) Int_Val(*P);
  }
  else {
    register char *CharP = (char *) (P + StrHdrWords);
    unsigned long UL;

    if (sscanf(CharP, "%lu", &UL) != 1) {
      DoorsFcpError = DOORSFCP_fromFcpTransUlong;
      return(False);
    }
    *CUlong = (DoorsUlong) UL;
  }
  return(True);
}

int fromFcpTransVersion(Version, CVersion)
     heapP Version;
     DoorsVersion *CVersion;
{
  register heapP P = Version;
  
  deref_ptr(P);
  if (!(IsNil(*P) || IsStr(*P))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransVersion;
    }
    return(False);
  }

  if (IsNil(*P)) {
    *CVersion = (DoorsVersion) Null;
    return(True);
  }

  *CVersion = (DoorsVersion) (P + StrHdrWords);
  return(True);
}

int fromFcpTransNodeId(NodeId, CNodeId)
     heapP NodeId;
     DoorsNodeId *CNodeId;
{
  register heapP P = NodeId;
  
  deref_ptr(P);
  if (!(IsNil(*P) || IsStr(*P))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransNodeId;
    }
    return(False);
  }

  if (IsNil(*P)) {
    *CNodeId = (DoorsNodeId) Null;
    return(True);
  }

  *CNodeId = (DoorsNodeId) (P + StrHdrWords);
  return(True);
}

int fromFcpTransPortId(PortId, CPortId)
     heapP PortId;
     DoorsPortId *CPortId;
{
  register heapP P = PortId;
  
  deref_ptr(P);
  if (!(IsNil(*P) || IsInt(*P) || IsStr(*P))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransPortId;
    }
    return(False);
  }

  if (IsNil(*P)) {
    *CPortId = (DoorsPortId) 0;
    return(True);
  }

  if (IsInt(*P)) {
    *CPortId = (DoorsPortId) Int_Val(*P);
  }
  else {
    register char *CharP = (char *) (P + StrHdrWords);
    unsigned long L;

    if (sscanf(CharP, "%ld", &L) != 1) {
      DoorsFcpError = DOORSFCP_fromFcpTransPortId;
      return(False);
    }
    *CPortId = (DoorsPortId) L;
  }
  return(True);
}

int fromFcpTransPlaceId(PlaceId, CPlaceId)
     heapP PlaceId;
     DoorsPlaceId *CPlaceId;
{
  register heapP P = PlaceId;
  
  deref_ptr(P);
  if (!(IsNil(*P) || IsStr(*P))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransPlaceId;
    }
    return(False);
  }

  if (IsNil(*P)) {
    *CPlaceId = (DoorsPlaceId) Null;
    return(True);
  }

  *CPlaceId = (DoorsPlaceId) (P + StrHdrWords);
  return(True);
}

int fromFcpTransMboneChannel(MboneChannel, CMboneChannel)
     heapP MboneChannel;
     DoorsMboneChannel *CMboneChannel;
{
  register heapP P = MboneChannel;
  
  deref_ptr(P);
  if (!(IsNil(*P) || IsStr(*P))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransMboneChannel;
    }
    return(False);
  }

  if (IsNil(*P)) {
    *CMboneChannel = (DoorsMboneChannel) Null;
    return(True);
  }

  *CMboneChannel = (DoorsMboneChannel) (P + StrHdrWords);
  return(True);
}

int fromFcpTransPresenceId(PresenceId, CPresenceId)
     heapP PresenceId;
     DoorsPresenceId *CPresenceId;
{
  register heapP P = PresenceId;
  
  deref_ptr(P);
  if (!(IsNil(*P) || IsStr(*P))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransPresenceId;
    }
    return(False);
  }

  if (IsNil(*P)) {
    *CPresenceId = (DoorsPresenceId) Null;
    return(True);
  }

  *CPresenceId = (DoorsPresenceId) (P + StrHdrWords);
  return(True);
}

int fromFcpTransObjectId(ObjectId, CObjectId)
     heapP ObjectId;
     DoorsObjectId *CObjectId;
{
  register heapP P = ObjectId;
  
  deref_ptr(P);
  if (!(IsNil(*P) || IsInt(*P) || IsStr(*P))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransObjectId;
    }
    return(False);
  }

  if (IsNil(*P)) {
    *CObjectId = (DoorsObjectId) 0;
    return(True);
  }

  if (IsInt(*P)) {
    *CObjectId = (DoorsObjectId) Int_Val(*P);
  }
  else {
    register char *CharP = (char *) (P + StrHdrWords);
    unsigned long UL;

    if (sscanf(CharP, "%lu", &UL) != 1) {
      DoorsFcpError = DOORSFCP_fromFcpTransObjectId;
      return(False);
    }
    *CObjectId = (DoorsObjectId) UL;
  }
  return(True);
}

int fromFcpTransBytes(Bytes, CBytes)
     heapP Bytes;
     DoorsBytes **CBytes;
{
  register heapP P = Bytes;
  register DoorsBytes *BytesP = *CBytes;
  
  deref_ptr(P);
  if (!(IsNil(*P) || IsStr(*P))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransBytes;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (BytesP == ((DoorsBytes *) Null)) {
      return(True);
    }
  }

  if (BytesP == ((DoorsBytes *) Null)) {
    drsfcptc_allocate(sizeof(DoorsBytes), (DoorsBytes *), BytesP);
  }

  BytesP->length = (DoorsUlong) Str_Length(P);
  BytesP->data = (DoorsChars) (P + StrHdrWords);

  *CBytes = BytesP;
  return(True);
}

int fromFcpTransFace(Face, CFace)
     heapP Face;
     DoorsFace **CFace;
{
  register heapP P = Face;
  register DoorsFace *FaceP = *CFace;
  
  deref_ptr(P);
  if (!(IsNil(*P) || IsStr(*P))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransFace;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (FaceP == ((DoorsFace *) Null)) {
      return(True);
    }
  }

  if (FaceP == ((DoorsFace *) Null)) {
    drsfcptc_allocate(sizeof(DoorsFace), (DoorsFace *), FaceP);
  }

  FaceP->length = (DoorsUlong) Str_Length(P);
  FaceP->data = (DoorsChars) (P + StrHdrWords);

  *CFace = FaceP;
  return(True);
}

int fromFcpTransDate(Date, CDate)
     heapP Date;
     DoorsDate **CDate;
{
  register heapP P;
  register DoorsDateP DateP = *CDate;

  P = Date;
  deref_ptr(P);
  if (!(IsNil(*P) || IsStr(*P))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransDate;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (DateP == ((DoorsDate *) Null)) {
      return(True);
    }
  }

  if (DateP == ((DoorsDate *) Null)) {
    drsfcptc_allocate(sizeof(DoorsDate), (DoorsDate *), DateP);
  }

  {
    char *CharP = (char *) (P + StrHdrWords);

    if (sscanf(CharP, "%ld.%ld", &(DateP->seconds), &(DateP->micro_seconds))
	!= 2) {
      DoorsFcpError = DOORSFCP_fromFcpTransDate;
      return(False);
    }
  }

  *CDate = DateP;
  return(True);
}

int fromFcpTransDoor(Door, CDoor)
     heapP Door;
     DoorsDoor **CDoor;
{
  register heapP P;
  register DoorsDoor *DoorP = *CDoor;

  P = Door;
  deref_ptr(P);
  if (!(IsNil(*P)  || (IsTpl(*P) && (Arity_of(*P) == 3)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransDoor;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (DoorP == ((DoorsDoor *) Null)) {
      return(True);
    }
  }

  if (DoorP == ((DoorsDoor *) Null)) {
    drsfcptc_allocate(sizeof(DoorsDoor), (DoorsDoor *), DoorP);
  }

  DoorP->server = ((DoorsNodeId) Null);
  if (!fromFcpTransNodeId((P+1), &(DoorP->server))) {
    return(False);
  }

  if (!fromFcpTransPortId((P+2), &(DoorP->port))) {
    return(False);
  }

  DoorP->place = ((DoorsPlaceId) Null);
  if (!fromFcpTransPlaceId((P+3), &(DoorP->place))) {
    return(False);
  }

  *CDoor = DoorP;
  return(True);
}

int fromFcpTransPresenceInfo(PresenceInfo, CPresenceInfo)
     heapP PresenceInfo;
     DoorsPresenceInfo **CPresenceInfo;
{
  register heapP P = PresenceInfo;
  register DoorsPresenceInfo *PresenceInfoP = *CPresenceInfo;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 5)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransPresenceInfo;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (PresenceInfoP == ((DoorsPresenceInfo *) Null)) {
      return(True);
    }
  }

  if (PresenceInfoP == ((DoorsPresenceInfo *) Null)) {
    drsfcptc_allocate(sizeof(DoorsPresenceInfo), (DoorsPresenceInfo *),
		      PresenceInfoP);
  }
  
  PresenceInfoP->login_name = ((DoorsString) Null);
  if (!fromFcpTransString((P+1), &(PresenceInfoP->login_name))) {
    return(False);
  }

  PresenceInfoP->domain = ((DoorsString) Null);
  if (!fromFcpTransString((P+2), &(PresenceInfoP->domain))) {
    return(False);
  }

  PresenceInfoP->client_node = ((DoorsString) Null);
  if (!fromFcpTransString((P+3), &(PresenceInfoP->client_node))) {
    return(False);
  }

  if (!fromFcpTransUlong((P+4), &(PresenceInfoP->client_id))) {
    return(False);
  }

  {
    DoorsDateP DateP = (DoorsDate *) &(PresenceInfoP->presence_date);

    if (!fromFcpTransDate((P+5), &(DateP))) {
      return(False);
    }
  }

  *CPresenceInfo = PresenceInfoP;
  return(True);
}

int fromFcpTransHtmlAnchor(HtmlAnchor, CHtmlAnchor)
     heapP HtmlAnchor;
     DoorsHtmlAnchor **CHtmlAnchor;
{
  register heapP P = HtmlAnchor;
  register DoorsHtmlAnchor *HtmlAnchorP = *CHtmlAnchor;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 6)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransHtmlAnchor;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (HtmlAnchorP == ((DoorsHtmlAnchor *) Null)) {
      return(True);
    }
  }

  if (HtmlAnchorP == ((DoorsHtmlAnchor *) Null)) {
    drsfcptc_allocate(sizeof(DoorsHtmlAnchor), (DoorsHtmlAnchor *),
		      HtmlAnchorP);
  }
  
  if (!fromFcpTransHtmlAnchorType((P+1), &(HtmlAnchorP->type))) {
    return(False);
  }

  if (!fromFcpTransInteger((P+2), &(HtmlAnchorP->number))) {
    return(False);
  }

  if (!fromFcpTransBoolean((P+3), &(HtmlAnchorP->inside))) {
    return(False);
  }

  {
    DoorsProportionalOffset *ProportionalOffsetP =
      (DoorsProportionalOffset *) &(HtmlAnchorP->prop_offset);

    if (!fromFcpTransProportionalOffset((P+4), &(ProportionalOffsetP))) {
      return(False);
    }
  }

  {
    DoorsAbsoluteOffset *AbsoluteOffsetP =
      (DoorsAbsoluteOffset *) &(HtmlAnchorP->abs_offset);

    if (!fromFcpTransAbsoluteOffset((P+5), &(AbsoluteOffsetP))) {
      return(False);
    }
  }

  {
    DoorsProportionalOffset *ProportionalOffsetP =
      (DoorsProportionalOffset *) &(HtmlAnchorP->rel_offset);

    if (!fromFcpTransProportionalOffset((P+6), &(ProportionalOffsetP))) {
      return(False);
    }
  }

  *CHtmlAnchor = HtmlAnchorP;
  return(True);
}

int fromFcpTransPresenceAnchor(PresenceAnchor, CPresenceAnchor)
     heapP PresenceAnchor;
     DoorsPresenceAnchor **CPresenceAnchor;
{
  register heapP P = PresenceAnchor;
  register DoorsPresenceAnchor *PresenceAnchorP = *CPresenceAnchor;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 4)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransPresenceAnchor;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (PresenceAnchorP == ((DoorsPresenceAnchor *) Null)) {
      return(True);
    }
  }

  if (PresenceAnchorP == ((DoorsPresenceAnchor *) Null)) {
    drsfcptc_allocate(sizeof(DoorsPresenceAnchor), (DoorsPresenceAnchor *),
		      PresenceAnchorP);
  }
  
  PresenceAnchorP->presence_id = ((DoorsPresenceId) Null);
  if (!fromFcpTransPresenceId((P+1), &(PresenceAnchorP->presence_id))) {
    return(False);
  }

  if (!fromFcpTransObjectId((P+2), &(PresenceAnchorP->object_id))) {
    return(False);
  }

  {
    DoorsProportionalOffset *ProportionalOffsetP =
      (DoorsProportionalOffset *) &(PresenceAnchorP->prop_offset);

    if (!fromFcpTransProportionalOffset((P+3), &(ProportionalOffsetP))) {
      return(False);
    }
  }

  {
    DoorsAbsoluteOffset *AbsoluteOffsetP =
      (DoorsAbsoluteOffset *) &(PresenceAnchorP->abs_offset);

    if (!fromFcpTransAbsoluteOffset((P+4), &(AbsoluteOffsetP))) {
      return(False);
    }
  }

  *CPresenceAnchor = PresenceAnchorP;
  return(True);
}

int fromFcpTransRelativeAnchor(RelativeAnchor, CRelativeAnchor)
     heapP RelativeAnchor;
     DoorsRelativeAnchor **CRelativeAnchor;
{
  register heapP P = RelativeAnchor;
  register DoorsRelativeAnchor *RelativeAnchorP = *CRelativeAnchor;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 2)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransRelativeAnchor;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (RelativeAnchorP == ((DoorsRelativeAnchor *) Null)) {
      return(True);
    }
  }

  if (RelativeAnchorP == ((DoorsRelativeAnchor *) Null)) {
    drsfcptc_allocate(sizeof(DoorsRelativeAnchor), (DoorsRelativeAnchor *),
		      RelativeAnchorP);
  }
  
  {
    DoorsProportionalOffset *ProportionalOffsetP =
      (DoorsProportionalOffset *) &(RelativeAnchorP->rel_offset);

    if (!fromFcpTransProportionalOffset((P+1), &(ProportionalOffsetP))) {
      return(False);
    }
  }

  {
    DoorsAbsoluteOffset *AbsoluteOffsetP =
      (DoorsAbsoluteOffset *) &(RelativeAnchorP->abs_offset);

    if (!fromFcpTransAbsoluteOffset((P+2), &(AbsoluteOffsetP))) {
      return(False);
    }
  }

  *CRelativeAnchor = RelativeAnchorP;
  return(True);
}

int fromFcpTransAbsoluteOffset(AbsoluteOffset, CAbsoluteOffset)
     heapP AbsoluteOffset;
     DoorsAbsoluteOffset **CAbsoluteOffset;
{
  register heapP P = AbsoluteOffset;
  register DoorsAbsoluteOffset *AbsoluteOffsetP = *CAbsoluteOffset;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 3)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransAbsoluteOffset;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (AbsoluteOffsetP == ((DoorsAbsoluteOffset *) Null)) {
      return(True);
    }
  }

  if (AbsoluteOffsetP == ((DoorsAbsoluteOffset *) Null)) {
    drsfcptc_allocate(sizeof(DoorsAbsoluteOffset), (DoorsAbsoluteOffset *),
		      AbsoluteOffsetP);
  }
  
  if (!fromFcpTransUlong((P+1), &(AbsoluteOffsetP->x))) {
    return(False);
  }

  if (!fromFcpTransUlong((P+2), &(AbsoluteOffsetP->y))) {
    return(False);
  }

  if (!fromFcpTransUlong((P+3), &(AbsoluteOffsetP->z))) {
    return(False);
  }

  *CAbsoluteOffset = AbsoluteOffsetP;
  return(True);
}

int fromFcpTransProportionalOffset(ProportionalOffset, CProportionalOffset)
     heapP ProportionalOffset;
     DoorsProportionalOffset **CProportionalOffset;
{
  register heapP P = ProportionalOffset;
  register DoorsProportionalOffset *ProportionalOffsetP = *CProportionalOffset;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 3)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransProportionalOffset;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (ProportionalOffsetP == ((DoorsProportionalOffset *) Null)) {
      return(True);
    }
  }

  if (ProportionalOffsetP == ((DoorsProportionalOffset *) Null)) {
    drsfcptc_allocate(sizeof(DoorsProportionalOffset),
		      (DoorsProportionalOffset *), ProportionalOffsetP);
  }
  
  if (!fromFcpTransUlong((P+1), &(ProportionalOffsetP->x))) {
    return(False);
  }

  if (!fromFcpTransUlong((P+2), &(ProportionalOffsetP->y))) {
    return(False);
  }

  if (!fromFcpTransUlong((P+3), &(ProportionalOffsetP->z))) {
    return(False);
  }

  *CProportionalOffset = ProportionalOffsetP;
  return(True);
}

int fromFcpTransPosition(Position, CPosition)
     heapP Position;
     DoorsPosition **CPosition;
{
  register heapP P = Position;
  register DoorsPosition *PositionP = *CPosition;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 4)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransPosition;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (PositionP == ((DoorsPosition *) Null)) {
      return(True);
    }
  }

  if (PositionP == ((DoorsPosition *) Null)) {
    drsfcptc_allocate(sizeof(DoorsPosition),
		      (DoorsPosition *), PositionP);
  }
  
  if (!fromFcpTransPositionType((P+1), &(PositionP->position_type))) {
    return(False);
  }

  if (!fromFcpTransAnchorType((P+2), &(PositionP->anchor_type))) {
    return(False);
  }

  PositionP->door = ((DoorsDoor *) Null);
  if (!fromFcpTransDoor((P+3), &(PositionP->door))) {
    return(False);
  }

  switch(PositionP->anchor_type) {
  case DOORS_ANCHOR_HTML:
    {
      DoorsHtmlAnchor *HtmlAnchorP =
	(DoorsHtmlAnchor *) &(PositionP->anchor.html);

      if (!fromFcpTransHtmlAnchor((P+4), &(HtmlAnchorP))) {
	return(False);
      }
    }
    break;
  case DOORS_ANCHOR_PRESENCE:
    {
      DoorsPresenceAnchor *PresenceAnchorP = (DoorsPresenceAnchor *)
	&(PositionP->anchor.presence);

      if (!fromFcpTransPresenceAnchor((P+4), &(PresenceAnchorP))) {
	return(False);
      }
    }
    break;
  case DOORS_ANCHOR_RELATIVE:
    {
      DoorsRelativeAnchor *RelativeAnchorP = (DoorsRelativeAnchor *)
	&(PositionP->anchor.presence);

      if (!fromFcpTransRelativeAnchor((P+4), &(RelativeAnchorP))) {
	return(False);
      }
    }
    break;
  }

  *CPosition = PositionP;
  return(True);
}

int fromFcpTransObjectState(ObjectState, CObjectState)
     heapP ObjectState;
     DoorsObjectState **CObjectState;
{
  register heapP P = ObjectState;
  register DoorsObjectState *ObjectStateP = *CObjectState;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 9)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransObjectState;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (ObjectStateP == ((DoorsObjectState *) Null)) {
      return(True);
    }
  }

  if (ObjectStateP == ((DoorsObjectState *) Null)) {
    drsfcptc_allocate(sizeof(DoorsObjectState), (DoorsObjectState *),
		      ObjectStateP);
  }
  
  if (!fromFcpTransObjectType((P+1), &(ObjectStateP->type))) {
    return(False);
  }

  ObjectStateP->name = ((DoorsString) Null);
  if (!fromFcpTransString((P+2), &(ObjectStateP->name))) {
    return(False);
  }

  ObjectStateP->position = ((DoorsPosition *) Null);
  if (!fromFcpTransPosition((P+3), &(ObjectStateP->position))) {
    return(False);
  }

  ObjectStateP->date = ((DoorsDate *) Null);
  if (!fromFcpTransDate((P+4), &(ObjectStateP->date))) {
    return(False);
  }

  if (!fromFcpTransBoolean((P+5), &(ObjectStateP->has_icon))) {
    return(False);
  }

  if (!fromFcpTransConversationType((P+6), &(ObjectStateP->conversation))) {
    return(False);
  }

  if (!fromFcpTransInteger((P+7), &(ObjectStateP->max_capacity))) {
    return(False);
  }

  if (!fromFcpTransInteger((P+8), &(ObjectStateP->number_used))) {
    return(False);
  }

  if (!fromFcpTransMboneChannel((P+9), &(ObjectStateP->mbone_channel))) {
    return(False);
  }

  *CObjectState = ObjectStateP;
  return(True);
}

int fromFcpTransObjectInPresence(ObjectInPresence, CObjectInPresence)
     heapP ObjectInPresence;
     DoorsObjectInPresence **CObjectInPresence;
{
  register heapP P = ObjectInPresence;
  DoorsObjectInPresence *FirstObjectInPresenceP =
    ((DoorsObjectInPresence *) Null);
  DoorsObjectInPresence *LastObjectInPresenceP =
    ((DoorsObjectInPresence *) Null);
  register DoorsObjectInPresence *ObjectInPresenceP = *CObjectInPresence;

  P = ObjectInPresence;
  deref_ptr(P);
  if (!(IsNil(*P) || IsList(*P))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransObjectInPresence;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (ObjectInPresenceP == ((DoorsObjectInPresence *) Null)) {
      return(True);
    }
  }

  while (IsList(*P)) {
    if (ObjectInPresenceP == ((DoorsObjectInPresence *) Null)) {
      drsfcptc_allocate(sizeof(DoorsObjectInPresence),
			(DoorsObjectInPresence *), ObjectInPresenceP);
      ObjectInPresenceP->next = ((DoorsObjectInPresence *) Null);
    }

    {
      register heapP Pa = P;
      register heapT Va = Off_List(*Pa);

      deref(Va, Pa);
      if (!(IsTpl(Va) && (Arity_of(Va) == 2))) {
	if (IsVar(Va)) {
	  sus_tbl_add(Pa);
	}
	else {
	  DoorsFcpError = DOORSFCP_fromFcpTransObjectInPresence;
	}
	return(False);
      }

      if (!fromFcpTransObjectId((Pa+1), &(ObjectInPresenceP->object_id))) {
	return(False);
      }

      ObjectInPresenceP->object_state = ((DoorsObjectState *) Null);
      if (!fromFcpTransObjectState((Pa+2),
				   &(ObjectInPresenceP->object_state))) {
	return(False);
      }
    }

    if (FirstObjectInPresenceP == ((DoorsObjectInPresence *) Null)) {
      FirstObjectInPresenceP = ObjectInPresenceP;
    }
    if (LastObjectInPresenceP == ((DoorsObjectInPresence *) Null)) {
      LastObjectInPresenceP = ObjectInPresenceP;
    }
    else {
      LastObjectInPresenceP->next = ObjectInPresenceP;
      LastObjectInPresenceP = ObjectInPresenceP;
    }
    ObjectInPresenceP = ObjectInPresenceP->next;

    P = Cdr(P);
    deref_ptr(P);
  }
  if (!IsNil(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransObjectInPresence;
    }
    return(False);
  }

  if (FirstObjectInPresenceP != ((DoorsObjectInPresence *) Null)) {
    LastObjectInPresenceP->next = ((DoorsObjectInPresence *) Null);
  }
  *CObjectInPresence = FirstObjectInPresenceP;
  return(True);
}

int fromFcpTransConnection(Connection, CConnection)
     heapP Connection;
     DoorsConnection **CConnection;
{
  register heapP P = Connection;
  register DoorsConnection *ConnectionP = *CConnection;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 3)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransConnection;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (ConnectionP == ((DoorsConnection *) Null)) {
      return(True);
    }
  }

  if (ConnectionP == ((DoorsConnection *) Null)) {
    drsfcptc_allocate(sizeof(DoorsConnection),
		      (DoorsConnection *), ConnectionP);
  }
  
  ConnectionP->presence_id = ((DoorsPresenceId) Null);
  if (!fromFcpTransPresenceId((P+1), &(ConnectionP->presence_id))) {
    return(False);
  }

  if (!fromFcpTransObjectId((P+2), &(ConnectionP->object_id))) {
    return(False);
  }

  if (!fromFcpTransConnectionType((P+3), &(ConnectionP->type))) {
    return(False);
  }

  *CConnection = ConnectionP;
  return(True);
}

int fromFcpTransPresenceState(PresenceState, CPresenceState)
     heapP PresenceState;
     DoorsPresenceState **CPresenceState;
{
  register heapP P = PresenceState;
  register DoorsPresenceState *PresenceStateP = *CPresenceState;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 15)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransPresenceState;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (PresenceStateP == ((DoorsPresenceState *) Null)) {
      return(True);
    }
  }

  if (PresenceStateP == ((DoorsPresenceState *) Null)) {
    drsfcptc_allocate(sizeof(DoorsPresenceState), (DoorsPresenceState *),
		      PresenceStateP);
  }
  
  PresenceStateP->nick_name = ((DoorsString) Null);
  if (!fromFcpTransString((P+1), &(PresenceStateP->nick_name))) {
    return(False);
  }

  PresenceStateP->version = ((DoorsVersion) Null);
  if (!fromFcpTransVersion((P+2), &(PresenceStateP->version))) {
    return(False);
  }

  PresenceStateP->position = ((DoorsPosition *) Null);
  if (!fromFcpTransPosition((P+3), &(PresenceStateP->position))) {
    return(False);
  }

  PresenceStateP->click_position = ((DoorsPosition *) Null);
  if (!fromFcpTransPosition(
         (P+4), &(PresenceStateP->click_position))) {
    return(False);
  }

  PresenceStateP->click_door = ((DoorsDoor *) Null);
  if (!fromFcpTransDoor((P+5), &(PresenceStateP->click_door))) {
    return(False);
  }

  if (!fromFcpTransFaceState((P+6), &(PresenceStateP->state))) {
    return(False);
  }

  PresenceStateP->edit_date = ((DoorsDate *) Null);
  if (!fromFcpTransDate((P+7), &(PresenceStateP->edit_date))) {
    return(False);
  }

  PresenceStateP->alert_date = ((DoorsDate *) Null);
  if (!fromFcpTransDate((P+8), &(PresenceStateP->alert_date))) {
    return(False);
  }

  PresenceStateP->joined_date = ((DoorsDate *) Null);
  if (!fromFcpTransDate((P+9), &(PresenceStateP->joined_date))) {
    return(False);
  }

  PresenceStateP->date = ((DoorsDate *) Null);
  if (!fromFcpTransDate((P+10), &(PresenceStateP->date))) {
    return(False);
  }

  if (!fromFcpTransBoolean((P+11), &(PresenceStateP->has_icon))) {
    return(False);
  }

  PresenceStateP->objects = ((DoorsObjectInPresence *) Null);
  if (!fromFcpTransObjectInPresence((P+12), &(PresenceStateP->objects))) {
    return(False);
  }

  PresenceStateP->connection = ((DoorsConnection *) Null);
  if (!fromFcpTransConnection(
         (P+13), &(PresenceStateP->connection))) {
    return(False);
  }

  if (!fromFcpTransUlong((P+14), &(PresenceStateP->audio_port))) {
    return(False);
  }

  if (!fromFcpTransBoolean((P+15), &(PresenceStateP->audio_focus))) {
    return(False);
  }

  *CPresenceState = PresenceStateP;
  return(True);
}

int fromFcpTransPlaceState(PlaceState, CPlaceState)
     heapP PlaceState;
     DoorsPlaceState **CPlaceState;
{
  register heapP P = PlaceState;
  register DoorsPlaceState *PlaceStateP = *CPlaceState;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 4)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransPlaceState;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (PlaceStateP == ((DoorsPlaceState *) Null)) {
      return(True);
    }
  }

  if (PlaceStateP == ((DoorsPlaceState *) Null)) {
    drsfcptc_allocate(sizeof(DoorsPlaceState), (DoorsPlaceState *),
		      PlaceStateP);
  }
  
  if (!fromFcpTransUlong((P+1), &(PlaceStateP->document_size))) {
    return(False);
  }

  if (!fromFcpTransUlong((P+2), &(PlaceStateP->document_hash))) {
    return(False);
  }

  PlaceStateP->document_load_date = ((DoorsDate *) Null);
  if (!fromFcpTransDate((P+3), &(PlaceStateP->document_load_date))) {
    return(False);
  }

  PlaceStateP->document_date = ((DoorsDate *) Null);
  if (!fromFcpTransDate((P+4), &(PlaceStateP->document_date))) {
    return(False);
  }

  *CPlaceState = PlaceStateP;
  return(True);
}

int fromFcpTransPresenceList(PresenceList, CPresenceList)
     heapP PresenceList;
     DoorsPresenceList **CPresenceList;
{
  register heapP P = PresenceList;
  DoorsPresenceList *FirstPresenceListP = ((DoorsPresenceList *) Null);
  DoorsPresenceList *LastPresenceListP = ((DoorsPresenceList *) Null);
  register DoorsPresenceList *PresenceListP = *CPresenceList;

  P = PresenceList;
  deref_ptr(P);
  if (!(IsNil(*P) || IsList(*P))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransPresenceList;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (PresenceListP == ((DoorsPresenceList *) Null)) {
      return(True);
    }
  }

  while (IsList(*P)) {
    if (PresenceListP == ((DoorsPresenceList *) Null)) {
      drsfcptc_allocate(sizeof(DoorsPresenceList), (DoorsPresenceList *),
			PresenceListP);
      PresenceListP->next = ((DoorsPresenceList *) Null);
    }

    {
      register heapP Pa = P;
      register heapT Va = Off_List(*Pa);

      deref(Va, Pa);
      PresenceListP->id = ((DoorsPresenceId) Null);
      if (!fromFcpTransPresenceId(Pa, &(PresenceListP->id))) {
	return(False);
      }
    }

    if (FirstPresenceListP == ((DoorsPresenceList *) Null)) {
      FirstPresenceListP = PresenceListP;
    }
    if (LastPresenceListP == ((DoorsPresenceList *) Null)) {
      LastPresenceListP = PresenceListP;
    }
    else {
      LastPresenceListP->next = PresenceListP;
      LastPresenceListP = PresenceListP;
    }
    PresenceListP = PresenceListP->next;

    P = Cdr(P);
    deref_ptr(P);
  }
  if (!IsNil(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransPresenceList;
    }
    return(False);
  }

  if (FirstPresenceListP != ((DoorsPresenceList *) Null)) {
    LastPresenceListP->next = ((DoorsPresenceList *) Null);
  }
  *CPresenceList = FirstPresenceListP;
  return(True);
}

int fromFcpTransHeader(Header, CHeader)
     heapP Header;
     DoorsHeader **CHeader;
{
  register heapP P = Header;
  register DoorsHeader *HeaderP = *CHeader;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 5)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransHeader;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (HeaderP == ((DoorsHeader *) Null)) {
      return(True);
    }
  }

  if (HeaderP == ((DoorsHeader *) Null)) {
    drsfcptc_allocate(sizeof(DoorsHeader), (DoorsHeader *), HeaderP);
  }
  
  HeaderP->id = (DoorsPresenceId) Null;
  if (!fromFcpTransPresenceId((P+1), &(HeaderP->id))) {
    return(False);
  }

  if (!fromFcpTransDestination((P+2), &(HeaderP->destination))) {
    return(False);
  }

  HeaderP->list = (DoorsPresenceList *) Null;
  if (!fromFcpTransPresenceList((P+3), &(HeaderP->list))) {
    return(False);
  }

  HeaderP->door = (DoorsDoor *) Null;
  if (!fromFcpTransDoor((P+4), &(HeaderP->door))) {
    return(False);
  }

  {
    DoorsDateP DateP = (DoorsDateP) &(HeaderP->date);

    if (!fromFcpTransDate((P+5), &(DateP))) {
      return(False);
    }
  }

  *CHeader = HeaderP;
  return(True);
}

int fromFcpTransClientCapabilities(ClientCapabilities, CClientCapabilities)
     heapP ClientCapabilities;
     DoorsClientCapabilities **CClientCapabilities;
{
  register heapP P = ClientCapabilities;
  register DoorsClientCapabilities *ClientCapabilitiesP =
    *CClientCapabilities;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 2)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransClientCapabilities;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (ClientCapabilitiesP == ((DoorsClientCapabilities *) Null)) {
      return(True);
    }
  }

  if (ClientCapabilitiesP == ((DoorsClientCapabilities *) Null)) {
    drsfcptc_allocate(sizeof(DoorsClientCapabilities),
		      (DoorsClientCapabilities *), ClientCapabilitiesP);
  }
  
  if (!fromFcpTransBoolean((P+1),
			   &(ClientCapabilitiesP->audio_enabled))) {
    return(False);
  }

  if (!fromFcpTransBoolean((P+2),
			   &(ClientCapabilitiesP->multicast_supported))) {
    return(False);
  }

  *CClientCapabilities = ClientCapabilitiesP;
  return(True);
}

int fromFcpTransClubTicketCapabilities(ClubTicketCapabilities,
				       CClubTicketCapabilities)
     heapP ClubTicketCapabilities;
     DoorsClubTicketCapabilities **CClubTicketCapabilities;
{
  register heapP P = ClubTicketCapabilities;
  register DoorsClubTicketCapabilities *ClubTicketCapabilitiesP =
    *CClubTicketCapabilities;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 1)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransClubTicketCapabilities;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (ClubTicketCapabilitiesP ==
	((DoorsClubTicketCapabilities *) Null)) {
      return(True);
    }
  }

  if (ClubTicketCapabilitiesP == ((DoorsClubTicketCapabilities *) Null)) {
    drsfcptc_allocate(sizeof(DoorsClubTicketCapabilities),
		      (DoorsClubTicketCapabilities *),
		      ClubTicketCapabilitiesP);
  }
  
  if (!fromFcpTransUlong((P+1), &(ClubTicketCapabilitiesP->bus_capacity))) {
    return(False);
  }

  *CClubTicketCapabilities = ClubTicketCapabilitiesP;
  return(True);
}

/* doorsTyp.h - end */

/* doorsRc.h - start */

int fromFcpTransErrorVal(ErrorVal, CErrorVal)
     heapP ErrorVal;
     DoorsErrorVal *CErrorVal;
{
  register heapP P = ErrorVal;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransErrorVal;
    }
    return(False);
  }

  {
    DoorsErrorVal E = (DoorsErrorVal) Int_Val(*P);

    switch (E) {
    case DOORS_OK_ERROR_VAL:
    case DOORS_BAD_ARGUMENT_ERROR_VAL:
    case DOORS_MALLOC_ERROR_VAL:
    case DOORS_INTERNAL_ERROR:
    case DOORS_NO_LOCAL_IP_ADDRESS:
    case DOORS_SHM_ERROR:
    case DOORS_FORK_ERROR:
    case DOORS_EXEC_ERROR:
    case DOORS_NO_RESOURCES:
    case DOORS_TOO_MANY_CLIENTS:
    case DOORS_WK_PORT_IN_USE:
    case DOORS_ALREADY_INIT:
    case DOORS_NOT_INIT:
    case DOORS_TOKEN_BAD_FILE:
    case DOORS_TOKEN_NOT_INITIALIZED:
    case DOORS_TOKEN_INVALID:
    case DOORS_TOKEN_EXPIRED:
    case DOORS_COMMUNICATION_CRASHED:
    case DOORS_AUDIO_NOT_ENABLED:
    case DOORS_MULTICAST_NOT_SUPPORTED:
      *CErrorVal = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransErrorVal;
      return(False);
    }
  }
}

int fromFcpTransWarning(Warning, CWarning)
     heapP Warning;
     DoorsWarning *CWarning;
{
  register heapP P = Warning;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransWarning;
    }
    return(False);
  }

  {
    DoorsWarning E = (DoorsWarning) Int_Val(*P);

    switch (E) {
    case DOORS_NO_WARNING:
    case DOORS_TOKEN_EXPIRES_SOON:
      *CWarning = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransWarning;
      return(False);
    }
  }
}

/* doorsRc.h - end */

/* doorsErr.h - start */

int fromFcpTransErrorCategory(ErrorCategory, CErrorCategory)
     heapP ErrorCategory;
     DoorsErrorCategory *CErrorCategory;
{
  register heapP P = ErrorCategory;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransErrorCategory;
    }
    return(False);
  }

  {
    DoorsErrorCategory E = (DoorsErrorCategory) Int_Val(*P);

    switch (E) {
    case DOORS_PLACE_CONNECT_ERROR_CATEGORY:
    case DOORS_INTERFACE_ERROR_CATEGORY:
    case DOORS_AUDIO_ERROR_CATEGORY:
      *CErrorCategory = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransErrorCategory;
      return(False);
    }
  }
}

int fromFcpTransPlaceConnectErrorType(PlaceConnectErrorType,
				      CPlaceConnectErrorType)
     heapP PlaceConnectErrorType;
     DoorsPlaceConnectErrorType *CPlaceConnectErrorType;
{
  register heapP P = PlaceConnectErrorType;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransPlaceConnectErrorType;
    }
    return(False);
  }

  {
    DoorsPlaceConnectErrorType E = (DoorsPlaceConnectErrorType) Int_Val(*P);

    switch (E) {
    case DOORS_PLACE_CONNECT_SERVER_CLOSED:
    case DOORS_PLACE_CONNECT_VERSION_MISMATCH:
    case DOORS_PLACE_CONNECT_SERVER_INTERNAL:
    case DOORS_PLACE_CONNECT_TOKEN_INVALID:
    case DOORS_PLACE_CONNECT_TOKEN_EXPIRED:
    case DOORS_PLACE_CONNECT_TOKEN_INTERNAL:
    case DOORS_PLACE_CONNECT_PUBLIC_AREA_IS_FULL:
    case DOORS_PLACE_CONNECT_SERVER_IS_FULL:
    case DOORS_PLACE_CONNECT_CLIENT_IS_FULL:
    case DOORS_PLACE_CONNECT_PROCESSOR_IS_FULL:
    case DOORS_PLACE_CONNECT_SERVER_NOT_THERE:
    case DOORS_PLACE_CONNECT_CANNOT_CONNECT:
    case DOORS_PLACE_CONNECT_PROTOCOL_ERROR:
      *CPlaceConnectErrorType = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransPlaceConnectErrorType;
      return(False);
    }
  }
}

int fromFcpTransInterfaceErrorType(InterfaceErrorType, CInterfaceErrorType)
     heapP InterfaceErrorType;
     DoorsInterfaceErrorType *CInterfaceErrorType;
{
  register heapP P = InterfaceErrorType;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransInterfaceErrorType;
    }
    return(False);
  }

  {
    DoorsInterfaceErrorType E = (DoorsInterfaceErrorType) Int_Val(*P);

    switch (E) {
    case DOORS_INTERFACE_DISCONNECT_UNKNOWN_PRESENCE_ERROR:
    case DOORS_INTERFACE_VIEW_EVENTS_UNKNOWN_PRESENCE_ERROR:
    case DOORS_INTERFACE_GOT_EVENT_UNKNOWN_PRESENCE_ERROR:
    case DOORS_INTERFACE_QUERY_CACHE_UNKNOWN_PRESENCE_ERROR:
    case DOORS_INTERFACE_QUERY_PLACE_PRESENCES_UNKNOWN_PRESENCE_ERROR:
    case DOORS_INTERFACE_CREATE_OBJECT_NO_USER_ERROR:
    case DOORS_INTERFACE_CONNECT_TO_PRESENCE_UNKNOWN_PRESENCE_ERROR:
    case DOORS_INTERFACE_CONNECT_TO_OBJECT_UNKNOWN_PRESENCE_ERROR:
    case DOORS_INTERFACE_QUERY_OBJECT_CONNECTIONS_UNKNOWN_PRESENCE_ERROR:
      *CInterfaceErrorType = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransInterfaceErrorType;
      return(False);
    }
  }
}

int fromFcpTransAudioErrorType(AudioErrorType, CAudioErrorType)
     heapP AudioErrorType;
     DoorsAudioErrorType *CAudioErrorType;
{
  register heapP P = AudioErrorType;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransAudioErrorType;
    }
    return(False);
  }

  {
    DoorsAudioErrorType E = (DoorsAudioErrorType) Int_Val(*P);

    switch (E) {
    case DOORS_AUDIO_NET_IO_ERROR:
    case DOORS_AUDIO_DEV_IO_ERROR:
    case DOORS_AUDIO_DEV_BUSY:
    case DOORS_AUDIO_DEV_CANT_OPEN:
    case DOORS_AUDIO_CONNECT_TO_SELF:
    case DOORS_AUDIO_OTHER_ERROR:
      *CAudioErrorType = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransAudioErrorType;
      return(False);
    }
  }
}

int fromFcpTransErrorSeverity(ErrorSeverity, CErrorSeverity)
     heapP ErrorSeverity;
     DoorsErrorSeverity *CErrorSeverity;
{
  register heapP P = ErrorSeverity;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransErrorSeverity;
    }
    return(False);
  }

  {
    DoorsErrorSeverity E = (DoorsErrorSeverity) Int_Val(*P);

    switch (E) {
    case DOORS_ERROR_SEVERITY_WARNING:
    case DOORS_ERROR_SEVERITY_ERROR:
    case DOORS_ERROR_SEVERITY_FATAL:
      *CErrorSeverity = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransErrorSeverity;
      return(False);
    }
  }
}

int fromFcpTransInterfaceError(InterfaceError, CInterfaceError)
     heapP InterfaceError;
     DoorsInterfaceError **CInterfaceError;
{
  register heapP P = InterfaceError;
  register DoorsInterfaceError *InterfaceErrorP = *CInterfaceError;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 2)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransInterfaceError;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (InterfaceErrorP == ((DoorsInterfaceError *) Null)) {
      return(True);
    }
  }

  if (InterfaceErrorP == ((DoorsInterfaceError *) Null)) {
    drsfcptc_allocate(sizeof(DoorsInterfaceError), (DoorsInterfaceError *),
		      InterfaceErrorP);
  }
  
  if (!fromFcpTransInterfaceErrorType(
         (P+1), &(InterfaceErrorP->type))) {
    return(False);
  }

  InterfaceErrorP->client_data = ((DoorsBytes *) Null);
  if (!fromFcpTransBytes((P+2), &(InterfaceErrorP->client_data))) {
    return(False);
  }

  *CInterfaceError = InterfaceErrorP;
  return(True);
}

int fromFcpTransError(Error, CError)
     heapP Error;
     DoorsError **CError;
{
  register heapP P = Error;
  register DoorsError *ErrorP = *CError;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 3)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransError;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (ErrorP == ((DoorsError *) Null)) {
      return(True);
    }
  }

  if (ErrorP == ((DoorsError *) Null)) {
    drsfcptc_allocate(sizeof(DoorsError), (DoorsError *), ErrorP);
  }
  
  if (!fromFcpTransErrorSeverity((P+1), &(ErrorP->severity))) {
    return(False);
  }

  if (!fromFcpTransErrorCategory((P+2), &(ErrorP->category))) {
    return(False);
  }

  switch(ErrorP->category) {
  case DOORS_PLACE_CONNECT_ERROR_CATEGORY:
    if (!fromFcpTransPlaceConnectErrorType((P+3),
	  &(ErrorP->content.place_connect_error))) {
      return(False);
    }
    break;
  case DOORS_INTERFACE_ERROR_CATEGORY:
    ErrorP->content.interface_error = (DoorsInterfaceError *) Null;
    if (!fromFcpTransInterfaceError(
           (P+3), &(ErrorP->content.interface_error))) {
      return(False);
    }
    break;
  case DOORS_AUDIO_ERROR_CATEGORY:
    if (!fromFcpTransAudioErrorType((P+3),
	   &(ErrorP->content.audio_error))) {
      return(False);
    }
    break;
  }

  *CError = ErrorP;
  return(True);
}

/* doorsErr.h - end */

/* doorsEvn.h - start */

int fromFcpTransEventCategory(EventCategory, CEventCategory)
     heapP EventCategory;
     DoorsEventCategory *CEventCategory;
{
  register heapP P = EventCategory;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransEventCategory;
    }
    return(False);
  }

  {
    DoorsEventCategory E = (DoorsEventCategory) Int_Val(*P);

    switch (E) {
    case DOORS_PRESENCE_EVENT_CATEGORY:
    case DOORS_PLACE_EVENT_CATEGORY:
    case DOORS_TRANSIENT_EVENT_CATEGORY:
    case DOORS_ALERT_EVENT_CATEGORY:
    case DOORS_MESSAGE_EVENT_CATEGORY:
    case DOORS_OTHER_EVENT_CATEGORY:
    case DOORS_AUDIO_EVENT_CATEGORY:
      *CEventCategory = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransEventCategory;
      return(False);
    }
  }
}

int fromFcpTransPresenceEventType(PresenceEventType, CPresenceEventType)
     heapP PresenceEventType;
     DoorsPresenceEventType *CPresenceEventType;
{
  register heapP P = PresenceEventType;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransPresenceEventType;
    }
    return(False);
  }

  {
    DoorsPresenceEventType E = (DoorsPresenceEventType) Int_Val(*P);

    switch (E) {
    case DOORS_PRESENCE_ENTERED_EVENT:
    case DOORS_PRESENCE_LEFT_EVENT:
      *CPresenceEventType = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransPresenceEventType;
      return(False);
    }
  }
}

int fromFcpTransPlaceEventType(PlaceEventType, CPlaceEventType)
     heapP PlaceEventType;
     DoorsPlaceEventType *CPlaceEventType;
{
  register heapP P = PlaceEventType;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransPlaceEventType;
    }
    return(False);
  }

  {
    DoorsPlaceEventType E = (DoorsPlaceEventType) Int_Val(*P);

    switch (E) {
    case DOORS_PLACE_INVITE_EVENT:
    case DOORS_PLACE_UPDATE_EVENT:
    case DOORS_PLACE_EDIT_STARTED_EVENT:
    case DOORS_PLACE_EDIT_FAILED_EVENT:
    case DOORS_PLACE_EDIT_FINISHED_EVENT:
    case DOORS_PLACE_POST_EVENT:
    case DOORS_PLACE_MAIL_EVENT:
    case DOORS_PLACE_CREATED_OBJECT_EVENT:
    case DOORS_PLACE_FAILED_TO_CREATE_OBJECT_EVENT:
    case DOORS_PLACE_DELETED_OBJECT_EVENT:
    case DOORS_PLACE_CONNECTED_PRESENCE_EVENT:
    case DOORS_PLACE_CONNECTED_OBJECT_EVENT:
    case DOORS_PLACE_DISCONNECTED_PRESENCE_EVENT:
    case DOORS_PLACE_DISCONNECTED_OBJECT_EVENT:
    case DOORS_PLACE_FAILED_TO_CONNECT_TO_PRESENCE_EVENT:
    case DOORS_PLACE_FAILED_TO_CONNECT_TO_OBJECT_EVENT:
      *CPlaceEventType = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransPlaceEventType;
      return(False);
    }
  }
}

int fromFcpTransTransientEventType(TransientEventType, CTransientEventType)
     heapP TransientEventType;
     DoorsTransientEventType *CTransientEventType;
{
  register heapP P = TransientEventType;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransTransientEventType;
    }
    return(False);
  }

  {
    DoorsTransientEventType E = (DoorsTransientEventType) Int_Val(*P);

    switch (E) {
    case DOORS_TRANSIENT_CLICKED_EVENT:
    case DOORS_TRANSIENT_MOVED_EVENT:
    case DOORS_TRANSIENT_FACE_STATE_EVENT:
    case DOORS_TRANSIENT_AUDIO_FOCUS_EVENT:
    case DOORS_TRANSIENT_CONVERSATION_TYPE_EVENT:
      *CTransientEventType = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransTransientEventType;
      return(False);
    }
  }
}

int fromFcpTransAlertEventType(AlertEventType, CAlertEventType)
     heapP AlertEventType;
     DoorsAlertEventType *CAlertEventType;
{
  register heapP P = AlertEventType;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransAlertEventType;
    }
    return(False);
  }

  {
    DoorsAlertEventType E = (DoorsAlertEventType) Int_Val(*P);

    switch (E) {
    case DOORS_ALERT_ALERT_EVENT:
      *CAlertEventType = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransAlertEventType;
      return(False);
    }
  }
}

int fromFcpTransMessageEventType(MessageEventType, CMessageEventType)
     heapP MessageEventType;
     DoorsMessageEventType *CMessageEventType;
{
  register heapP P = MessageEventType;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransMessageEventType;
    }
    return(False);
  }

  {
    DoorsMessageEventType E = (DoorsMessageEventType) Int_Val(*P);

    switch (E) {
    case DOORS_MESSAGE_TEXT_EVENT:
      *CMessageEventType = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransMessageEventType;
      return(False);
    }
  }
}

int fromFcpTransOtherEventType(OtherEventType, COtherEventType)
     heapP OtherEventType;
     DoorsOtherEventType *COtherEventType;
{
  register heapP P = OtherEventType;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransOtherEventType;
    }
    return(False);
  }

  {
    DoorsOtherEventType E = (DoorsOtherEventType) Int_Val(*P);

    switch (E) {
    case DOORS_OTHER_TEXT_EVENT:
      *COtherEventType = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransOtherEventType;
      return(False);
    }
  }
}

int fromFcpTransAudioEventType(AudioEventType, CAudioEventType)
     heapP AudioEventType;
     DoorsAudioEventType *CAudioEventType;
{
  register heapP P = AudioEventType;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransAudioEventType;
    }
    return(False);
  }

  {
    DoorsAudioEventType E = (DoorsAudioEventType) Int_Val(*P);

    switch (E) {
    case DOORS_AUDIO_REMOTE_JOINED_EVENT:
    case DOORS_AUDIO_REMOTE_LEFT_EVENT:
      *CAudioEventType = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransAudioEventType;
      return(False);
    }
  }
}

int fromFcpTransCreateObjectFailure(CreateObjectFailure, CCreateObjectFailure)
     heapP CreateObjectFailure;
     DoorsCreateObjectFailure *CCreateObjectFailure;
{
  register heapP P = CreateObjectFailure;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransCreateObjectFailure;
    }
    return(False);
  }

  {
    DoorsCreateObjectFailure E = (DoorsCreateObjectFailure) Int_Val(*P);

    switch (E) {
    case DOORS_CREATE_OBJECT_FAILED_ALREADY_DRIVER:
    case DOORS_CREATE_OBJECT_FAILED_ALREADY_PASSENGER:
    case DOORS_CREATE_OBJECT_FAILED_IN_ONE_TO_ONE:
    case DOORS_CREATE_OBJECT_UNAUTHORIZED:
    case DOORS_CREATE_OBJECT_TOO_BIG:
      *CCreateObjectFailure = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransCreateObjectFailure;
      return(False);
    }
  }
}

int fromFcpTransConnectFailure(ConnectFailure, CConnectFailure)
     heapP ConnectFailure;
     DoorsConnectFailure *CConnectFailure;
{
  register heapP P = ConnectFailure;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransConnectFailure;
    }
    return(False);
  }

  {
    DoorsConnectFailure E = (DoorsConnectFailure) Int_Val(*P);

    switch (E) {
    case DOORS_CONNECT_FAILED_SERVER_ERROR:
    case DOORS_CONNECT_FAILED_NO_CONNECTEE:
    case DOORS_CONNECT_FAILED_BUSY:
    case DOORS_CONNECT_FAILED_MISMATCH:
    case DOORS_CONNECT_FAILED_NO_ROOM:
      *CConnectFailure = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransConnectFailure;
      return(False);
    }
  }
}

int fromFcpTransPresenceEvent(PresenceEvent, CPresenceEvent)
     heapP PresenceEvent;
     DoorsPresenceEvent **CPresenceEvent;
{
  register heapP P = PresenceEvent;
  register DoorsPresenceEvent *PresenceEventP = *CPresenceEvent;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 2)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransPresenceEvent;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (PresenceEventP == ((DoorsPresenceEvent *) Null)) {
      return(True);
    }
  }

  if (PresenceEventP == ((DoorsPresenceEvent *) Null)) {
    drsfcptc_allocate(sizeof(DoorsPresenceEvent), (DoorsPresenceEvent *),
		      PresenceEventP);
  }
  
  if (!fromFcpTransPresenceEventType((P+1), &(PresenceEventP->type))) {
    return(False);
  }

  switch(PresenceEventP->type) {
  case DOORS_PRESENCE_ENTERED_EVENT:
    {
      register heapP Pa = (P+2);

      deref_ptr(Pa);
      if (!(IsTpl(*Pa) && (Arity_of(*Pa) == 3))) {
	if (IsVar(*Pa)) {
	  sus_tbl_add(Pa);
	}
	return(False);
      }

      PresenceEventP->data.presence_entered_event.place_state =
	(DoorsPlaceState *) Null;
      if (!fromFcpTransPlaceState(
	     (Pa+1),
              &(PresenceEventP->data.presence_entered_event.place_state))) {
	return(False);
      }

      PresenceEventP->data.presence_entered_event.presence_state =
	(DoorsPresenceState *) Null;
      if (!fromFcpTransPresenceState(
	     (Pa+2),
              &(PresenceEventP->data.presence_entered_event.presence_state))) {
	return(False);
      }

      PresenceEventP->data.presence_entered_event.from = (DoorsDoor *) Null;
      if (!fromFcpTransDoor(
	     (Pa+3),
              &(PresenceEventP->data.presence_entered_event.from))) {
	return(False);
      }
    }
    break;
  case DOORS_PRESENCE_LEFT_EVENT:
    PresenceEventP->data.left_to = (DoorsDoor *) Null;
    if (!fromFcpTransDoor((P+2), &(PresenceEventP->data.left_to))) {
      return(False);
    }
  }

  *CPresenceEvent = PresenceEventP;
  return(True);
}

int fromFcpTransPlaceEvent(PlaceEvent, CPlaceEvent)
     heapP PlaceEvent;
     DoorsPlaceEvent **CPlaceEvent;
{
  register heapP P = PlaceEvent;
  register DoorsPlaceEvent *PlaceEventP = *CPlaceEvent;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 2)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransPlaceEvent;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (PlaceEventP == ((DoorsPlaceEvent *) Null)) {
      return(True);
    }
  }

  if (PlaceEventP == ((DoorsPlaceEvent *) Null)) {
    drsfcptc_allocate(sizeof(DoorsPlaceEvent), (DoorsPlaceEvent *),
		      PlaceEventP);
  }
  
  if (!fromFcpTransPlaceEventType((P+1), &(PlaceEventP->type))) {
    return(False);
  }

  switch(PlaceEventP->type) {
  case DOORS_PLACE_INVITE_EVENT:
  case DOORS_PLACE_UPDATE_EVENT:
  case DOORS_PLACE_EDIT_STARTED_EVENT:
  case DOORS_PLACE_EDIT_FAILED_EVENT:
  case DOORS_PLACE_EDIT_FINISHED_EVENT:
  case DOORS_PLACE_POST_EVENT:
  case DOORS_PLACE_MAIL_EVENT:
    PlaceEventP->data.text = (DoorsString) Null;
    if (!fromFcpTransString((P+2), &(PlaceEventP->data.text))) {
      return(False);
    }
    break;
  case DOORS_PLACE_CREATED_OBJECT_EVENT:
   {
      register heapP Pa = (P+2);

      deref_ptr(Pa);
      if (!(IsTpl(*Pa) && (Arity_of(*Pa) == 2))) {
        if (IsVar(*Pa)) {
          sus_tbl_add(Pa);
        }
        else {
          DoorsFcpError = DOORSFCP_fromFcpTransPlaceEvent;
        }
        return(False);
      }

      if (!fromFcpTransObjectId((Pa+1),
          &(PlaceEventP->data.place_created_object_event.object_id))) {
        return(False);
      }

      PlaceEventP->data.place_created_object_event.object_state =
        (DoorsObjectState *) Null;
      if (!fromFcpTransObjectState((Pa+2),
       &(PlaceEventP->data.place_created_object_event.object_state))) {
        return(False); 
      }   
    }
    break;
  case DOORS_PLACE_FAILED_TO_CREATE_OBJECT_EVENT:
   {
      register heapP Pa = (P+2);

      deref_ptr(Pa);
      if (!(IsTpl(*Pa) && (Arity_of(*Pa) == 2))) {
        if (IsVar(*Pa)) {
          sus_tbl_add(Pa);
        }
        else {
          DoorsFcpError = DOORSFCP_fromFcpTransPlaceEvent;
        }
        return(False);
      }

      if (!fromFcpTransCreateObjectFailure((Pa+1), &(PlaceEventP->
	     data.place_failed_to_create_object_event.reason))) {
        return(False);
      }

      if (!fromFcpTransObjectId((Pa+2), &(PlaceEventP->
	   data.place_failed_to_create_object_event.object_id))) {
        return(False);
      }
    }
    break;
  case DOORS_PLACE_DELETED_OBJECT_EVENT:
    if (!fromFcpTransObjectId((P+2), &(PlaceEventP->data.object_id))) {
      return(False);
    }
    break;
  case DOORS_PLACE_CONNECTED_PRESENCE_EVENT:
  {
      register heapP Pa = (P+2);

      deref_ptr(Pa);
      if (!(IsTpl(*Pa) && (Arity_of(*Pa) == 2))) {
        if (IsVar(*Pa)) {
          sus_tbl_add(Pa);
        }
        else {
          DoorsFcpError = DOORSFCP_fromFcpTransPlaceEvent;
        }
        return(False);
      }

      if (!fromFcpTransPresenceId((Pa+1),
          &(PlaceEventP-> 		
		data.place_connected_presence_event.connected_presence_id))) {
        return(False);
      }

      PlaceEventP->data.place_connected_presence_event.new_position=
        (DoorsPosition *) Null;
      if (!fromFcpTransPosition((Pa+2),
       &(PlaceEventP->data.place_connected_presence_event.new_position))) {
        return(False); 
      }   
    }
    break;
  case DOORS_PLACE_CONNECTED_OBJECT_EVENT:
   {
     register heapP Pa = (P+2);

      deref_ptr(Pa);
      if (!(IsTpl(*Pa) && (Arity_of(*Pa) == 3))) {
        if (IsVar(*Pa)) {
          sus_tbl_add(Pa);
        }
        else {
          DoorsFcpError = DOORSFCP_fromFcpTransPlaceEvent;
        }
        return(False);
      }

      if (!fromFcpTransPresenceId((Pa+1),
          &(PlaceEventP-> 		
		data.place_connected_object_event.connected_presence_id))) {
        return(False);
      }

      PlaceEventP->data.place_connected_object_event.connected_object_id =
        (DoorsObjectId) Null;
      if (!fromFcpTransObjectId((Pa+2),
       &(PlaceEventP->
	 data.place_connected_object_event.connected_object_id))) {
        return(False); 
      }  

      PlaceEventP->data.place_connected_object_event.new_position=
        (DoorsPosition *) Null;
      if (!fromFcpTransPosition((Pa+3),
       &(PlaceEventP->data.place_connected_object_event.new_position))) {
        return(False); 
      }   
    }
    break;
  case DOORS_PLACE_DISCONNECTED_PRESENCE_EVENT:
    if (!fromFcpTransPresenceId((P+2),
		 &(PlaceEventP->data.disconnected_presence_id))) {
      return(False);
    }
    break;
  case DOORS_PLACE_DISCONNECTED_OBJECT_EVENT:
    {
     register heapP Pa = (P+2);

      deref_ptr(Pa);
      if (!(IsTpl(*Pa) && (Arity_of(*Pa) == 2))) {
        if (IsVar(*Pa)) {
          sus_tbl_add(Pa);
        }
        else {
          DoorsFcpError = DOORSFCP_fromFcpTransPlaceEvent;
        }
        return(False);
      }

      if (!fromFcpTransPresenceId((Pa+1),
          &(PlaceEventP-> 		
	    data.place_disconnected_object_event.disconnected_presence_id))) {
        return(False);
      }

      PlaceEventP->
	data.place_disconnected_object_event.disconnected_object_id =
	  (DoorsObjectId) Null;
      if (!fromFcpTransObjectId((Pa+2),
       &(PlaceEventP->
	 data.place_disconnected_object_event.disconnected_object_id))) {
        return(False); 
      }   
    }
    break;
  case DOORS_PLACE_FAILED_TO_CONNECT_TO_PRESENCE_EVENT:
    {
     register heapP Pa = (P+2);

      deref_ptr(Pa);
      if (!(IsTpl(*Pa) && (Arity_of(*Pa) == 2))) {
        if (IsVar(*Pa)) {
          sus_tbl_add(Pa);
        }
        else {
          DoorsFcpError = DOORSFCP_fromFcpTransPlaceEvent;
        }
        return(False);
      }

      if (!fromFcpTransConnectFailure((Pa+1),
	 &(PlaceEventP-> 		
	    data.place_failed_to_connect_to_presence_event.reason))) {
        return(False);
      }
      if (!fromFcpTransPresenceId((Pa+2),
          &(PlaceEventP-> 		
	    data.place_failed_to_connect_to_presence_event.presence_id))) {
        return(False);
      }  
    }
    break;
  case DOORS_PLACE_FAILED_TO_CONNECT_TO_OBJECT_EVENT:
    {
    register heapP Pa = (P+2);

      deref_ptr(Pa);
      if (!(IsTpl(*Pa) && (Arity_of(*Pa) == 3))) {
        if (IsVar(*Pa)) {
          sus_tbl_add(Pa);
        }
        else {
          DoorsFcpError = DOORSFCP_fromFcpTransPlaceEvent;
        }
        return(False);
      }

      if (!fromFcpTransConnectFailure((Pa+1),
	 &(PlaceEventP-> 		
	    data.place_failed_to_connect_to_object_event.reason))) {
        return(False);
      }
      if (!fromFcpTransPresenceId((Pa+2),
          &(PlaceEventP-> 		
	    data.place_failed_to_connect_to_object_event.presence_id))) {
        return(False);
      } 
      if (!fromFcpTransObjectId((Pa+3),
          &(PlaceEventP-> 		
	    data.place_failed_to_connect_to_object_event.object_id))) {
        return(False);
      } 
    }
   break;
  }

  *CPlaceEvent = PlaceEventP;
  return(True);
}

int fromFcpTransTransientEvent(TransientEvent, CTransientEvent)
     heapP TransientEvent;
     DoorsTransientEvent **CTransientEvent;
{
  register heapP P = TransientEvent;
  register DoorsTransientEvent *TransientEventP = *CTransientEvent;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 2)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransTransientEvent;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (TransientEventP == ((DoorsTransientEvent *) Null)) {
      return(True);
    }
  }

  if (TransientEventP == ((DoorsTransientEvent *) Null)) {
    drsfcptc_allocate(sizeof(DoorsTransientEvent), (DoorsTransientEvent *),
		      TransientEventP);
  }
  
  if (!fromFcpTransTransientEventType((P+1), &(TransientEventP->type))) {
    return(False);
  }

  switch(TransientEventP->type) {
  case DOORS_TRANSIENT_CLICKED_EVENT:
    {
      register heapP Pa = (P+2);

      deref_ptr(Pa);
      if (!(IsTpl(*Pa) && (Arity_of(*Pa) == 2))) {
	if (IsVar(*Pa)) {
	  sus_tbl_add(Pa);
	}
	else {
	  DoorsFcpError = DOORSFCP_fromFcpTransTransientEvent;
	}
	return(False);
      }

      TransientEventP->data.transient_clicked_event.position =
	(DoorsPosition *) Null;
      if (!fromFcpTransPosition(
	     (Pa+1),
             &(TransientEventP->data.transient_clicked_event.position))) {
	return(False);
      }

      TransientEventP->data.transient_clicked_event.door = (DoorsDoor *) Null;
      if (!fromFcpTransDoor(
	     (Pa+2),
             &(TransientEventP->data.transient_clicked_event.door))) {
	return(False);
      }
    }
    break;
  case DOORS_TRANSIENT_MOVED_EVENT:
    {
      register heapP Pa = (P+2);

      deref_ptr(Pa);
      if (!(IsTpl(*Pa) && (Arity_of(*Pa) == 2))) {
	if (IsVar(*Pa)) {
	  sus_tbl_add(Pa);
	}
	else {
	  DoorsFcpError = DOORSFCP_fromFcpTransTransientEvent;
	}
	return(False);
      }

      if (!fromFcpTransObjectId(
	     (Pa+1),
             &(TransientEventP->data.transient_moved_event.object_id))) {
	return(False);
      }

      TransientEventP->data.transient_moved_event.position =
	(DoorsPosition *) Null;
      if (!fromFcpTransPosition(
	     (Pa+2),
             &(TransientEventP->data.transient_moved_event.position))) {
	return(False);
      }
    }
    break;
  case DOORS_TRANSIENT_FACE_STATE_EVENT:
    if (!fromFcpTransFaceState(
           (P+2), &(TransientEventP->data.new_face_state))) {
      return(False);
    }
    break;
  case DOORS_TRANSIENT_AUDIO_FOCUS_EVENT:
    if (!fromFcpTransBoolean(
           (P+2), &(TransientEventP->data.new_audio_focus))) {
      return(False);
    }
    break;
  case DOORS_TRANSIENT_CONVERSATION_TYPE_EVENT:
    {
      register heapP Pa = (P+2);

      deref_ptr(Pa);
      if (!(IsTpl(*Pa) && (Arity_of(*Pa) == 2))) {
	if (IsVar(*Pa)) {
	  sus_tbl_add(Pa);
	}
	else {
	  DoorsFcpError = DOORSFCP_fromFcpTransTransientEvent;
	}
	return(False);
      }

      if (!fromFcpTransObjectId(
	     (Pa+1),
             &(TransientEventP->
	       data.transient_conversation_type_event.object_id))) {
	return(False);
      }

      if (!fromFcpTransConversationType(
	     (Pa+2),
             &(TransientEventP->
	      data.transient_conversation_type_event.new_conversation_type))) {
	return(False);
      }
    }
    break;
  }

  *CTransientEvent = TransientEventP;
  return(True);
}

int fromFcpTransAlertEvent(AlertEvent, CAlertEvent)
     heapP AlertEvent;
     DoorsAlertEvent **CAlertEvent;
{
  register heapP P = AlertEvent;
  register DoorsAlertEvent *AlertEventP = *CAlertEvent;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 1)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransAlertEvent;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (AlertEventP == ((DoorsAlertEvent *) Null)) {
      return(True);
    }
  }

  if (AlertEventP == ((DoorsAlertEvent *) Null)) {
    drsfcptc_allocate(sizeof(DoorsAlertEvent), (DoorsAlertEvent *),
		      AlertEventP);
  }
  
  if (!fromFcpTransAlertEventType((P+1), &(AlertEventP->type))) {
    return(False);
  }

  *CAlertEvent = AlertEventP;
  return(True);
}

int fromFcpTransMessageEvent(MessageEvent, CMessageEvent)
     heapP MessageEvent;
     DoorsMessageEvent **CMessageEvent;
{
  register heapP P = MessageEvent;
  register DoorsMessageEvent *MessageEventP = *CMessageEvent;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 2)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransMessageEvent;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (MessageEventP == ((DoorsMessageEvent *) Null)) {
      return(True);
    }
  }

  if (MessageEventP == ((DoorsMessageEvent *) Null)) {
    drsfcptc_allocate(sizeof(DoorsMessageEvent), (DoorsMessageEvent *),
		      MessageEventP);
  }
  
  if (!fromFcpTransMessageEventType((P+1), &(MessageEventP->type))) {
    return(False);
  }

  switch(MessageEventP->type) {
  case DOORS_MESSAGE_TEXT_EVENT:
    MessageEventP->data.text = (DoorsString) Null;
    if (!fromFcpTransString((P+2), &(MessageEventP->data.text))) {
      return(False);
    }
    break;
  }

  *CMessageEvent = MessageEventP;
  return(True);
}

int fromFcpTransOtherEvent(OtherEvent, COtherEvent)
     heapP OtherEvent;
     DoorsOtherEvent **COtherEvent;
{
  register heapP P = OtherEvent;
  register DoorsOtherEvent *OtherEventP = *COtherEvent;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 2)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransOtherEvent;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (OtherEventP == ((DoorsOtherEvent *) Null)) {
      return(True);
    }
  }

  if (OtherEventP == ((DoorsOtherEvent *) Null)) {
    drsfcptc_allocate(sizeof(DoorsOtherEvent), (DoorsOtherEvent *),
		      OtherEventP);
  }
  
  if (!fromFcpTransOtherEventType((P+1), &(OtherEventP->type))) {
    return(False);
  }

  switch(OtherEventP->type) {
  case DOORS_OTHER_TEXT_EVENT:
    OtherEventP->data.text = (DoorsString) Null;
    if (!fromFcpTransString((P+2), &(OtherEventP->data.text))) {
      return(False);
    }
    break;
  }

  *COtherEvent = OtherEventP;
  return(True);
}

int fromFcpTransAudioEvent(AudioEvent, CAudioEvent)
     heapP AudioEvent;
     DoorsAudioEvent **CAudioEvent;
{
  register heapP P = AudioEvent;
  register DoorsAudioEvent *AudioEventP = *CAudioEvent;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 2)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransAudioEvent;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (AudioEventP == ((DoorsAudioEvent *) Null)) {
      return(True);
    }
  }

  if (AudioEventP == ((DoorsAudioEvent *) Null)) {
    drsfcptc_allocate(sizeof(DoorsAudioEvent), (DoorsAudioEvent *),
		      AudioEventP);
  }
  
  if (!fromFcpTransAudioEventType((P+1), &(AudioEventP->type))) {
    return(False);
  }

  switch(AudioEventP->type) {
  case DOORS_AUDIO_REMOTE_JOINED_EVENT:
  case DOORS_AUDIO_REMOTE_LEFT_EVENT:
    AudioEventP->remote_id = (DoorsString) Null;
    if (!fromFcpTransString((P+2), &(AudioEventP->remote_id))) {
      return(False);
    }
    break;
  }

  *CAudioEvent = AudioEventP;
  return(True);
}

int fromFcpTransEvent(Event, CEvent)
     heapP Event;
     DoorsEvent **CEvent;
{
  register heapP P = Event;
  register DoorsEvent *EventP = *CEvent;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 2)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransEvent;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (EventP == ((DoorsEvent *) Null)) {
      return(True);
    }
  }

  if (EventP == ((DoorsEvent *) Null)) {
    drsfcptc_allocate(sizeof(DoorsEvent), (DoorsEvent *), EventP);
  }
  
  if (!fromFcpTransEventCategory((P+1), &(EventP->category))) {
    return(False);
  }

  switch(EventP->category) {
  case DOORS_PRESENCE_EVENT_CATEGORY:
    EventP->content.presence_event = (DoorsPresenceEvent *) Null;
    if (!fromFcpTransPresenceEvent((P+2), &(EventP->content.presence_event))) {
      return(False);
    }
    break;
  case DOORS_PLACE_EVENT_CATEGORY:
    EventP->content.place_event = (DoorsPlaceEvent *) Null;
    if (!fromFcpTransPlaceEvent((P+2), &(EventP->content.place_event))) {
      return(False);
    }
    break;
  case DOORS_TRANSIENT_EVENT_CATEGORY:
    EventP->content.transient_event = (DoorsTransientEvent *) Null;
    if (!fromFcpTransTransientEvent(
           (P+2), &(EventP->content.transient_event))) {
      return(False);
    }
    break;
  case DOORS_ALERT_EVENT_CATEGORY:
    EventP->content.alert_event = (DoorsAlertEvent *) Null;
    if (!fromFcpTransAlertEvent((P+2), &(EventP->content.alert_event))) {
      return(False);
    }
    break;
  case DOORS_MESSAGE_EVENT_CATEGORY:
    EventP->content.message_event = (DoorsMessageEvent *) Null;
    if (!fromFcpTransMessageEvent((P+2), &(EventP->content.message_event))) {
      return(False);
    }
    break;
  case DOORS_OTHER_EVENT_CATEGORY:
    EventP->content.other_event = (DoorsOtherEvent *) Null;
    if (!fromFcpTransOtherEvent((P+2), &(EventP->content.other_event))) {
      return(False);
    }
    break;
  case DOORS_AUDIO_EVENT_CATEGORY:
    EventP->content.audio_event = (DoorsAudioEvent *) Null;
    if (!fromFcpTransAudioEvent((P+2), &(EventP->content.audio_event))) {
      return(False);
    }
    break;
  }

  *CEvent = EventP;
  return(True);
}

/* doorsEvn.h - end */

/* doorsBc.h - start */

int fromFcpTransBusinessCard(BusinessCard, CBusinessCard)
     heapP BusinessCard;
     DoorsBusinessCard **CBusinessCard;
{
  register heapP P = BusinessCard;
  register DoorsBusinessCard *BusinessCardP = *CBusinessCard;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 14)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransBusinessCard;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (BusinessCardP == ((DoorsBusinessCard *) Null)) {
      return(True);
    }
  }

  if (BusinessCardP == ((DoorsBusinessCard *) Null)) {
    drsfcptc_allocate(sizeof(DoorsBusinessCard), (DoorsBusinessCard *),
		      BusinessCardP);
  }
  
  BusinessCardP->email_address = ((DoorsString) Null);
  if (!fromFcpTransString((P+1), &(BusinessCardP->email_address))) {
    return(False);
  }
  BusinessCardP->private_url = ((DoorsString) Null);
  if (!fromFcpTransString((P+2), &(BusinessCardP->private_url))) {
    return(False);
  }
  BusinessCardP->first_name = ((DoorsString) Null);
  if (!fromFcpTransString((P+3), &(BusinessCardP->first_name))) {
    return(False);
  }
  BusinessCardP->last_name = ((DoorsString) Null);
  if (!fromFcpTransString((P+4), &(BusinessCardP->last_name))) {
    return(False);
  }
  BusinessCardP->organization = ((DoorsString) Null);
  if (!fromFcpTransString((P+5), &(BusinessCardP->organization))) {
    return(False);
  }
  BusinessCardP->title = ((DoorsString) Null);
  if (!fromFcpTransString((P+6), &(BusinessCardP->title))) {
    return(False);
  }
  BusinessCardP->mail_stop = ((DoorsString) Null);
  if (!fromFcpTransString((P+7), &(BusinessCardP->mail_stop))) {
    return(False);
  }
  BusinessCardP->address = ((DoorsString) Null);
  if (!fromFcpTransString((P+8), &(BusinessCardP->address))) {
    return(False);
  }
  BusinessCardP->city = ((DoorsString) Null);
  if (!fromFcpTransString((P+9), &(BusinessCardP->city))) {
    return(False);
  }
  BusinessCardP->state = ((DoorsString) Null);
  if (!fromFcpTransString((P+10), &(BusinessCardP->state))) {
    return(False);
  }
  BusinessCardP->zip_code = ((DoorsString) Null);
  if (!fromFcpTransString((P+11), &(BusinessCardP->zip_code))) {
    return(False);
  }
  BusinessCardP->phone = ((DoorsString) Null);
  if (!fromFcpTransString((P+12), &(BusinessCardP->phone))) {
    return(False);
  }
  BusinessCardP->fax = ((DoorsString) Null);
  if (!fromFcpTransString((P+13), &(BusinessCardP->fax))) {
    return(False);
  }
  BusinessCardP->remark = ((DoorsString) Null);
  if (!fromFcpTransString((P+14), &(BusinessCardP->remark))) {
    return(False);
  }

  *CBusinessCard = BusinessCardP;
  return(True);
}

/* doorsBc.h - end */

/* doorsReq.h - start */

int fromFcpTransRequestCategory(RequestCategory, CRequestCategory)
     heapP RequestCategory;
     DoorsRequestCategory *CRequestCategory;
{
  register heapP P = RequestCategory;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransRequestCategory;
    }
    return(False);
  }

  {
    DoorsRequestCategory E = (DoorsRequestCategory) Int_Val(*P);

    switch (E) {
    case DOORS_USER_REQUEST_CATEGORY:
    case DOORS_PLACE_REQUEST_CATEGORY:
    case DOORS_SERVER_REQUEST_CATEGORY:
      *CRequestCategory = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransRequestCategory;
      return(False);
    }
  }
}

int fromFcpTransUserRequestType(UserRequestType, CUserRequestType)
     heapP UserRequestType;
     DoorsUserRequestType *CUserRequestType;
{
  register heapP P = UserRequestType;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransUserRequestType;
    }
    return(False);
  }

  {
    DoorsUserRequestType E = (DoorsUserRequestType) Int_Val(*P);

    switch (E) {
    case DOORS_USER_DATA_REQUEST:
    case DOORS_USER_FACE_REQUEST:
    case DOORS_USER_ALL_REQUEST:
      *CUserRequestType = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransUserRequestType;
      return(False);
    }
  }
}

int fromFcpTransPlaceRequestType(PlaceRequestType, CPlaceRequestType)
     heapP PlaceRequestType;
     DoorsPlaceRequestType *CPlaceRequestType;
{
  register heapP P = PlaceRequestType;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransPlaceRequestType;
    }
    return(False);
  }

  {
    DoorsPlaceRequestType E = (DoorsPlaceRequestType) Int_Val(*P);

    switch (E) {
    case DOORS_PLACE_CONNECT_REQUEST:
    case DOORS_PLACE_VIEW_EVENTS_REQUEST:
    case DOORS_PLACE_CACHE_REQUEST:
    case DOORS_PLACE_PRESENCES_REQUEST:
    case DOORS_PLACE_CONNECT_TO_PRESENCE_REQUEST:
    case DOORS_PLACE_CONNECT_TO_OBJECT_REQUEST:
    case DOORS_PLACE_OBJECT_CONNECTIONS_REQUEST:
    case DOORS_PLACE_CREATE_OBJECT_REQUEST:
      *CPlaceRequestType = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransPlaceRequestType;
      return(False);
    }
  }
}

int fromFcpTransServerRequestType(ServerRequestType, CServerRequestType)
     heapP ServerRequestType;
     DoorsServerRequestType *CServerRequestType;
{
  register heapP P = ServerRequestType;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransServerRequestType;
    }
    return(False);
  }

  {
    DoorsServerRequestType E = (DoorsServerRequestType) Int_Val(*P);

    switch (E) {
    case DOORS_SERVER_PRESENCES_COUNT_REQUEST:
    case DOORS_SERVER_PRESENCES_LIST_REQUEST:
      *CServerRequestType = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransServerRequestType;
      return(False);
    }
  }
}

int fromFcpTransUserObjectList(UserObjectList, CUserObjectList)
     heapP UserObjectList;
     DoorsUserObjectList **CUserObjectList;
{
  register heapP P = UserObjectList;
  DoorsUserObjectList *FirstUserObjectListP = ((DoorsUserObjectList *) Null);
  DoorsUserObjectList *LastUserObjectListP = ((DoorsUserObjectList *) Null);
  register DoorsUserObjectList *UserObjectListP = *CUserObjectList;

  P = UserObjectList;
  deref_ptr(P);
  if (!(IsNil(*P) || IsList(*P))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransUserObjectList;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (UserObjectListP == ((DoorsUserObjectList *) Null)) {
      return(True);
    }
  }

  while (IsList(*P)) {
    if (UserObjectListP == ((DoorsUserObjectList *) Null)) {
      drsfcptc_allocate(sizeof(DoorsUserObjectList),
			(DoorsUserObjectList *), UserObjectListP);
      UserObjectListP->next = ((DoorsUserObjectList *) Null);
    }

    {
      register heapP Pa = P;
      register heapT Va = Off_List(*Pa);

      deref(Va, Pa);
      if (!(IsTpl(Va) && (Arity_of(Va) == 2))) {
	if (IsVar(Va)) {
	  sus_tbl_add(Pa);
	}
	else {
	  DoorsFcpError = DOORSFCP_fromFcpTransUserObjectList;
	}
	return(False);
      }

      UserObjectListP->net_name = ((DoorsString) Null);
      if (!fromFcpTransString((Pa+1), &(UserObjectListP->net_name))) {
	return(False);
      }

      UserObjectListP->object_name = ((DoorsString) Null);
      if (!fromFcpTransString((Pa+2), &(UserObjectListP->object_name))) {
	return(False);
      }
    }

    if (FirstUserObjectListP == ((DoorsUserObjectList *) Null)) {
      FirstUserObjectListP = UserObjectListP;
    }
    if (LastUserObjectListP == ((DoorsUserObjectList *) Null)) {
      LastUserObjectListP = UserObjectListP;
    }
    else {
      LastUserObjectListP->next = UserObjectListP;
      LastUserObjectListP = UserObjectListP;
    }
    UserObjectListP = UserObjectListP->next;

    P = Cdr(P);
    deref_ptr(P);
  }
  if (!IsNil(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransUserObjectList;
    }
    return(False);
  }

  if (FirstUserObjectListP != ((DoorsUserObjectList *) Null)) {
    LastUserObjectListP->next = ((DoorsUserObjectList *) Null);
  }
  *CUserObjectList = FirstUserObjectListP;
  return(True);
}

int fromFcpTransEventCategoryList(EventCategoryList, CEventCategoryList)
     heapP EventCategoryList;
     DoorsEventCategoryList **CEventCategoryList;
{
  register heapP P = EventCategoryList;
  DoorsEventCategoryList *FirstEventCategoryListP =
    ((DoorsEventCategoryList *) Null);
  DoorsEventCategoryList *LastEventCategoryListP =
    ((DoorsEventCategoryList *) Null);
  register DoorsEventCategoryList *EventCategoryListP = *CEventCategoryList;

  P = EventCategoryList;
  deref_ptr(P);
  if (!(IsNil(*P) || IsList(*P))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransEventCategoryList;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (EventCategoryListP == ((DoorsEventCategoryList *) Null)) {
      return(True);
    }
  }

  while (IsList(*P)) {
    if (EventCategoryListP == ((DoorsEventCategoryList *) Null)) {
      drsfcptc_allocate(sizeof(DoorsEventCategoryList),
			(DoorsEventCategoryList *), EventCategoryListP);
      EventCategoryListP->next = ((DoorsEventCategoryList *) Null);
    }

    {
      register heapP Pa = P;
      register heapT Va = Off_List(*Pa);

      deref(Va, Pa);
      if (!fromFcpTransEventCategory(Pa, &(EventCategoryListP->category))) {
	return(False);
      }
    }

    if (FirstEventCategoryListP == ((DoorsEventCategoryList *) Null)) {
      FirstEventCategoryListP = EventCategoryListP;
    }
    if (LastEventCategoryListP == ((DoorsEventCategoryList *) Null)) {
      LastEventCategoryListP = EventCategoryListP;
    }
    else {
      LastEventCategoryListP->next = EventCategoryListP;
      LastEventCategoryListP = EventCategoryListP;
    }
    EventCategoryListP = EventCategoryListP->next;

    P = Cdr(P);
    deref_ptr(P);
  }
  if (!IsNil(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransEventCategoryList;
    }
    return(False);
  }

  if (FirstEventCategoryListP != ((DoorsEventCategoryList *) Null)) {
    LastEventCategoryListP->next = ((DoorsEventCategoryList *) Null);
  }
  *CEventCategoryList = FirstEventCategoryListP;
  return(True);
}

int fromFcpTransUserRequest(UserRequest, CUserRequest)
     heapP UserRequest;
     DoorsUserRequest **CUserRequest;
{
  register heapP P = UserRequest;
  register DoorsUserRequest *UserRequestP = *CUserRequest;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 2)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransUserRequest;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (UserRequestP == ((DoorsUserRequest *) Null)) {
      return(True);
    }
  }

  if (UserRequestP == ((DoorsUserRequest *) Null)) {
    drsfcptc_allocate(sizeof(DoorsUserRequest), (DoorsUserRequest *),
		      UserRequestP);
  }
  
  if (!fromFcpTransUserRequestType((P+1), &(UserRequestP->type))) {
    return(False);
  }

  UserRequestP->query_list = (DoorsUserObjectList *) Null;
  if (!fromFcpTransUserObjectList((P+2), &(UserRequestP->query_list))) {
    return(False);
  }

  *CUserRequest = UserRequestP;
  return(True);
}

int fromFcpTransPlaceRequest(PlaceRequest, CPlaceRequest)
     heapP PlaceRequest;
     DoorsPlaceRequest **CPlaceRequest;
{
  register heapP P = PlaceRequest;
  register DoorsPlaceRequest *PlaceRequestP = *CPlaceRequest;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 2)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransPlaceRequest;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (PlaceRequestP == ((DoorsPlaceRequest *) Null)) {
      return(True);
    }
  }

  if (PlaceRequestP == ((DoorsPlaceRequest *) Null)) {
    drsfcptc_allocate(sizeof(DoorsPlaceRequest), (DoorsPlaceRequest *),
		      PlaceRequestP);
  }
  
  if (!fromFcpTransPlaceRequestType((P+1), &(PlaceRequestP->type))) {
    return(False);
  }

  switch(PlaceRequestP->type) {
  case DOORS_PLACE_CONNECT_REQUEST:
    {
      register heapP Pa = (P+2);

      deref_ptr(Pa);
      if (!(IsTpl(*Pa) && (Arity_of(*Pa) == 5))) {
	if (IsVar(*Pa)) {
	  sus_tbl_add(Pa);
	}
	else {
	  DoorsFcpError = DOORSFCP_fromFcpTransPlaceRequest;
	}
	return(False);
      }

      PlaceRequestP->data.place_connect_request.place_state =
	(DoorsPlaceState *) Null;
      if (!fromFcpTransPlaceState(
	     (Pa+1),
             &(PlaceRequestP->data.place_connect_request.place_state))) {
	return(False);
      }

      PlaceRequestP->data.place_connect_request.presence_state =
	(DoorsPresenceState *) Null;
      if (!fromFcpTransPresenceState(
	     (Pa+2),
             &(PlaceRequestP->data.place_connect_request.presence_state))) {
	return(False);
      }

      PlaceRequestP->data.place_connect_request.from = (DoorsDoor *) Null;
      if (!fromFcpTransDoor(
	     (Pa+3), &(PlaceRequestP->data.place_connect_request.from))) {
	return(False);
      }

      PlaceRequestP->data.place_connect_request.to = (DoorsDoor *) Null;
      if (!fromFcpTransDoor(
	     (Pa+4), &(PlaceRequestP->data.place_connect_request.to))) {
	return(False);
      }

      PlaceRequestP->data.place_connect_request.business_card =
	(DoorsBusinessCard *) Null;
      if (!fromFcpTransBusinessCard((Pa+5),
	    &(PlaceRequestP->data.place_connect_request.business_card))) {
	return(False);
      }

    }
    break;
  case DOORS_PLACE_VIEW_EVENTS_REQUEST:
    {
      register heapP Pa = (P+2);

      deref_ptr(Pa);
      if (!(IsTpl(*Pa) && (Arity_of(*Pa) == 2))) {
	if (IsVar(*Pa)) {
	  sus_tbl_add(Pa);
	}
	else {
	  DoorsFcpError = DOORSFCP_fromFcpTransPlaceRequest;
	}
	return(False);
      }

      if (!fromFcpTransBoolean(
             (Pa+1),
             &(PlaceRequestP->data.place_view_events_request.snapshot))) {
	return(False);
      }

      PlaceRequestP->data.place_view_events_request.categories =
	(DoorsEventCategoryList *) Null;
      if (!fromFcpTransEventCategoryList(
	     (Pa+2),
             &(PlaceRequestP->data.place_view_events_request.categories))) {
	return(False);
      }
    }
    break;
  case DOORS_PLACE_CACHE_REQUEST:
    {
      register heapP Pa = (P+2);

      deref_ptr(Pa);
      if (!(IsTpl(*Pa) && (Arity_of(*Pa) == 4))) {
	if (IsVar(*Pa)) {
	  sus_tbl_add(Pa);
	}
	else {
	  DoorsFcpError = DOORSFCP_fromFcpTransPlaceRequest;
	}
	return(False);
      }

      PlaceRequestP->data.place_cache_request.ctgrs =
	(DoorsEventCategoryList *) Null;
      if (!fromFcpTransEventCategoryList(
	     (Pa+1),
             &(PlaceRequestP->data.place_cache_request.ctgrs))) {
	return(False);
      }

      if (!fromFcpTransUlong(
             (Pa+2), &(PlaceRequestP->data.place_cache_request.number))) {
	return(False);
      }

      PlaceRequestP->data.place_cache_request.from = (DoorsDate *) Null;
      if (!fromFcpTransDate(
	     (Pa+3), &(PlaceRequestP->data.place_cache_request.from))) {
	return(False);
      }

      PlaceRequestP->data.place_cache_request.until = (DoorsDate *) Null;
      if (!fromFcpTransDate(
	     (Pa+4), &(PlaceRequestP->data.place_cache_request.until))) {
	return(False);
      }
    }
    break;
  case DOORS_PLACE_PRESENCES_REQUEST:
    break;
  case DOORS_PLACE_CONNECT_TO_PRESENCE_REQUEST:
    {  
      register heapP Pa = (P+2);

      deref_ptr(Pa);
      if (!(IsTpl(*Pa) && (Arity_of(*Pa) == 3))) {
        if (IsVar(*Pa)) {
          sus_tbl_add(Pa);
        }
        else {
          DoorsFcpError = DOORSFCP_fromFcpTransPlaceRequest;
        }
        return(False);
      }

      PlaceRequestP->
		data.place_connect_to_presence_request.connectee_presence_id =
	 (DoorsPresenceId) Null;

      if (!fromFcpTransPresenceId((Pa+1),
	     &(PlaceRequestP->
		data.place_connect_to_presence_request.connectee_presence_id))){
        return(False);
      }

      PlaceRequestP->
		data.place_connect_to_presence_request.connectee_position =
	 (DoorsPosition *) Null;

      if (!fromFcpTransPresenceId((Pa+2),
	     &(PlaceRequestP->
		data.place_connect_to_presence_request.connectee_position))){
        return(False);
      }
      PlaceRequestP->
		data.place_connect_to_presence_request.new_position =
	 (DoorsPosition *) Null;

      if (!fromFcpTransPresenceId((Pa+3),
	     &(PlaceRequestP->
		data.place_connect_to_presence_request.new_position))){
        return(False);
      }

    }
    break;
  case DOORS_PLACE_CONNECT_TO_OBJECT_REQUEST:
    { 
      register heapP Pa = (P+2);
 
      deref_ptr(Pa);
      if (!(IsTpl(*Pa) && (Arity_of(*Pa) == 4))) {
        if (IsVar(*Pa)) {
          sus_tbl_add(Pa);
        }
        else {
          DoorsFcpError = DOORSFCP_fromFcpTransPlaceRequest;
        }
        return(False);
      }

      PlaceRequestP->
                data.place_connect_to_object_request.connectee_presence_id =
         (DoorsPresenceId) Null;

      if (!fromFcpTransPresenceId((Pa+1),
             &(PlaceRequestP->
	       data.place_connect_to_object_request.connectee_presence_id))){  
	return(False);
      }

      if (!fromFcpTransObjectId((Pa+2),
	     &(PlaceRequestP->
	       data.place_connect_to_object_request.connectee_object_id))){  
	return(False);
      }

     PlaceRequestP->
                data.place_connect_to_object_request.connectee_position =
         (DoorsPosition *) Null;

      if (!fromFcpTransPosition((Pa+3),
             &(PlaceRequestP->
                data.place_connect_to_object_request.connectee_position))){
        return(False);
      }

      PlaceRequestP->
                data.place_connect_to_object_request.new_position =
         (DoorsPosition *) Null;

      if (!fromFcpTransPosition((Pa+4),
             &(PlaceRequestP->
                data.place_connect_to_object_request.new_position))){
        return(False);
      }

    }
    break;
  case DOORS_PLACE_OBJECT_CONNECTIONS_REQUEST:
    {
      register heapP Pa = (P+2);
 
      deref_ptr(Pa);
      if (!(IsTpl(*Pa) && (Arity_of(*Pa) == 2))) {
        if (IsVar(*Pa)) {
          sus_tbl_add(Pa);
        }
        else {
          DoorsFcpError = DOORSFCP_fromFcpTransPlaceRequest;
        }
        return(False);
      }

      PlaceRequestP->
                data.place_object_connections_request.presence_id =
         (DoorsPresenceId) Null;

      if (!fromFcpTransPresenceId((Pa+1),
             &(PlaceRequestP->
                data.place_object_connections_request.presence_id))){          	
	return(False);
      }

      if (!fromFcpTransObjectId((Pa+2),
             &(PlaceRequestP->
                data.place_object_connections_request.object_id))){  
        return(False);
      }
    }
    break;
  case DOORS_PLACE_CREATE_OBJECT_REQUEST:
    {
      register heapP Pa = (P+2);
 
      deref_ptr(Pa);
      if (!(IsTpl(*Pa) && (Arity_of(*Pa) == 2))) {
        if (IsVar(*Pa)) {
          sus_tbl_add(Pa);
        }
        else {
          DoorsFcpError = DOORSFCP_fromFcpTransPlaceRequest;
        }
        return(False);
      }

      if (!fromFcpTransObjectId((Pa+1),
             &(PlaceRequestP->
                data.place_create_object_request.object_id))) {
        return(False);
      }

      PlaceRequestP->data.place_create_object_request.object_state =
	(DoorsObjectState *) Null;

      if (!fromFcpTransObjectState((Pa+2),
	     &(PlaceRequestP->
	       data.place_create_object_request.object_state))) {
	return(False);
      }

    }
    break;
  }

  *CPlaceRequest = PlaceRequestP;
  return(True);
}

int fromFcpTransServerRequest(ServerRequest, CServerRequest)
     heapP ServerRequest;
     DoorsServerRequest **CServerRequest;
{
  register heapP P = ServerRequest;
  register DoorsServerRequest *ServerRequestP = *CServerRequest;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 1)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransServerRequest;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (ServerRequestP == ((DoorsServerRequest *) Null)) {
      return(True);
    }
  }

  if (ServerRequestP == ((DoorsServerRequest *) Null)) {
    drsfcptc_allocate(sizeof(DoorsServerRequest), (DoorsServerRequest *),
		      ServerRequestP);
  }
  
  if (!fromFcpTransServerRequestType((P+1), &(ServerRequestP->type))) {
    return(False);
  }

  *CServerRequest = ServerRequestP;
  return(True);
}

int fromFcpTransRequest(Request, CRequest)
     heapP Request;
     DoorsRequest **CRequest;
{
  register heapP P = Request;
  register DoorsRequest *RequestP = *CRequest;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 3)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransRequest;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (RequestP == ((DoorsRequest *) Null)) {
      return(True);
    }
  }

  if (RequestP == ((DoorsRequest *) Null)) {
    drsfcptc_allocate(sizeof(DoorsRequest), (DoorsRequest *), RequestP);
  }
  
  RequestP->client_data = (DoorsBytes *) Null;
  if (!fromFcpTransBytes((P+1), &(RequestP->client_data))) {
    return(False);
  }

  if (!fromFcpTransRequestCategory((P+2), &(RequestP->category))) {
    return(False);
  }

  switch(RequestP->category) {
  case DOORS_USER_REQUEST_CATEGORY:
    RequestP->content.user_request = (DoorsUserRequest *) Null;
    if (!fromFcpTransUserRequest((P+3), &(RequestP->content.user_request))) {
      return(False);
    }
    break;
  case DOORS_PLACE_REQUEST_CATEGORY:
    RequestP->content.place_request = (DoorsPlaceRequest *) Null;
    if (!fromFcpTransPlaceRequest((P+3), &(RequestP->content.place_request))) {
      return(False);
    }
    break;
  case DOORS_SERVER_REQUEST_CATEGORY:
    RequestP->content.server_request = (DoorsServerRequest *) Null;
    if (!fromFcpTransServerRequest(
           (P+3), &(RequestP->content.server_request))) {
      return(False);
    }
    break;
  }

  *CRequest = RequestP;
  return(True);
}

/* doorsReq.h - end */

/* doorsRes.h - start */

int fromFcpTransResponseCategory(ResponseCategory, CResponseCategory)
     heapP ResponseCategory;
     DoorsResponseCategory *CResponseCategory;
{
  register heapP P = ResponseCategory;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransResponseCategory;
    }
    return(False);
  }

  {
    DoorsResponseCategory E = (DoorsResponseCategory) Int_Val(*P);

    switch (E) {
    case DOORS_USER_RESPONSE_CATEGORY:
    case DOORS_PLACE_RESPONSE_CATEGORY:
    case DOORS_SERVER_RESPONSE_CATEGORY:
      *CResponseCategory = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransResponseCategory;
      return(False);
    }
  }
}

int fromFcpTransUserResponseType(UserResponseType, CUserResponseType)
     heapP UserResponseType;
     DoorsUserResponseType *CUserResponseType;
{
  register heapP P = UserResponseType;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransUserResponseType;
    }
    return(False);
  }

  {
    DoorsUserResponseType E = (DoorsUserResponseType) Int_Val(*P);

    switch (E) {
    case DOORS_USER_DATA_RESPONSE:
    case DOORS_USER_FACE_RESPONSE:
    case DOORS_USER_ALL_RESPONSE:
      *CUserResponseType = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransUserResponseType;
      return(False);
    }
  }
}

int fromFcpTransPlaceResponseType(PlaceResponseType, CPlaceResponseType)
     heapP PlaceResponseType;
     DoorsPlaceResponseType *CPlaceResponseType;
{
  register heapP P = PlaceResponseType;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransPlaceResponseType;
    }
    return(False);
  }

  {
    DoorsPlaceResponseType E = (DoorsPlaceResponseType) Int_Val(*P);

    switch (E) {
    case DOORS_PLACE_SNAPSHOT_RESPONSE:
    case DOORS_PLACE_CACHE_RESPONSE:
    case DOORS_PLACE_PRESENCES_RESPONSE:
    case DOORS_PLACE_OBJECT_CONNECTIONS_RESPONSE:
      *CPlaceResponseType = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransPlaceResponseType;
      return(False);
    }
  }
}

int fromFcpTransServerResponseType(ServerResponseType, CServerResponseType)
     heapP ServerResponseType;
     DoorsServerResponseType *CServerResponseType;
{
  register heapP P = ServerResponseType;

  deref_ptr(P);
  if (!IsInt(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransServerResponseType;
    }
    return(False);
  }

  {
    DoorsServerResponseType E = (DoorsServerResponseType) Int_Val(*P);

    switch (E) {
    case DOORS_SERVER_PRESENCES_COUNT_RESPONSE:
    case DOORS_SERVER_PRESENCES_LIST_RESPONSE:
      *CServerResponseType = E;
      return(True);
    default:
      DoorsFcpError = DOORSFCP_fromFcpTransServerResponseType;
      return(False);
    }
  }
}

int fromFcpTransPlacePresenceList(PlacePresenceList, CPlacePresenceList)
     heapP PlacePresenceList;
     DoorsPlacePresenceList **CPlacePresenceList;
{
  register heapP P = PlacePresenceList;
  DoorsPlacePresenceList *FirstPlacePresenceListP =
    ((DoorsPlacePresenceList *) Null);
  DoorsPlacePresenceList *LastPlacePresenceListP =
    ((DoorsPlacePresenceList *) Null);
  register DoorsPlacePresenceList *PlacePresenceListP = *CPlacePresenceList;

  P = PlacePresenceList;
  deref_ptr(P);
  if (!(IsNil(*P) || IsList(*P))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransPlacePresenceList;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (PlacePresenceListP == ((DoorsPlacePresenceList *) Null)) {
      return(True);
    }
  }

  while (IsList(*P)) {
    if (PlacePresenceListP == ((DoorsPlacePresenceList *) Null)) {
      drsfcptc_allocate(sizeof(DoorsPlacePresenceList),
			(DoorsPlacePresenceList *), PlacePresenceListP);
      PlacePresenceListP->next = ((DoorsPlacePresenceList *) Null);
    }

    {
      register heapP Pa = P;
      register heapT Va = Off_List(*Pa);

      deref(Va, Pa);
      if (!(IsTpl(Va) && (Arity_of(Va) == 2))) {
	if (IsVar(Va)) {
	  sus_tbl_add(Pa);
	}
	else {
	  DoorsFcpError = DOORSFCP_fromFcpTransPlacePresenceList;
	}
	return(False);
      }

      PlacePresenceListP->door = ((DoorsDoor *) Null);
      if (!fromFcpTransDoor((Pa+1), &(PlacePresenceListP->door))) {
	return(False);
      }

      PlacePresenceListP->presences = ((DoorsPresenceList *) Null);
      if (!fromFcpTransPresenceList(
             (Pa+2), &(PlacePresenceListP->presences))) {
	return(False);
      }
    }

    if (FirstPlacePresenceListP == ((DoorsPlacePresenceList *) Null)) {
      FirstPlacePresenceListP = PlacePresenceListP;
    }
    if (LastPlacePresenceListP == ((DoorsPlacePresenceList *) Null)) {
      LastPlacePresenceListP = PlacePresenceListP;
    }
    else {
      LastPlacePresenceListP->next = PlacePresenceListP;
      LastPlacePresenceListP = PlacePresenceListP;
    }
    PlacePresenceListP = PlacePresenceListP->next;

    P = Cdr(P);
    deref_ptr(P);
  }
  if (!IsNil(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransPlacePresenceList;
    }
    return(False);
  }

  if (FirstPlacePresenceListP != ((DoorsPlacePresenceList *) Null)) {
    LastPlacePresenceListP->next = ((DoorsPlacePresenceList *) Null);
  }
  *CPlacePresenceList = FirstPlacePresenceListP;
  return(True);
}

int fromFcpTransUserDataList(UserDataList, CUserDataList)
     heapP UserDataList;
     DoorsUserDataList **CUserDataList;
{
  register heapP P = UserDataList;
  DoorsUserDataList *FirstUserDataListP = ((DoorsUserDataList *) Null);
  DoorsUserDataList *LastUserDataListP = ((DoorsUserDataList *) Null);
  register DoorsUserDataList *UserDataListP = *CUserDataList;

  P = UserDataList;
  deref_ptr(P);
  if (!(IsNil(*P) || IsList(*P))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransUserDataList;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (UserDataListP == ((DoorsUserDataList *) Null)) {
      return(True);
    }
  }

  while (IsList(*P)) {
    if (UserDataListP == ((DoorsUserDataList *) Null)) {
      drsfcptc_allocate(sizeof(DoorsUserDataList),
			(DoorsUserDataList *), UserDataListP);
      UserDataListP->next = ((DoorsUserDataList *) Null);
    }

    {
      register heapP Pa = P;
      register heapT Va = Off_List(*Pa);

      deref(Va, Pa);
      if (!(IsTpl(Va) && (Arity_of(Va) == 2))) {
	if (IsVar(Va)) {
	  sus_tbl_add(Pa);
	}
	else {
	  DoorsFcpError = DOORSFCP_fromFcpTransUserDataList;
	}
	return(False);
      }

      UserDataListP->net_name = ((DoorsString) Null);
      if (!fromFcpTransString((Pa+1), &(UserDataListP->net_name))) {
	return(False);
      }

      UserDataListP->business_card = ((DoorsBusinessCard *) Null);
      if (!fromFcpTransBusinessCard((Pa+2), &(UserDataListP->business_card))) {
	return(False);
      }
    }

    if (FirstUserDataListP == ((DoorsUserDataList *) Null)) {
      FirstUserDataListP = UserDataListP;
    }
    if (LastUserDataListP == ((DoorsUserDataList *) Null)) {
      LastUserDataListP = UserDataListP;
    }
    else {
      LastUserDataListP->next = UserDataListP;
      LastUserDataListP = UserDataListP;
    }
    UserDataListP = UserDataListP->next;

    P = Cdr(P);
    deref_ptr(P);
  }
  if (!IsNil(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransUserDataList;
    }
    return(False);
  }

  if (FirstUserDataListP != ((DoorsUserDataList *) Null)) {
    LastUserDataListP->next = ((DoorsUserDataList *) Null);
  }
  *CUserDataList = FirstUserDataListP;
  return(True);
}

int fromFcpTransUserFaceList(UserFaceList, CUserFaceList)
     heapP UserFaceList;
     DoorsUserFaceList **CUserFaceList;
{
  register heapP P = UserFaceList;
  DoorsUserFaceList *FirstUserFaceListP = ((DoorsUserFaceList *) Null);
  DoorsUserFaceList *LastUserFaceListP = ((DoorsUserFaceList *) Null);
  register DoorsUserFaceList *UserFaceListP = *CUserFaceList;

  P = UserFaceList;
  deref_ptr(P);
  if (!(IsNil(*P) || IsList(*P))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransUserFaceList;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (UserFaceListP == ((DoorsUserFaceList *) Null)) {
      return(True);
    }
  }

  while (IsList(*P)) {
    if (UserFaceListP == ((DoorsUserFaceList *) Null)) {
      drsfcptc_allocate(sizeof(DoorsUserFaceList),
			(DoorsUserFaceList *), UserFaceListP);
      UserFaceListP->next = ((DoorsUserFaceList *) Null);
    }

    {
      register heapP Pa = P;
      register heapT Va = Off_List(*Pa);

      deref(Va, Pa);
      if (!(IsTpl(Va) && (Arity_of(Va) == 4))) {
	if (IsVar(Va)) {
	  sus_tbl_add(Pa);
	}
	else {
	  DoorsFcpError = DOORSFCP_fromFcpTransUserFaceList;
	}
	return(False);
      }

      UserFaceListP->date = ((DoorsDate *) Null);
      if (!fromFcpTransDate((Pa+1), &(UserFaceListP->date))) {
	return(False);
      }

      UserFaceListP->net_name = ((DoorsString) Null);
      if (!fromFcpTransString((Pa+2), &(UserFaceListP->net_name))) {
	return(False);
      }

      UserFaceListP->object_name = ((DoorsString) Null);
      if (!fromFcpTransString((Pa+3), &(UserFaceListP->object_name))) {
	return(False);
      }

      UserFaceListP->face = ((DoorsFace *) Null);
      if (!fromFcpTransFace((Pa+4), &(UserFaceListP->face))) {
	return(False);
      }
    }

    if (FirstUserFaceListP == ((DoorsUserFaceList *) Null)) {
      FirstUserFaceListP = UserFaceListP;
    }
    if (LastUserFaceListP == ((DoorsUserFaceList *) Null)) {
      LastUserFaceListP = UserFaceListP;
    }
    else {
      LastUserFaceListP->next = UserFaceListP;
      LastUserFaceListP = UserFaceListP;
    }
    UserFaceListP = UserFaceListP->next;

    P = Cdr(P);
    deref_ptr(P);
  }
  if (!IsNil(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransUserFaceList;
    }
    return(False);
  }

  if (FirstUserFaceListP != ((DoorsUserFaceList *) Null)) {
    LastUserFaceListP->next = ((DoorsUserFaceList *) Null);
  }
  *CUserFaceList = FirstUserFaceListP;
  return(True);
}

int fromFcpTransUserAllList(UserAllList, CUserAllList)
     heapP UserAllList;
     DoorsUserAllList **CUserAllList;
{
  register heapP P = UserAllList;
  DoorsUserAllList *FirstUserAllListP = ((DoorsUserAllList *) Null);
  DoorsUserAllList *LastUserAllListP = ((DoorsUserAllList *) Null);
  register DoorsUserAllList *UserAllListP = *CUserAllList;

  P = UserAllList;
  deref_ptr(P);
  if (!(IsNil(*P) || IsList(*P))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransUserAllList;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (UserAllListP == ((DoorsUserAllList *) Null)) {
      return(True);
    }
  }

  while (IsList(*P)) {
    if (UserAllListP == ((DoorsUserAllList *) Null)) {
      drsfcptc_allocate(sizeof(DoorsUserAllList),
			(DoorsUserAllList *), UserAllListP);
      UserAllListP->next = ((DoorsUserAllList *) Null);
    }

    {
      register heapP Pa = P;
      register heapT Va = Off_List(*Pa);

      deref(Va, Pa);
      if (!(IsTpl(Va) && (Arity_of(Va) == 4))) {
	if (IsVar(Va)) {
	  sus_tbl_add(Pa);
	}
	else {
	  DoorsFcpError = DOORSFCP_fromFcpTransUserAllList;
	}
	return(False);
      }

      UserAllListP->date = ((DoorsDate *) Null);
      if (!fromFcpTransDate((Pa+1), &(UserAllListP->date))) {
	return(False);
      }

      UserAllListP->net_name = ((DoorsString) Null);
      if (!fromFcpTransString((Pa+2), &(UserAllListP->net_name))) {
	return(False);
      }

      UserAllListP->business_card = ((DoorsBusinessCard *) Null);
      if (!fromFcpTransBusinessCard((Pa+3), &(UserAllListP->business_card))) {
	return(False);
      }

      UserAllListP->face = ((DoorsFace *) Null);
      if (!fromFcpTransFace((Pa+4), &(UserAllListP->face))) {
	return(False);
      }
    }

    if (FirstUserAllListP == ((DoorsUserAllList *) Null)) {
      FirstUserAllListP = UserAllListP;
    }
    if (LastUserAllListP == ((DoorsUserAllList *) Null)) {
      LastUserAllListP = UserAllListP;
    }
    else {
      LastUserAllListP->next = UserAllListP;
      LastUserAllListP = UserAllListP;
    }
    UserAllListP = UserAllListP->next;

    P = Cdr(P);
    deref_ptr(P);
  }
  if (!IsNil(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransUserAllList;
    }
    return(False);
  }

  if (FirstUserAllListP != ((DoorsUserAllList *) Null)) {
    LastUserAllListP->next = ((DoorsUserAllList *) Null);
  }
  *CUserAllList = FirstUserAllListP;
  return(True);
}

int fromFcpTransPresenceStateList(PresenceStateList, CPresenceStateList)
     heapP PresenceStateList;
     DoorsPresenceStateList **CPresenceStateList;
{
  register heapP P = PresenceStateList;
  DoorsPresenceStateList *FirstPresenceStateListP =
    ((DoorsPresenceStateList *) Null);
  DoorsPresenceStateList *LastPresenceStateListP =
    ((DoorsPresenceStateList *) Null);
  register DoorsPresenceStateList *PresenceStateListP = *CPresenceStateList;

  P = PresenceStateList;
  deref_ptr(P);
  if (!(IsNil(*P) || IsList(*P))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransPresenceStateList;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (PresenceStateListP == ((DoorsPresenceStateList *) Null)) {
      return(True);
    }
  }

  while (IsList(*P)) {
    if (PresenceStateListP == ((DoorsPresenceStateList *) Null)) {
      drsfcptc_allocate(sizeof(DoorsPresenceStateList),
			(DoorsPresenceStateList *), PresenceStateListP);
      PresenceStateListP->next = ((DoorsPresenceStateList *) Null);
    }

    {
      register heapP Pa = P;
      register heapT Va = Off_List(*Pa);

      deref(Va, Pa);
      if (!(IsTpl(Va) && (Arity_of(Va) == 2))) {
	if (IsVar(Va)) {
	  sus_tbl_add(Pa);
	}
	else {
	  DoorsFcpError = DOORSFCP_fromFcpTransPresenceStateList;
	}
	return(False);
      }

      PresenceStateListP->id = ((DoorsPresenceId) Null);
      if (!fromFcpTransPresenceId((Pa+1), &(PresenceStateListP->id))) {
	return(False);
      }

      PresenceStateListP->state = ((DoorsPresenceState *) Null);
      if (!fromFcpTransPresenceState((Pa+2), &(PresenceStateListP->state))) {
	return(False);
      }
    }

    if (FirstPresenceStateListP == ((DoorsPresenceStateList *) Null)) {
      FirstPresenceStateListP = PresenceStateListP;
    }
    if (LastPresenceStateListP == ((DoorsPresenceStateList *) Null)) {
      LastPresenceStateListP = PresenceStateListP;
    }
    else {
      LastPresenceStateListP->next = PresenceStateListP;
      LastPresenceStateListP = PresenceStateListP;
    }
    PresenceStateListP = PresenceStateListP->next;

    P = Cdr(P);
    deref_ptr(P);
  }
  if (!IsNil(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransPresenceStateList;
    }
    return(False);
  }

  if (FirstPresenceStateListP != ((DoorsPresenceStateList *) Null)) {
    LastPresenceStateListP->next = ((DoorsPresenceStateList *) Null);
  }
  *CPresenceStateList = FirstPresenceStateListP;
  return(True);
}

int fromFcpTransPlaceSnapShot(PlaceSnapShot, CPlaceSnapShot)
     heapP PlaceSnapShot;
     DoorsPlaceSnapShot **CPlaceSnapShot;
{
  register heapP P = PlaceSnapShot;
  register DoorsPlaceSnapShot *PlaceSnapShotP = *CPlaceSnapShot;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 2)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransPlaceSnapShot;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (PlaceSnapShotP == ((DoorsPlaceSnapShot *) Null)) {
      return(True);
    }
  }

  if (PlaceSnapShotP == ((DoorsPlaceSnapShot *) Null)) {
    drsfcptc_allocate(sizeof(DoorsPlaceSnapShot), (DoorsPlaceSnapShot *),
		      PlaceSnapShotP);
  }
  
  PlaceSnapShotP->place_state = ((DoorsPlaceState *) Null);
  if (!fromFcpTransPlaceState((P+1), &(PlaceSnapShotP->place_state))) {
    return(False);
  }

  PlaceSnapShotP->presence_state_list = ((DoorsPresenceStateList *) Null);
  if (!fromFcpTransPresenceStateList((P+2),
				     &(PlaceSnapShotP->presence_state_list))) {
    return(False);
  }

  *CPlaceSnapShot = PlaceSnapShotP;
  return(True);
}

int fromFcpTransEventList(EventList, CEventList)
     heapP EventList;
     DoorsEventList **CEventList;
{
  register heapP P = EventList;
  DoorsEventList *FirstEventListP = ((DoorsEventList *) Null);
  DoorsEventList *LastEventListP = ((DoorsEventList *) Null);
  register DoorsEventList *EventListP = *CEventList;

  P = EventList;
  deref_ptr(P);
  if (!(IsNil(*P) || IsList(*P))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransEventList;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (EventListP == ((DoorsEventList *) Null)) {
      return(True);
    }
  }

  while (IsList(*P)) {
    if (EventListP == ((DoorsEventList *) Null)) {
      drsfcptc_allocate(sizeof(DoorsEventList), (DoorsEventList *),
			EventListP);
      EventListP->next = ((DoorsEventList *) Null);
    }

    {
      register heapP Pa = P;
      register heapT Va = Off_List(*Pa);

      deref(Va, Pa);
      if (!(IsTpl(Va) && (Arity_of(Va) == 2))) {
	if (IsVar(Va)) {
	  sus_tbl_add(Pa);
	}
	else {
	  DoorsFcpError = DOORSFCP_fromFcpTransEventList;
	}
	return(False);
      }

      EventListP->event = ((DoorsEvent *) Null);
      if (!fromFcpTransEvent((Pa+1), &(EventListP->event))) {
	return(False);
      }

      EventListP->header = ((DoorsHeader *) Null);
      if (!fromFcpTransHeader((Pa+2), &(EventListP->header))) {
	return(False);
      }
    }

    if (FirstEventListP == ((DoorsEventList *) Null)) {
      FirstEventListP = EventListP;
    }
    if (LastEventListP == ((DoorsEventList *) Null)) {
      LastEventListP = EventListP;
    }
    else {
      LastEventListP->next = EventListP;
      LastEventListP = EventListP;
    }
    EventListP = EventListP->next;

    P = Cdr(P);
    deref_ptr(P);
  }
  if (!IsNil(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransEventList;
    }
    return(False);
  }

  if (FirstEventListP != ((DoorsEventList *) Null)) {
    LastEventListP->next = ((DoorsEventList *) Null);
  }
  *CEventList = FirstEventListP;
  return(True);
}

int fromFcpTransUserResponse(UserResponse, CUserResponse)
     heapP UserResponse;
     DoorsUserResponse **CUserResponse;
{
  register heapP P = UserResponse;
  register DoorsUserResponse *UserResponseP = *CUserResponse;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 2)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransUserResponse;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (UserResponseP == ((DoorsUserResponse *) Null)) {
      return(True);
    }
  }

  if (UserResponseP == ((DoorsUserResponse *) Null)) {
    drsfcptc_allocate(sizeof(DoorsUserResponse), (DoorsUserResponse *),
		      UserResponseP);
  }
  
  if (!fromFcpTransUserResponseType((P+1), &(UserResponseP->type))) {
    return(False);
  }

  switch(UserResponseP->type) {
  case DOORS_USER_DATA_RESPONSE:
    UserResponseP->data.user_data_list = (DoorsUserDataList *) Null;
    if (!fromFcpTransUserDataList(
           (P+2), &(UserResponseP->data.user_data_list))) {
      return(False);
    }
    break;
  case DOORS_USER_FACE_RESPONSE:
    UserResponseP->data.user_face_list = (DoorsUserFaceList *) Null;
    if (!fromFcpTransUserFaceList(
           (P+2), &(UserResponseP->data.user_face_list))) {
      return(False);
    }
    break;
  case DOORS_USER_ALL_RESPONSE:
    UserResponseP->data.user_all_list = (DoorsUserAllList *) Null;
    if (!fromFcpTransUserAllList(
           (P+2), &(UserResponseP->data.user_all_list))) {
      return(False);
    }
    break;
  }

  *CUserResponse = UserResponseP;
  return(True);
}

int fromFcpTransPlaceResponse(PlaceResponse, CPlaceResponse)
     heapP PlaceResponse;
     DoorsPlaceResponse **CPlaceResponse;
{
  register heapP P = PlaceResponse;
  register DoorsPlaceResponse *PlaceResponseP = *CPlaceResponse;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 2)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransPlaceResponse;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (PlaceResponseP == ((DoorsPlaceResponse *) Null)) {
      return(True);
    }
  }

  if (PlaceResponseP == ((DoorsPlaceResponse *) Null)) {
    drsfcptc_allocate(sizeof(DoorsPlaceResponse), (DoorsPlaceResponse *),
		      PlaceResponseP);
  }
  
  if (!fromFcpTransPlaceResponseType((P+1), &(PlaceResponseP->type))) {
    return(False);
  }

  switch(PlaceResponseP->type) {
  case DOORS_PLACE_SNAPSHOT_RESPONSE:
    PlaceResponseP->data.place_snapshot = (DoorsPlaceSnapShot *) Null;
    if (!fromFcpTransPlaceSnapShot(
           (P+2), &(PlaceResponseP->data.place_snapshot))) {
      return(False);
    }
    break;
  case DOORS_PLACE_CACHE_RESPONSE:
    PlaceResponseP->data.event_list = (DoorsEventList *) Null;
    if (!fromFcpTransEventList(
           (P+2), &(PlaceResponseP->data.event_list))) {
      return(False);
    }
    break;
  case DOORS_PLACE_PRESENCES_RESPONSE:
    PlaceResponseP->data.presences = (DoorsPresenceList *) Null;
    if (!fromFcpTransPresenceList(
           (P+2), &(PlaceResponseP->data.presences))) {
      return(False);
    }
    break;
  case DOORS_PLACE_OBJECT_CONNECTIONS_RESPONSE:
    {
      register heapP Pa = (P+2);

      deref_ptr(Pa);
      if (!(IsTpl(*Pa) && (Arity_of(*Pa) == 3))) {
	if (IsVar(*Pa)) {
	  sus_tbl_add(Pa);
	}
	else {
	  DoorsFcpError = DOORSFCP_fromFcpTransPlaceResponse;
	}
	return(False);
      }

      PlaceResponseP->data.place_object_connections_response.presence_id =
	(DoorsPresenceId) Null;
      if (!fromFcpTransPresenceId(
             (Pa+1),
	     &(PlaceResponseP->
	       data.place_object_connections_response.presence_id))) {
	return(False);
      }

      if (!fromFcpTransObjectId(
	     (Pa+2),
             &(PlaceResponseP->
	       data.place_object_connections_response.object_id))) {
	return(False);
      }

      PlaceResponseP->data.place_object_connections_response.presences =
	(DoorsPresenceList *) Null;
      if (!fromFcpTransPresenceList(
	     (Pa+3),
	     &(PlaceResponseP->
	       data.place_object_connections_response.presences))) {
	return(False);
      }

    }
    break;
  }

  *CPlaceResponse = PlaceResponseP;
  return(True);
}

int fromFcpTransServerResponse(ServerResponse, CServerResponse)
     heapP ServerResponse;
     DoorsServerResponse **CServerResponse;
{
  register heapP P = ServerResponse;
  register DoorsServerResponse *ServerResponseP = *CServerResponse;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 2)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransServerResponse;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (ServerResponseP == ((DoorsServerResponse *) Null)) {
      return(True);
    }
  }

  if (ServerResponseP == ((DoorsServerResponse *) Null)) {
    drsfcptc_allocate(sizeof(DoorsServerResponse), (DoorsServerResponse *),
		      ServerResponseP);
  }
  
  if (!fromFcpTransServerResponseType((P+1), &(ServerResponseP->type))) {
    return(False);
  }

  switch(ServerResponseP->type) {
  case DOORS_SERVER_PRESENCES_COUNT_RESPONSE:
    if (!fromFcpTransUlong(
           (P+2), &(ServerResponseP->data.presences_number))) {
      return(False);
    }
    break;
  case DOORS_SERVER_PRESENCES_LIST_RESPONSE:
    ServerResponseP->data.place_presence_list =
      (DoorsPlacePresenceList *) Null;
    if (!fromFcpTransPlacePresenceList(
           (P+2), &(ServerResponseP->data.place_presence_list))) {
      return(False);
    }
    break;
  }

  *CServerResponse = ServerResponseP;
  return(True);
}

int fromFcpTransResponse(Response, CResponse)
     heapP Response;
     DoorsResponse **CResponse;
{
  register heapP P = Response;
  register DoorsResponse *ResponseP = *CResponse;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == 3)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_fromFcpTransResponse;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (ResponseP == ((DoorsResponse *) Null)) {
      return(True);
    }
  }

  if (ResponseP == ((DoorsResponse *) Null)) {
    drsfcptc_allocate(sizeof(DoorsResponse), (DoorsResponse *), ResponseP);
  }
  
  ResponseP->client_data = (DoorsBytes *) Null;
  if (!fromFcpTransBytes((P+1), &(ResponseP->client_data))) {
    return(False);
  }

  if (!fromFcpTransResponseCategory((P+2), &(ResponseP->category))) {
    return(False);
  }

  switch(ResponseP->category) {
  case DOORS_USER_RESPONSE_CATEGORY:
    ResponseP->content.user_response = (DoorsUserResponse *) Null;
    if (!fromFcpTransUserResponse(
           (P+3), &(ResponseP->content.user_response))) {
      return(False);
    }
    break;
  case DOORS_PLACE_RESPONSE_CATEGORY:
    ResponseP->content.place_response = (DoorsPlaceResponse *) Null;
    if (!fromFcpTransPlaceResponse(
           (P+3), &(ResponseP->content.place_response))) {
      return(False);
    }
    break;
  case DOORS_SERVER_RESPONSE_CATEGORY:
    ResponseP->content.server_response = (DoorsServerResponse *) Null;
    if (!fromFcpTransServerResponse(
           (P+3), &(ResponseP->content.server_response))) {
      return(False);
    }
    break;
  }

  *CResponse = ResponseP;
  return(True);
}

/* doorsRes.h - end */

#ifdef A13434234

/* Translate list template */

int fromFcpTrans@(@, C@)
     heapP @;
     Doors@ **C@;
{
  register heapP P = @;
  Doors@ *First@P = ((Doors@ *) Null);
  Doors@ *Last@P = ((Doors@ *) Null);
  register Doors@ *@P = *C@;

  P = @;
  deref_ptr(P);
  if (!(IsNil(*P) || IsList(*P))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_@;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (@P == ((Doors@ *) Null)) {
      return(True);
    }
  }

  while (IsList(*P)) {
    if (@P == ((Doors@ *) Null)) {
      drsfcptc_allocate(sizeof(Doors@), (Doors@ *),   @P);
      @P->next = ((Doors@ *) Null);
    }

    {
      register heapP Pa = P;
      register heapT Va = Off_List(*Pa);

      deref(Va, Pa);
      trans contents;
    }

    if (First@P == ((Doors@ *) Null)) {
      First@P = @P;
    }
    if (Last@P == ((Doors@ *) Null)) {
      Last@P = @P;
    }
    else {
      Last@P->next = @P;
      Last@P = @P;
    }
    @P = @P->next;

    P = Cdr(P);
    deref_ptr(P);
  }
  if (!IsNil(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_@;
    }
    return(False);
  }

  if (First@P != ((Doors@ *) Null)) {
    Last@P->next = ((Doors@ *) Null);
  }
  *C@ = First@P;
  return(True);
}

/* Translate structure template */

int fromFcpTrans@(@, C@)
     heapP @;
     Doors@ **C@;
{
  register heapP P = @;
  register Doors@ *@P = *C@;

  deref_ptr(P);
  if (!(IsNil(*P) || (IsTpl(*P) && (Arity_of(*P) == #)))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    else {
      DoorsFcpError = DOORSFCP_@;
    }
    return(False);
  }

  if (IsNil(*P)) {
    if (@P == ((Doors@ *) Null)) {
      return(True);
    }
  }

  if (@P == ((Doors@ *) Null)) {
    drsfcptc_allocate(sizeof(Doors@), (Doors@ *), @P);
  }
  
  *C@ = @P;
  return(True);
}

#endif
