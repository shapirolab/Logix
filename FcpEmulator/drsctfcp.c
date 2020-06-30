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

#include <string.h>

#include "doors.h"

#include "fcp.h"
#include "codes.h"
#include "global.h"
#include "macros.h"

#include "doorsfcp.h"
#include "doorsvar.h"

/* doorsTyp.h - start */

int toFcpTransBoolean(Boolean, FCPBoolean)
     DoorsBoolean Boolean;
     heapP FCPBoolean;
{
  switch (Boolean) {
  case DOORS_FALSE:
  case DOORS_TRUE:
    *FCPBoolean = Word(Boolean, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransBoolean;
    return(False);
  }
}

int toFcpTransFaceState(FaceState, FCPFaceState)
     DoorsFaceState FaceState;
     heapP FCPFaceState;
{
  switch (FaceState) {
  case DOORS_FACE_PRESENT:
  case DOORS_FACE_CONVERSING:
  case DOORS_FACE_ICONIZED:
  case DOORS_FACE_DISABLED:
    *FCPFaceState = Word(FaceState, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransFaceState;
    return(False);
  }
}

int toFcpTransConnectionType(ConnectionType, FCPConnectionType)
     DoorsConnectionType ConnectionType;
     heapP FCPConnectionType;
{
  switch (ConnectionType) {
  case DOORS_CONNECTION_NONE:
  case DOORS_CONNECTION_TO_OBJECT:
  case DOORS_CONNECTION_TO_PRESENCE:
    *FCPConnectionType = Word(ConnectionType, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransConnectionType;
    return(False);
  }
}

int toFcpTransConversationType(ConversationType, FCPConversationType)
     DoorsConversationType ConversationType;
     heapP FCPConversationType;
{
  switch (ConversationType) {
  case DOORS_CONVERSATION_NONE:
  case DOORS_CONVERSATION_CONFERENCE:
  case DOORS_CONVERSATION_LECTURE:
    *FCPConversationType = Word(ConversationType, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransConversationType;
    return(False);
  }
}

int toFcpTransObjectType(ObjectType, FCPObjectType)
     DoorsObjectType ObjectType;
     heapP FCPObjectType;
{
  switch (ObjectType) {
  case DOORS_OBJECT_TRANSPARENT_BUS:
  case DOORS_OBJECT_OPAQUE_BUS:
    *FCPObjectType = Word(ObjectType, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransObjectType;
    return(False);
  }
}

int toFcpTransPositionType(PositionType, FCPPositionType)
     DoorsPositionType PositionType;
     heapP FCPPositionType;
{
  switch (PositionType) {
  case DOORS_POSITION_NONE:
  case DOORS_POSITION_DOCUMENT:
  case DOORS_POSITION_GALLERY:
    *FCPPositionType = Word(PositionType, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransPositionType;
    return(False);
  }
}

int toFcpTransAnchorType(AnchorType, FCPAnchorType)
     DoorsAnchorType AnchorType;
     heapP FCPAnchorType;
{
  switch (AnchorType) {
  case DOORS_ANCHOR_HTML:
  case DOORS_ANCHOR_RELATIVE:
  case DOORS_ANCHOR_PRESENCE:
    *FCPAnchorType = Word(AnchorType, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransAnchorType;
    return(False);
  }
}

int toFcpTransHtmlAnchorType(HtmlAnchorType, FCPHtmlAnchorType)
     DoorsHtmlAnchorType HtmlAnchorType;
     heapP FCPHtmlAnchorType;
{
  switch (HtmlAnchorType) {
  case DOORS_HTML_ANCHOR_IMAGE:
  case DOORS_HTML_ANCHOR_CHAR:
    *FCPHtmlAnchorType = Word(HtmlAnchorType, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransHtmlAnchorType;
    return(False);
  }
}

int toFcpTransDestination(Destination, FCPDestination)
     DoorsDestination Destination;
     heapP FCPDestination;
{
  switch (Destination) {
  case DOORS_SERVER:
  case DOORS_PLACE:
  case DOORS_CONVERSATION:
  case DOORS_LIST:
    *FCPDestination = Word(Destination, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransDestination;
    return(False);
  }
}

int toFcpTransAudioOutputDevice(AudioOutputDevice,
				FCPAudioOutputDevice)
     DoorsAudioOutputDevice AudioOutputDevice;
     heapP FCPAudioOutputDevice;
{
  switch (AudioOutputDevice) {
  case DOORS_LINE:
  case DOORS_HEADPHONE:
  case DOORS_DIGITAL:
  case DOORS_SPEAKER:
    *FCPAudioOutputDevice = Word(AudioOutputDevice, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransAudioOutputDevice;
    return(False);
  }
}

int toFcpTransChars(Chars, FCPChars)
     DoorsChars Chars;
     heapP FCPChars;
{
  if (Chars == ((DoorsChars) Null)) {
    *FCPChars = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(str_total_words(strlen(Chars)));

  *FCPChars = Ref_Word(produce_string(Chars));
  return(True);
}

int toFcpTransString(String, FCPString)
     DoorsString String;
     heapP FCPString;
{
  if (String == ((DoorsString) Null)) {
    *FCPString = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(str_total_words(strlen(String)));

  *FCPString = Ref_Word(produce_string(String));
  return(True);
}

int toFcpTransInteger(Integer, FCPInteger)
     DoorsInteger Integer;
     heapP FCPInteger;
{
  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;
    register char *PChar = (char *) (HP+StrHdrWords);
    register int StrLen = 0;

    sprintf(PChar, "%d", Integer);
    while (*PChar++ != '\0') {
      StrLen++;
    }
    *HP++ = Str_Hdr1(CharType, 0);
    *HP = Str_Hdr2(hash((char *)(HP+1), StrLen), StrLen);
    HP += 1 + Str_Words((HP-1));
    while (PChar < (char *) HP) {
      *PChar++ = '\0';
    }
    *FCPInteger = Ref_Word(P);
  }

  return(True);
}

int toFcpTransLong(Long, FCPLong)
     DoorsLong Long;
     heapP FCPLong;
{
  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;
    register char *PChar = (char *) (HP+StrHdrWords);
    register int StrLen = 0;

    sprintf(PChar, "%ld", Long);
    while (*PChar++ != '\0') {
      StrLen++;
    }
    *HP++ = Str_Hdr1(CharType, 0);
    *HP = Str_Hdr2(hash((char *)(HP+1), StrLen), StrLen);
    HP += 1 + Str_Words((HP-1));
    while (PChar < (char *) HP) {
      *PChar++ = '\0';
    }
    *FCPLong = Ref_Word(P);
  }

  return(True);
}

int toFcpTransUlong(Ulong, FCPUlong)
     DoorsUlong Ulong;
     heapP FCPUlong;
{
  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;
    register char *PChar = (char *) (HP+StrHdrWords);
    register int StrLen = 0;

    sprintf(PChar, "%lu", Ulong);
    while (*PChar++ != '\0') {
      StrLen++;
    }
    *HP++ = Str_Hdr1(CharType, 0);
    *HP = Str_Hdr2(hash((char *)(HP+1), StrLen), StrLen);
    HP += 1 + Str_Words((HP-1));
    while (PChar < (char *) HP) {
      *PChar++ = '\0';
    }
    *FCPUlong = Ref_Word(P);
  }

  return(True);
}

int toFcpTransVersion(Version, FCPVersion)
     DoorsVersion Version;
     heapP FCPVersion;
{
  if (Version == ((DoorsVersion) Null)) {
    *FCPVersion = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(str_total_words(strlen(Version)));

  *FCPVersion = Ref_Word(produce_string(Version));
  return(True);
}

int toFcpTransNodeId(NodeId, FCPNodeId)
     DoorsNodeId NodeId;
     heapP FCPNodeId;
{
  if (NodeId == ((DoorsNodeId) Null)) {
    *FCPNodeId = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(str_total_words(strlen(NodeId)));

  *FCPNodeId = Ref_Word(produce_string(NodeId));
  return(True);
}

int toFcpTransPortId(PortId, FCPPortId)
     DoorsPortId PortId;
     heapP FCPPortId;
{
  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;
    register char *PChar = (char *) (HP+StrHdrWords);
    register int StrLen = 0;

    sprintf(PChar, "%ld", PortId);
    while (*PChar++ != '\0') {
      StrLen++;
    }
    *HP++ = Str_Hdr1(CharType, 0);
    *HP = Str_Hdr2(hash((char *)(HP+1), StrLen), StrLen);
    HP += 1 + Str_Words((HP-1));
    while (PChar < (char *) HP) {
      *PChar++ = '\0';
    }
    *FCPPortId = Ref_Word(P);
  }

  return(True);
}

int toFcpTransPlaceId(PlaceId, FCPPlaceId)
     DoorsPlaceId PlaceId;
     heapP FCPPlaceId;
{
  if (PlaceId == ((DoorsPlaceId) Null)) {
    *FCPPlaceId = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(str_total_words(strlen(PlaceId)));

  *FCPPlaceId = Ref_Word(produce_string(PlaceId));
  return(True);
}

int toFcpTransMboneChannel(MboneChannel, FCPMboneChannel)
     DoorsMboneChannel MboneChannel;
     heapP FCPMboneChannel;
{
  if (MboneChannel == ((DoorsMboneChannel) Null)) {
    *FCPMboneChannel = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(str_total_words(strlen(MboneChannel)));

  *FCPMboneChannel = Ref_Word(produce_string(MboneChannel));
  return(True);
}

int toFcpTransPresenceId(PresenceId, FCPPresenceId)
     DoorsPresenceId PresenceId;
     heapP FCPPresenceId;
{
  if (PresenceId == ((DoorsPresenceId) Null)) {
    *FCPPresenceId = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(str_total_words(strlen(PresenceId)));

  *FCPPresenceId = Ref_Word(produce_string(PresenceId));
  return(True);
}

int toFcpTransObjectId(ObjectId, FCPObjectId)
     DoorsObjectId ObjectId;
     heapP FCPObjectId;
{
  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;
    register char *PChar = (char *) (HP+StrHdrWords);
    register int StrLen = 0;

    sprintf(PChar, "%lu", ObjectId);
    while (*PChar++ != '\0') {
      StrLen++;
    }
    *HP++ = Str_Hdr1(CharType, 0);
    *HP = Str_Hdr2(hash((char *)(HP+1), StrLen), StrLen);
    HP += 1 + Str_Words((HP-1));
    while (PChar < (char *) HP) {
      *PChar++ = '\0';
    }
    *FCPObjectId = Ref_Word(P);
  }

  return(True);
}

int toFcpTransBytes(Bytes, FCPBytes)
     DoorsBytes *Bytes;
     heapP FCPBytes;
{
  if (Bytes == ((DoorsBytes *) Null)) {
    *FCPBytes = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory((Bytes->length));

  {
    register heapP P = HP;
    register char *PCharD = (char *) (HP+StrHdrWords);
    register char *PCharS = (char *) Bytes->data;
    register int StrLen = Bytes->length;

    while (StrLen > 0) {
      *PCharD++ = *PCharS++;
      StrLen--;
    }
    
    StrLen = Bytes->length;
    *HP++ = Str_Hdr1(CharType, 0);
    *HP = Str_Hdr2(hash((char *)(HP+1), StrLen), StrLen);
    HP += 1 + Str_Words((HP-1));
    while (PCharD < (char *) HP) {
      *PCharD++ = '\0';
    }
    *FCPBytes = Ref_Word(P);
  }

  return(True);
}

int toFcpTransFace(Face, FCPFace)
     DoorsFace *Face;
     heapP FCPFace;
{
  if (Face == ((DoorsFace *) Null)) {
    *FCPFace = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory((Face->length));

  {
    register heapP P = HP;
    register char *PCharD = (char *) (HP+StrHdrWords);
    register char *PCharS = (char *) Face->data;
    register int StrLen = Face->length;

    while (StrLen > 0) {
      *PCharD++ = *PCharS++;
      StrLen--;
    }
    
    StrLen = Face->length;
    *HP++ = Str_Hdr1(CharType, 0);
    *HP = Str_Hdr2(hash((char *)(HP+1), StrLen), StrLen);
    HP += 1 + Str_Words((HP-1));
    while (PCharD < (char *) HP) {
      *PCharD++ = '\0';
    }
    *FCPFace = Ref_Word(P);
  }

  return(True);
}

int toFcpTransDate(Date, FCPDate)
     DoorsDate *Date;
     heapP FCPDate;
{
  if (Date == ((DoorsDate *) Null)) {
    *FCPDate = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;
    register char *PChar = (char *) (HP+StrHdrWords);
    register int StrLen = 0;

    sprintf(PChar, "%12ld.%06ld", Date->seconds, Date->micro_seconds);
    while (*PChar++ != '\0') {
      StrLen++;
    }
    *HP++ = Str_Hdr1(CharType, 0);
    *HP = Str_Hdr2(hash((char *)(HP+1), StrLen), StrLen);
    HP += 1 + Str_Words((HP-1));
    while (PChar < (char *) HP) {
      *PChar++ = '\0';
    }
    *FCPDate = Ref_Word(P);
  }

  return(True);
}

int toFcpTransDoor(Door, FCPDoor)
     DoorsDoor *Door;
     heapP FCPDoor;
{
  if (Door == ((DoorsDoor *) Null)) {
    *FCPDoor = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 4;

    *(P+0) = Word(3, TplTag);

    if (!toFcpTransNodeId((Door->server), (P+1))) {
      return(False);
    }

    if (!toFcpTransPortId((Door->port), (P+2))) {
      return(False);
    }

    if (!toFcpTransPlaceId((Door->place), (P+3))) {
      return(False);
    }

    *FCPDoor = Ref_Word(P);
  }
  return(True);
}

int toFcpTransPresenceInfo(PresenceInfo, FCPPresenceInfo)
     DoorsPresenceInfo *PresenceInfo;
     heapP FCPPresenceInfo;
{
  if (PresenceInfo == ((DoorsPresenceInfo *) Null)) {
    *FCPPresenceInfo = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 6;

    *(P+0) = Word(5, TplTag);

    if (!toFcpTransString((PresenceInfo->login_name), (P+1))) {
      return(False);
    }

    if (!toFcpTransString((PresenceInfo->domain), (P+2))) {
      return(False);
    }

    if (!toFcpTransString((PresenceInfo->client_node), (P+3))) {
      return(False);
    }

    if (!toFcpTransUlong((PresenceInfo->client_id), (P+4))) {
      return(False);
    }

    if (!toFcpTransDate((PresenceInfo->presence_date), (P+5))) {
      return(False);
    }

    *FCPPresenceInfo = Ref_Word(P);
  }
  return(True);
}

int toFcpTransHtmlAnchor(HtmlAnchor, FCPHtmlAnchor)
     DoorsHtmlAnchor *HtmlAnchor;
     heapP FCPHtmlAnchor;
{
  if (HtmlAnchor == ((DoorsHtmlAnchor *) Null)) {
    *FCPHtmlAnchor = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 7;

    *(P+0) = Word(6, TplTag);

    if (!toFcpTransHtmlAnchorType((HtmlAnchor->type), (P+1))) {
      return(False);
    }

    if (!toFcpTransInteger((HtmlAnchor->number), (P+2))) {
      return(False);
    }

    if (!toFcpTransBoolean((HtmlAnchor->inside), (P+3))) {
      return(False);
    }

    if (!toFcpTransProportionalOffset(&(HtmlAnchor->prop_offset),
					(P+4))) {
      return(False);
    }

    if (!toFcpTransAbsoluteOffset(&(HtmlAnchor->abs_offset), (P+5))) {
      return(False);
    }

    if (!toFcpTransProportionalOffset(&(HtmlAnchor->rel_offset), (P+6))) {
      return(False);
    }

    *FCPHtmlAnchor = Ref_Word(P);
  }
  return(True);
}

int toFcpTransPresenceAnchor(PresenceAnchor, FCPPresenceAnchor)
     DoorsPresenceAnchor *PresenceAnchor;
     heapP FCPPresenceAnchor;
{
  if (PresenceAnchor == ((DoorsPresenceAnchor *) Null)) {
    *FCPPresenceAnchor = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 5;

    *(P+0) = Word(4, TplTag);

    if (!toFcpTransPresenceId((PresenceAnchor->presence_id), (P+1))) {
      return(False);
    }

    if (!toFcpTransObjectId((PresenceAnchor->object_id), (P+2))) {
      return(False);
    }

    if (!toFcpTransProportionalOffset(&(PresenceAnchor->prop_offset), (P+3))) {
      return(False);
    }

    if (!toFcpTransAbsoluteOffset(&(PresenceAnchor->abs_offset), (P+4))) {
      return(False);
    }

    *FCPPresenceAnchor = Ref_Word(P);
  }
  return(True);
}

int toFcpTransRelativeAnchor(RelativeAnchor, FCPRelativeAnchor)
     DoorsRelativeAnchor *RelativeAnchor;
     heapP FCPRelativeAnchor;
{
  if (RelativeAnchor == ((DoorsRelativeAnchor *) Null)) {
    *FCPRelativeAnchor = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 3;

    *(P+0) = Word(2, TplTag);

    if (!toFcpTransProportionalOffset(&(RelativeAnchor->rel_offset), (P+1))) {
      return(False);
    }

    if (!toFcpTransAbsoluteOffset(&(RelativeAnchor->abs_offset), (P+2))) {
      return(False);
    }

    *FCPRelativeAnchor = Ref_Word(P);
  }
  return(True);
}

int toFcpTransAbsoluteOffset(AbsoluteOffset, FCPAbsoluteOffset)
     DoorsAbsoluteOffset *AbsoluteOffset;
     heapP FCPAbsoluteOffset;
{
  if (AbsoluteOffset == ((DoorsAbsoluteOffset *) Null)) {
    *FCPAbsoluteOffset = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 4;

    *(P+0) = Word(3, TplTag);

    if (!toFcpTransUlong((AbsoluteOffset->x), (P+1))) {
      return(False);
    }

    if (!toFcpTransUlong((AbsoluteOffset->y), (P+2))) {
      return(False);
    }

    if (!toFcpTransUlong((AbsoluteOffset->z), (P+3))) {
      return(False);
    }

    *FCPAbsoluteOffset = Ref_Word(P);
  }
  return(True);
}

int toFcpTransProportionalOffset(ProportionalOffset, FCPProportionalOffset)
     DoorsProportionalOffset *ProportionalOffset;
     heapP FCPProportionalOffset;
{
  if (ProportionalOffset == ((DoorsProportionalOffset *) Null)) {
    *FCPProportionalOffset = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 4;

    *(P+0) = Word(3, TplTag);

    if (!toFcpTransUlong((ProportionalOffset->x), (P+1))) {
      return(False);
    }

    if (!toFcpTransUlong((ProportionalOffset->y), (P+2))) {
      return(False);
    }

    if (!toFcpTransUlong((ProportionalOffset->z), (P+3))) {
      return(False);
    }

    *FCPProportionalOffset = Ref_Word(P);
  }
  return(True);
}

int toFcpTransPosition(Position, FCPPosition)
     DoorsPosition *Position;
     heapP FCPPosition;
{
  if (Position == ((DoorsPosition *) Null)) {
    *FCPPosition = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 5;

    *(P+0) = Word(4, TplTag);

    if (!toFcpTransPositionType((Position->position_type), (P+1))) {
      return(False);
    }

    if (!toFcpTransAnchorType((Position->anchor_type), (P+2))) {
      return(False);
    }

    if (!toFcpTransDoor((Position->door), (P+3))) {
      return(False);
    }

    switch (Position->anchor_type) {
    case DOORS_ANCHOR_HTML:
      if (!toFcpTransHtmlAnchor(&(Position->anchor.html), (P+4))) {
	return(False);
      }
      break;
    case DOORS_ANCHOR_PRESENCE:
      if (!toFcpTransPresenceAnchor(&(Position->anchor.presence), (P+4))) {
	return(False);
      }
      break;
    case DOORS_ANCHOR_RELATIVE:
      if (!toFcpTransRelativeAnchor(&(Position->anchor.relative), (P+4))) {
	return(False);
      }
      break;
    }

    *FCPPosition = Ref_Word(P);
  }
  return(True);
}

int toFcpTransObjectState(ObjectState, FCPObjectState)
     DoorsObjectState *ObjectState;
     heapP FCPObjectState;
{
  if (ObjectState == ((DoorsObjectState *) Null)) {
    *FCPObjectState = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 10;

    *(P+0) = Word(9, TplTag);

    if (!toFcpTransObjectType((ObjectState->type), (P+1))) {
      return(False);
    }

    if (!toFcpTransString((ObjectState->name), (P+2))) {
      return(False);
    }

    if (!toFcpTransPosition((ObjectState->position), (P+3))) {
      return(False);
    }

    if (!toFcpTransDate((ObjectState->date), (P+4))) {
      return(False);
    }

    if (!toFcpTransBoolean((ObjectState->has_icon), (P+5))) {
      return(False);
    }

    if (!toFcpTransConversationType((ObjectState->conversation), (P+6))) {
      return(False);
    }

    if (!toFcpTransInteger((ObjectState->max_capacity), (P+7))) {
      return(False);
    }

    if (!toFcpTransInteger((ObjectState->number_used), (P+8))) {
      return(False);
    }

    if (!toFcpTransMboneChannel((ObjectState->mbone_channel), (P+9))) {
      return(False);
    }

    *FCPObjectState = Ref_Word(P);
  }
  return(True);
}

int toFcpTransObjectInPresence(ObjectInPresence, FCPObjectInPresence)
     DoorsObjectInPresence *ObjectInPresence;
     heapP FCPObjectInPresence;
{
  register DoorsObjectInPresence *ObjectInPresenceP = ObjectInPresence;

  register heapP First = Null, Last = Null;

  while (ObjectInPresenceP != ((DoorsObjectInPresence *) Null)) {
    register heapP P;
    
    doorsfcp_check_fcp_memory(1);
    
    P = HP;
    if (First == Null) {
      First = HP;
    }
    
    HP += 2;
    
    {
      register heapP Pa = HP;

      HP += 3;

      *(Pa+0) = Word(2, TplTag);

      if (!toFcpTransObjectId((ObjectInPresenceP->object_id), (Pa+1))) {
	return(False);
      }

      if (!toFcpTransObjectState((ObjectInPresenceP->object_state), (Pa+2))) {
	return(False);
      }
    
      *(P+0) = L_Ref_Word(Pa);
    }

    if (Last != Null) {
      *Last = Ref_Word(P);
    }
    Last = (P+1);
    ObjectInPresenceP = ObjectInPresenceP->next;
  }

  if (Last == Null) {
    *FCPObjectInPresence = Word(0, NilTag);
  }
  else {
    *Last = Word(0, NilTag);
    *FCPObjectInPresence = Ref_Word(First);
  }
  return(True);
}

int toFcpTransConnection(Connection, FCPConnection)
     DoorsConnection *Connection;
     heapP FCPConnection;
{
  if (Connection == ((DoorsConnection *) Null)) {
    *FCPConnection = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 4;

    *(P+0) = Word(3, TplTag);

    if (!toFcpTransPresenceId((Connection->presence_id), (P+1))) {
      return(False);
    }

    if (!toFcpTransObjectId((Connection->object_id), (P+2))) {
      return(False);
    }

    if (!toFcpTransConnectionType((Connection->type), (P+3))) {
      return(False);
    }

    *FCPConnection = Ref_Word(P);
  }
  return(True);
}

int toFcpTransPresenceState(PresenceState, FCPPresenceState)
     DoorsPresenceState *PresenceState;
     heapP FCPPresenceState;
{
  if (PresenceState == ((DoorsPresenceState *) Null)) {
    *FCPPresenceState = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 16;

    *(P+0) = Word(15, TplTag);

    if (!toFcpTransString((PresenceState->nick_name), (P+1))) {
      return(False);
    }

    if (!toFcpTransVersion((PresenceState->version), (P+2))) {
      return(False);
    }

    if (!toFcpTransPosition((PresenceState->position), (P+3))) {
      return(False);
    }

    if (!toFcpTransPosition((PresenceState->click_position), (P+4))) {
      return(False);
    }

    if (!toFcpTransDoor((PresenceState->click_door), (P+5))) {
      return(False);
    }

    if (!toFcpTransFaceState((PresenceState->state), (P+6))) {
      return(False);
    }

    if (!toFcpTransDate((PresenceState->edit_date), (P+7))) {
      return(False);
    }

    if (!toFcpTransDate((PresenceState->alert_date), (P+8))) {
      return(False);
    }

    if (!toFcpTransDate((PresenceState->joined_date), (P+9))) {
      return(False);
    }

    if (!toFcpTransDate((PresenceState->date), (P+10))) {
      return(False);
    }

    if (!toFcpTransBoolean((PresenceState->has_icon), (P+11))) {
      return(False);
    }

    if (!toFcpTransObjectInPresence((PresenceState->objects), (P+12))) {
      return(False);
    }

    if (!toFcpTransConnection((PresenceState->connection), (P+13))) {
      return(False);
    }

    if (!toFcpTransUlong((PresenceState->audio_port), (P+14))) {
      return(False);
    }

    if (!toFcpTransBoolean((PresenceState->audio_focus), (P+15))) {
      return(False);
    }

    *FCPPresenceState = Ref_Word(P);
  }
  return(True);
}

int toFcpTransPlaceState(PlaceState, FCPPlaceState)
     DoorsPlaceState *PlaceState;
     heapP FCPPlaceState;
{
  if (PlaceState == ((DoorsPlaceState *) Null)) {
    *FCPPlaceState = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 5;

    *(P+0) = Word(4, TplTag);

    if (!toFcpTransUlong((PlaceState->document_size), (P+1))) {
      return(False);
    }

    if (!toFcpTransUlong((PlaceState->document_hash), (P+2))) {
      return(False);
    }

    if (!toFcpTransDate((PlaceState->document_load_date), (P+3))) {
      return(False);
    }

    if (!toFcpTransDate((PlaceState->document_date), (P+4))) {
      return(False);
    }

    *FCPPlaceState = Ref_Word(P);
  }
  return(True);
}

int toFcpTransPresenceList(PresenceList, FCPPresenceList)
     DoorsPresenceList *PresenceList;
     heapP FCPPresenceList;
{
  register DoorsPresenceList *PresenceListP = PresenceList;

  register heapP First = Null, Last = Null;

  while (PresenceListP != ((DoorsPresenceList *) Null)) {
    register heapP P;
    
    doorsfcp_check_fcp_memory(1);
    
    P = HP;
    if (First == Null) {
      First = HP;
    }
    
    HP += 2;
    
    if (!toFcpTransPresenceId((PresenceListP->id), (P+0))) {
      return(False);
    }
    
    *(P+0) = Set_List((*(P+0)));

    if (Last != Null) {
      *Last = Ref_Word(P);
    }
    Last = (P+1);
    PresenceListP = PresenceListP->next;
  }

  if (Last == Null) {
    *FCPPresenceList = Word(0, NilTag);
  }
  else {
    *Last = Word(0, NilTag);
    *FCPPresenceList = Ref_Word(First);
  }
  return(True);
}

int toFcpTransHeader(Header, FCPHeader)
     DoorsHeader *Header;
     heapP FCPHeader;
{
  if (Header == ((DoorsHeader *) Null)) {
    *FCPHeader = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 6;

    *(P+0) = Word(5, TplTag);

    if (!toFcpTransPresenceId((Header->id), (P+1))) {
      return(False);
    }

    if (!toFcpTransDestination((Header->destination), (P+2))) {
      return(False);
    }

    if (!toFcpTransPresenceList((Header->list), (P+3))) {
      return(False);
    }

    if (!toFcpTransDoor((Header->door), (P+4))) {
      return(False);
    }

    if (!toFcpTransDate(&(Header->date), (P+5))) {
      return(False);
    }

    *FCPHeader = Ref_Word(P);
  }
  return(True);
}


int toFcpTransClientCapabilities(ClientCapabilities, FCPClientCapabilities)
     DoorsClientCapabilities *ClientCapabilities;
     heapP FCPClientCapabilities;
{
  if (ClientCapabilities == ((DoorsClientCapabilities *) Null)) {
    *FCPClientCapabilities = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 2;

    *(P+0) = Word(2, TplTag);

    if (!toFcpTransBoolean((ClientCapabilities->audio_enabled), (P+1))) {
      return(False);
    }

    if (!toFcpTransBoolean((ClientCapabilities->multicast_supported), (P+2))) {
      return(False);
    }

    *FCPClientCapabilities = Ref_Word(P);
  }
  return(True);
}

int toFcpTransClubTicketCapabilities(ClubTicketCapabilities,
				     FCPClubTicketCapabilities)
     DoorsClubTicketCapabilities *ClubTicketCapabilities;
     heapP FCPClubTicketCapabilities;
{
  if (ClubTicketCapabilities == ((DoorsClubTicketCapabilities *) Null)) {
    *FCPClubTicketCapabilities = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 2;

    *(P+0) = Word(1, TplTag);

    if (!toFcpTransUlong((ClubTicketCapabilities->bus_capacity), (P+1))) {
      return(False);
    }

    *FCPClubTicketCapabilities = Ref_Word(P);
  }
  return(True);
}

/* doorsTyp.h - end */

/* doorsRc.h - start */

int toFcpTransErrorVal(ErrorVal, FCPErrorVal)
     DoorsErrorVal ErrorVal;
     heapP FCPErrorVal;
{
  switch (ErrorVal) {
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
    *FCPErrorVal = Word(ErrorVal, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransErrorVal;
    return(False);
  }
}

int toFcpTransWarning(Warning, FCPWarning)
     DoorsWarning Warning;
     heapP FCPWarning;
{
  switch (Warning) {
  case DOORS_NO_WARNING:
  case DOORS_TOKEN_EXPIRES_SOON:
    *FCPWarning = Word(Warning, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransWarning;
    return(False);
  }
}

/* doorsRc.h - end */

/* doorsErr.h - start */

int toFcpTransErrorCategory(ErrorCategory, FCPErrorCategory)
     DoorsErrorCategory ErrorCategory;
     heapP FCPErrorCategory;
{
  switch (ErrorCategory) {
  case DOORS_PLACE_CONNECT_ERROR_CATEGORY:
  case DOORS_INTERFACE_ERROR_CATEGORY:
  case DOORS_AUDIO_ERROR_CATEGORY:
    *FCPErrorCategory = Word(ErrorCategory, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransErrorCategory;
    return(False);
  }
}

int toFcpTransPlaceConnectErrorType(PlaceConnectErrorType,
    FCPPlaceConnectErrorType)
     DoorsPlaceConnectErrorType PlaceConnectErrorType;
     heapP FCPPlaceConnectErrorType;
{
  switch (PlaceConnectErrorType) {
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
    *FCPPlaceConnectErrorType = Word(PlaceConnectErrorType, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransPlaceConnectErrorType;
    return(False);
  }
}

int toFcpTransInterfaceErrorType(InterfaceErrorType, FCPInterfaceErrorType)
     DoorsInterfaceErrorType InterfaceErrorType;
     heapP FCPInterfaceErrorType;
{
  switch (InterfaceErrorType) {
  case DOORS_INTERFACE_DISCONNECT_UNKNOWN_PRESENCE_ERROR:
  case DOORS_INTERFACE_VIEW_EVENTS_UNKNOWN_PRESENCE_ERROR:
  case DOORS_INTERFACE_GOT_EVENT_UNKNOWN_PRESENCE_ERROR:
  case DOORS_INTERFACE_QUERY_CACHE_UNKNOWN_PRESENCE_ERROR:
  case DOORS_INTERFACE_QUERY_PLACE_PRESENCES_UNKNOWN_PRESENCE_ERROR:
  case DOORS_INTERFACE_CREATE_OBJECT_NO_USER_ERROR:
  case DOORS_INTERFACE_CONNECT_TO_PRESENCE_UNKNOWN_PRESENCE_ERROR:
  case DOORS_INTERFACE_CONNECT_TO_OBJECT_UNKNOWN_PRESENCE_ERROR:
  case DOORS_INTERFACE_QUERY_OBJECT_CONNECTIONS_UNKNOWN_PRESENCE_ERROR:
    *FCPInterfaceErrorType = Word(InterfaceErrorType, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransInterfaceErrorType;
    return(False);
  }
}

int toFcpTransAudioErrorType(AudioErrorType, FCPAudioErrorType)
     DoorsAudioErrorType AudioErrorType;
     heapP FCPAudioErrorType;
{
  switch (AudioErrorType) {
  case DOORS_AUDIO_NET_IO_ERROR:
  case DOORS_AUDIO_DEV_IO_ERROR:
  case DOORS_AUDIO_DEV_BUSY:
  case DOORS_AUDIO_DEV_CANT_OPEN:
  case DOORS_AUDIO_CONNECT_TO_SELF:
  case DOORS_AUDIO_OTHER_ERROR:
    *FCPAudioErrorType = Word(AudioErrorType, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransAudioErrorType;
    return(False);
  }
}

int toFcpTransErrorSeverity(ErrorSeverity, FCPErrorSeverity)
     DoorsErrorSeverity ErrorSeverity;
     heapP FCPErrorSeverity;
{
  switch (ErrorSeverity) {
  case DOORS_ERROR_SEVERITY_WARNING:
  case DOORS_ERROR_SEVERITY_ERROR:
  case DOORS_ERROR_SEVERITY_FATAL:
    *FCPErrorSeverity = Word(ErrorSeverity, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransErrorSeverity;
    return(False);
  }
}

int toFcpTransInterfaceError(InterfaceError, FCPInterfaceError)
     DoorsInterfaceError *InterfaceError;
     heapP FCPInterfaceError;
{
  if (InterfaceError == ((DoorsInterfaceError *) Null)) {
    *FCPInterfaceError = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 3;

    *(P+0) = Word(2, TplTag);

    if (!toFcpTransInterfaceErrorType((InterfaceError->type), (P+1))) {
      return(False);
    }

    if (!toFcpTransBytes((InterfaceError->client_data), (P+2))) {
      return(False);
    }

    *FCPInterfaceError = Ref_Word(P);
  }
  return(True);
}

int toFcpTransError(Error, FCPError)
     DoorsError *Error;
     heapP FCPError;
{
  if (Error == ((DoorsError *) Null)) {
    *FCPError = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 4;

    *(P+0) = Word(3, TplTag);

    if (!toFcpTransErrorSeverity((Error->severity), (P+1))) {
      return(False);
    }

    if (!toFcpTransErrorCategory((Error->category), (P+2))) {
      return(False);
    }

    switch (Error->category) {
    case DOORS_PLACE_CONNECT_ERROR_CATEGORY:
      if (!toFcpTransPlaceConnectErrorType(
            (Error->content.place_connect_error), (P+3))) {
	return(False);
      }
      break;
    case DOORS_INTERFACE_ERROR_CATEGORY:
      if (!toFcpTransInterfaceError((Error->content.interface_error), (P+3))) {
	return(False);
      }
      break;
    case DOORS_AUDIO_ERROR_CATEGORY:
      if (!toFcpTransAudioErrorType((Error->content.audio_error), (P+3))) {
	return(False);
      }
      break;
    }

    *FCPError = Ref_Word(P);
  }
  return(True);
}

/* doorsErr.h - end */

/* doorsEvn.h - start */

int toFcpTransEventCategory(EventCategory, FCPEventCategory)
     DoorsEventCategory EventCategory;
     heapP FCPEventCategory;
{
  switch (EventCategory) {
  case DOORS_PRESENCE_EVENT_CATEGORY:
  case DOORS_PLACE_EVENT_CATEGORY:
  case DOORS_TRANSIENT_EVENT_CATEGORY:
  case DOORS_ALERT_EVENT_CATEGORY:
  case DOORS_MESSAGE_EVENT_CATEGORY:
  case DOORS_OTHER_EVENT_CATEGORY:
  case DOORS_AUDIO_EVENT_CATEGORY:
    *FCPEventCategory = Word(EventCategory, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransEventCategory;
    return(False);
  }
}

int toFcpTransPresenceEventType(PresenceEventType, FCPPresenceEventType)
     DoorsPresenceEventType PresenceEventType;
     heapP FCPPresenceEventType;
{
  switch (PresenceEventType) {
  case DOORS_PRESENCE_ENTERED_EVENT:
  case DOORS_PRESENCE_LEFT_EVENT:
    *FCPPresenceEventType = Word(PresenceEventType, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransPresenceEventType;
    return(False);
  }
}

int toFcpTransPlaceEventType(PlaceEventType, FCPPlaceEventType)
     DoorsPlaceEventType PlaceEventType;
     heapP FCPPlaceEventType;
{
  switch (PlaceEventType) {
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
    *FCPPlaceEventType = Word(PlaceEventType, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransPlaceEventType;
    return(False);
  }
}

int toFcpTransTransientEventType(TransientEventType, FCPTransientEventType)
     DoorsTransientEventType TransientEventType;
     heapP FCPTransientEventType;
{
  switch (TransientEventType) {
  case DOORS_TRANSIENT_CLICKED_EVENT:
  case DOORS_TRANSIENT_MOVED_EVENT:
  case DOORS_TRANSIENT_FACE_STATE_EVENT:
  case DOORS_TRANSIENT_AUDIO_FOCUS_EVENT:
  case DOORS_TRANSIENT_CONVERSATION_TYPE_EVENT:
    *FCPTransientEventType = Word(TransientEventType, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransTransientEventType;
    return(False);
  }
}

int toFcpTransAlertEventType(AlertEventType, FCPAlertEventType)
     DoorsAlertEventType AlertEventType;
     heapP FCPAlertEventType;
{
  switch (AlertEventType) {
  case DOORS_ALERT_ALERT_EVENT:
    *FCPAlertEventType = Word(AlertEventType, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransAlertEventType;
    return(False);
  }
}

int toFcpTransMessageEventType(MessageEventType, FCPMessageEventType)
     DoorsMessageEventType MessageEventType;
     heapP FCPMessageEventType;
{
  switch (MessageEventType) {
  case DOORS_MESSAGE_TEXT_EVENT:
    *FCPMessageEventType = Word(MessageEventType, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransMessageEventType;
    return(False);
  }
}

int toFcpTransOtherEventType(OtherEventType, FCPOtherEventType)
     DoorsOtherEventType OtherEventType;
     heapP FCPOtherEventType;
{
  switch (OtherEventType) {
  case DOORS_OTHER_TEXT_EVENT:
    *FCPOtherEventType = Word(OtherEventType, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransOtherEventType;
    return(False);
  }
}

int toFcpTransAudioEventType(AudioEventType, FCPAudioEventType)
     DoorsAudioEventType AudioEventType;
     heapP FCPAudioEventType;
{
  switch (AudioEventType) {
  case DOORS_AUDIO_REMOTE_JOINED_EVENT:
  case DOORS_AUDIO_REMOTE_LEFT_EVENT:
    *FCPAudioEventType = Word(AudioEventType, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransAudioEventType;
    return(False);
  }
}

int toFcpTransCreateObjectFailure(CreateObjectFailure, FCPCreateObjectFailure)
     DoorsCreateObjectFailure CreateObjectFailure;
     heapP FCPCreateObjectFailure;
{
  switch (CreateObjectFailure) {
  case DOORS_CREATE_OBJECT_FAILED_ALREADY_DRIVER:
  case DOORS_CREATE_OBJECT_FAILED_ALREADY_PASSENGER:
  case DOORS_CREATE_OBJECT_FAILED_IN_ONE_TO_ONE:
  case DOORS_CREATE_OBJECT_UNAUTHORIZED:
  case DOORS_CREATE_OBJECT_TOO_BIG:
    *FCPCreateObjectFailure = Word(CreateObjectFailure, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransCreateObjectFailure;
    return(False);
  }
}

int toFcpTransConnectFailure(ConnectFailure, FCPConnectFailure)
     DoorsConnectFailure ConnectFailure;
     heapP FCPConnectFailure;
{
  switch (ConnectFailure) {
  case DOORS_CONNECT_FAILED_SERVER_ERROR:
  case DOORS_CONNECT_FAILED_NO_CONNECTEE:
  case DOORS_CONNECT_FAILED_BUSY:
  case DOORS_CONNECT_FAILED_MISMATCH:
  case DOORS_CONNECT_FAILED_NO_ROOM:
    *FCPConnectFailure = Word(ConnectFailure, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransConnectFailure;
    return(False);
  }
}

int toFcpTransPresenceEvent(PresenceEvent, FCPPresenceEvent)
     DoorsPresenceEvent *PresenceEvent;
     heapP FCPPresenceEvent;
{
  if (PresenceEvent == ((DoorsPresenceEvent *) Null)) {
    *FCPPresenceEvent = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 3;

    *(P+0) = Word(2, TplTag);

    if (!toFcpTransPresenceEventType((PresenceEvent->type), (P+1))) {
      return(False);
    }

    switch (PresenceEvent->type) {
    case DOORS_PRESENCE_ENTERED_EVENT:
      {
	register heapP Pa = HP;

	HP += 4;

	*(Pa+0) = Word(3, TplTag);

	if (!toFcpTransPlaceState(
               (PresenceEvent->data.presence_entered_event.place_state),
	       (Pa+1))) {
	  return(False);
	}

	if (!toFcpTransPresenceState(
	       (PresenceEvent->data.presence_entered_event.presence_state),
	       (Pa+2))) {
	  return(False);
	}

	if (!toFcpTransDoor((PresenceEvent->data.presence_entered_event.from),
			    (Pa+3))) {
	  return(False);
	}

	*(P+2) = Ref_Word(Pa);
      }
      break;
    case DOORS_PRESENCE_LEFT_EVENT:
      if (!toFcpTransDoor((PresenceEvent->data.left_to), (P+2))) {
	return(False);
      }
      break;
    }

    *FCPPresenceEvent = Ref_Word(P);
  }
  return(True);
}

int toFcpTransPlaceEvent(PlaceEvent, FCPPlaceEvent)
     DoorsPlaceEvent *PlaceEvent;
     heapP FCPPlaceEvent;
{
  if (PlaceEvent == ((DoorsPlaceEvent *) Null)) {
    *FCPPlaceEvent = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 3;

    *(P+0) = Word(2, TplTag);

    if (!toFcpTransPlaceEventType((PlaceEvent->type), (P+1))) {
      return(False);
    }

    switch (PlaceEvent->type) {
    case DOORS_PLACE_INVITE_EVENT:
    case DOORS_PLACE_UPDATE_EVENT:
    case DOORS_PLACE_EDIT_STARTED_EVENT:
    case DOORS_PLACE_EDIT_FAILED_EVENT:
    case DOORS_PLACE_EDIT_FINISHED_EVENT:
    case DOORS_PLACE_POST_EVENT:
    case DOORS_PLACE_MAIL_EVENT:
      if (!toFcpTransString((PlaceEvent->data.text), (P+2))) {
	return(False);
      }
      break;
    case DOORS_PLACE_CREATED_OBJECT_EVENT:
     {
        register heapP Pa = HP;

        HP += 3;

        *(Pa+0) = Word(2, TplTag);

        if (!toFcpTransObjectId(
              (PlaceEvent->data.place_created_object_event.object_id),
              (Pa+1))) {
          return(False);
        }

        if (!toFcpTransObjectState(
           (PlaceEvent->data.place_created_object_event.object_state),
           (Pa+2))) {
          return(False);
        }

        *(P+2) = Ref_Word(Pa);
      }
      break;
    case DOORS_PLACE_FAILED_TO_CREATE_OBJECT_EVENT:
     {
        register heapP Pa = HP;

        HP += 3;

        *(Pa+0) = Word(2, TplTag);

        if (!toFcpTransCreateObjectFailure(
           (PlaceEvent->data.place_failed_to_create_object_event.reason),
					   (Pa+1))) {
          return(False);
        }

        if (!toFcpTransObjectId(
              (PlaceEvent->data.place_failed_to_create_object_event.object_id),
				(Pa+2))) {
          return(False);
        }

        *(P+2) = Ref_Word(Pa);
      }
      break;
    case DOORS_PLACE_DELETED_OBJECT_EVENT:
    if (!toFcpTransObjectId((PlaceEvent->data.object_id), (P+2))) {
        return(False);
      }
      break;
    case DOORS_PLACE_CONNECTED_PRESENCE_EVENT:
     {
        register heapP Pa = HP;

        HP += 3;

        *(Pa+0) = Word(2, TplTag);

        if (!toFcpTransPresenceId((PlaceEvent->
	        data.place_connected_presence_event.connected_presence_id),
           (Pa+1))) {
          return(False);
        }

        if (!toFcpTransPosition((PlaceEvent->
   	       data.place_connected_presence_event.new_position),
				(Pa+2))) {
          return(False);
        }

        *(P+2) = Ref_Word(Pa);
      }
      break;
    case DOORS_PLACE_CONNECTED_OBJECT_EVENT:
     {
        register heapP Pa = HP;

        HP += 4;

        *(Pa+0) = Word(3, TplTag);

        if (!toFcpTransPresenceId((PlaceEvent-> 
		data.place_connected_object_event.connected_presence_id),
              (Pa+1))) {
          return(False);
        }

        if (!toFcpTransObjectId((PlaceEvent->
		data.place_connected_object_event.connected_object_id),
           (Pa+2))) {
          return(False);
        }
        if (!toFcpTransPosition((PlaceEvent->
		data.place_connected_presence_event.new_position),
           (Pa+3))) {
          return(False);
        }

        *(P+2) = Ref_Word(Pa);
      }
      break;
    case DOORS_PLACE_DISCONNECTED_PRESENCE_EVENT:
    if (!toFcpTransPresenceId((PlaceEvent->data.disconnected_presence_id), 
	(P+2))) {
        return(False);
      }
      break;
    case DOORS_PLACE_DISCONNECTED_OBJECT_EVENT:
     {
        register heapP Pa = HP;

        HP += 3;

        *(Pa+0) = Word(2, TplTag);

        if (!toFcpTransPresenceId((PlaceEvent-> 
		data.place_disconnected_object_event.disconnected_presence_id),
              (Pa+1))) {
          return(False);
        }

        if (!toFcpTransObjectId((PlaceEvent->
		data.place_disconnected_object_event.disconnected_object_id),
           (Pa+2))) {
          return(False);
        }
        *(P+2) = Ref_Word(Pa);
      }
      break;

    case DOORS_PLACE_FAILED_TO_CONNECT_TO_PRESENCE_EVENT:
    {
        register heapP Pa = HP;

        HP += 3;

        *(Pa+0) = Word(2, TplTag);

       if (!toFcpTransConnectFailure((PlaceEvent->
		data.place_failed_to_connect_to_presence_event.reason),
           (Pa+1))) {
          return(False);
        }

        if (!toFcpTransPresenceId((PlaceEvent-> 
		data.place_failed_to_connect_to_presence_event.presence_id),
              (Pa+2))) {
          return(False);
        } 
        *(P+2) = Ref_Word(Pa);
	*(P+3) = Word(0, NilTag); /* to look consistent to the fcp server */
      }
      break;
    case DOORS_PLACE_FAILED_TO_CONNECT_TO_OBJECT_EVENT:
    {
        register heapP Pa = HP;

        HP += 4;

        *(Pa+0) = Word(3, TplTag);

       if (!toFcpTransConnectFailure((PlaceEvent->
		data.place_failed_to_connect_to_object_event.reason),
           (Pa+1))) {
          return(False);
        }

        if (!toFcpTransPresenceId((PlaceEvent-> 
		data.place_failed_to_connect_to_object_event.presence_id),
              (Pa+2))) {
          return(False);
        }

        if (!toFcpTransObjectId((PlaceEvent->
		data.place_failed_to_connect_to_object_event.object_id),
           (Pa+3))) {
          return(False);
        }
 
        *(P+2) = Ref_Word(Pa);
      }
      break;
    }

    *FCPPlaceEvent = Ref_Word(P);
  }
  return(True);
}

int toFcpTransTransientEvent(TransientEvent, FCPTransientEvent)
     DoorsTransientEvent *TransientEvent;
     heapP FCPTransientEvent;
{
  if (TransientEvent == ((DoorsTransientEvent *) Null)) {
    *FCPTransientEvent = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 3;

    *(P+0) = Word(2, TplTag);

    if (!toFcpTransTransientEventType((TransientEvent->type), (P+1))) {
      return(False);
    }

    switch (TransientEvent->type) {
    case DOORS_TRANSIENT_CLICKED_EVENT:
      {
	register heapP Pa = HP;

	HP += 3;

	*(Pa+0) = Word(2, TplTag);

	if (!toFcpTransPosition(
	       (TransientEvent->data.transient_clicked_event.position),
	       (Pa+1))) {
	  return(False);
	}

	if (!toFcpTransDoor(
               (TransientEvent->data.transient_clicked_event.door), (Pa+2))) {
	  return(False);
	}

	*(P+2) = Ref_Word(Pa);
      }
      break;
    case DOORS_TRANSIENT_MOVED_EVENT:
      {
	register heapP Pa = HP;

	HP += 3;

	*(Pa+0) = Word(2, TplTag);

	if (!toFcpTransObjectId(
               (TransientEvent->data.transient_moved_event.object_id),
               (Pa+1))) {
	  return(False);
	}

	if (!toFcpTransPosition(
               (TransientEvent->data.transient_moved_event.position),
	       (Pa+2))) {
	  return(False);
	}

	*(P+2) = Ref_Word(Pa);
      }
      break;
    case DOORS_TRANSIENT_FACE_STATE_EVENT:
      if (!toFcpTransFaceState((TransientEvent->data.new_face_state), (P+2))) {
	return(False);
      }
      break;
    case DOORS_TRANSIENT_AUDIO_FOCUS_EVENT:
      if (!toFcpTransBoolean((TransientEvent->data.new_audio_focus), (P+2))) {
	return(False);
      }
      break;
    case DOORS_TRANSIENT_CONVERSATION_TYPE_EVENT:
      {
	register heapP Pa = HP;

	HP += 3;

	*(Pa+0) = Word(2, TplTag);

	if (!toFcpTransObjectId((TransientEvent->
	       data.transient_conversation_type_event.object_id),
               (Pa+1))) {
	  return(False);
	}

	if (!toFcpTransConversationType((TransientEvent->
               data.transient_conversation_type_event.new_conversation_type),
	       (Pa+2))) {
	  return(False);
	}

	*(P+2) = Ref_Word(Pa);
      }
      break;
    }

    *FCPTransientEvent = Ref_Word(P);
  }
  return(True);
}

int toFcpTransAlertEvent(AlertEvent, FCPAlertEvent)
     DoorsAlertEvent *AlertEvent;
     heapP FCPAlertEvent;
{
  if (AlertEvent == ((DoorsAlertEvent *) Null)) {
    *FCPAlertEvent = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 2;

    *(P+0) = Word(1, TplTag);

    if (!toFcpTransAlertEventType((AlertEvent->type), (P+1))) {
      return(False);
    }

    *FCPAlertEvent = Ref_Word(P);
  }
  return(True);
}


int toFcpTransMessageEvent(MessageEvent, FCPMessageEvent)
     DoorsMessageEvent *MessageEvent;
     heapP FCPMessageEvent;
{
  if (MessageEvent == ((DoorsMessageEvent *) Null)) {
    *FCPMessageEvent = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 3;

    *(P+0) = Word(2, TplTag);

    if (!toFcpTransMessageEventType((MessageEvent->type), (P+1))) {
      return(False);
    }

    switch (MessageEvent->type) {
    case DOORS_MESSAGE_TEXT_EVENT:
      if (!toFcpTransString((MessageEvent->data.text), (P+2))) {
	return(False);
      }
      break;
    }

    *FCPMessageEvent = Ref_Word(P);
  }
  return(True);
}

int toFcpTransOtherEvent(OtherEvent, FCPOtherEvent)
     DoorsOtherEvent *OtherEvent;
     heapP FCPOtherEvent;
{
  if (OtherEvent == ((DoorsOtherEvent *) Null)) {
    *FCPOtherEvent = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 3;

    *(P+0) = Word(2, TplTag);

    if (!toFcpTransOtherEventType((OtherEvent->type), (P+1))) {
      return(False);
    }

    switch (OtherEvent->type) {
    case DOORS_OTHER_TEXT_EVENT:
      if (!toFcpTransString((OtherEvent->data.text), (P+2))) {
	return(False);
      }
      break;
    }

    *FCPOtherEvent = Ref_Word(P);
  }
  return(True);
}

int toFcpTransAudioEvent(AudioEvent, FCPAudioEvent)
     DoorsAudioEvent *AudioEvent;
     heapP FCPAudioEvent;
{
  if (AudioEvent == ((DoorsAudioEvent *) Null)) {
    *FCPAudioEvent = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 3;

    *(P+0) = Word(2, TplTag);

    if (!toFcpTransAudioEventType((AudioEvent->type), (P+1))) {
      return(False);
    }

    switch (AudioEvent->type) {
    case DOORS_AUDIO_REMOTE_JOINED_EVENT:
    case DOORS_AUDIO_REMOTE_LEFT_EVENT:
      if (!toFcpTransString((AudioEvent->remote_id), (P+2))) {
	return(False);
      }
      break;
    }

    *FCPAudioEvent = Ref_Word(P);
  }
  return(True);
}

int toFcpTransEvent(Event, FCPEvent)
     DoorsEvent *Event;
     heapP FCPEvent;
{
  if (Event == ((DoorsEvent *) Null)) {
    *FCPEvent = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 3;

    *(P+0) = Word(2, TplTag);

    if (!toFcpTransEventCategory((Event->category), (P+1))) {
      return(False);
    }

    switch (Event->category) {
    case DOORS_PRESENCE_EVENT_CATEGORY:
      if (!toFcpTransPresenceEvent((Event->content.presence_event), (P+2))) {
	return(False);
      }
      break;
    case DOORS_PLACE_EVENT_CATEGORY:
      if (!toFcpTransPlaceEvent((Event->content.place_event), (P+2))) {
	return(False);
      }
      break;
    case DOORS_TRANSIENT_EVENT_CATEGORY:
      if (!toFcpTransTransientEvent((Event->content.transient_event), (P+2))) {
	return(False);
      }
      break;
    case DOORS_ALERT_EVENT_CATEGORY:
      if (!toFcpTransAlertEvent((Event->content.alert_event), (P+2))) {
	return(False);
      }
      break;
    case DOORS_MESSAGE_EVENT_CATEGORY:
      if (!toFcpTransMessageEvent((Event->content.message_event), (P+2))) {
	return(False);
      }
      break;
    case DOORS_OTHER_EVENT_CATEGORY:
      if (!toFcpTransOtherEvent((Event->content.other_event), (P+2))) {
	return(False);
      }
      break;
    case DOORS_AUDIO_EVENT_CATEGORY:
      if (!toFcpTransAudioEvent((Event->content.audio_event), (P+2))) {
	return(False);
      }
      break;
    }

    *FCPEvent = Ref_Word(P);
  }
  return(True);
}

/* doorsEvn.h - end */

/* doorsBc.h - start */

int toFcpTransBusinessCard(BusinessCard, FCPBusinessCard)
     DoorsBusinessCard *BusinessCard;
     heapP FCPBusinessCard;
{
  if (BusinessCard == ((DoorsBusinessCard *) Null)) {
    *FCPBusinessCard = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 15;

    *(P+0) = Word(14, TplTag);

    if (!toFcpTransString((BusinessCard->email_address), (P+1))) {
      return(False);
    }

    if (!toFcpTransString((BusinessCard->private_url), (P+2))) {
      return(False);
    }

    if (!toFcpTransString((BusinessCard->first_name), (P+3))) {
      return(False);
    }

    if (!toFcpTransString((BusinessCard->last_name), (P+4))) {
      return(False);
    }

    if (!toFcpTransString((BusinessCard->organization), (P+5))) {
      return(False);
    }

    if (!toFcpTransString((BusinessCard->title), (P+6))) {
      return(False);
    }

    if (!toFcpTransString((BusinessCard->mail_stop), (P+7))) {
      return(False);
    }

    if (!toFcpTransString((BusinessCard->address), (P+8))) {
      return(False);
    }

    if (!toFcpTransString((BusinessCard->city), (P+9))) {
      return(False);
    }

    if (!toFcpTransString((BusinessCard->state), (P+10))) {
      return(False);
    }

    if (!toFcpTransString((BusinessCard->zip_code), (P+11))) {
      return(False);
    }

    if (!toFcpTransString((BusinessCard->phone), (P+12))) {
      return(False);
    }

    if (!toFcpTransString((BusinessCard->fax), (P+13))) {
      return(False);
    }

    if (!toFcpTransString((BusinessCard->remark), (P+14))) {
      return(False);
    }

    *FCPBusinessCard = Ref_Word(P);
  }
  return(True);
}

/* doorsBc.h - end */

/* doorsReq.h - start */

int toFcpTransRequestCategory(RequestCategory, FCPRequestCategory)
     DoorsRequestCategory RequestCategory;
     heapP FCPRequestCategory;
{
  switch (RequestCategory) {
  case DOORS_USER_REQUEST_CATEGORY:
  case DOORS_PLACE_REQUEST_CATEGORY:
  case DOORS_SERVER_REQUEST_CATEGORY:
    *FCPRequestCategory = Word(RequestCategory, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransRequestCategory;
    return(False);
  }
}

int toFcpTransUserRequestType(UserRequestType, FCPUserRequestType)
     DoorsUserRequestType UserRequestType;
     heapP FCPUserRequestType;
{
  switch (UserRequestType) {
  case DOORS_USER_DATA_REQUEST:
  case DOORS_USER_FACE_REQUEST:
  case DOORS_USER_ALL_REQUEST:
    *FCPUserRequestType = Word(UserRequestType, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransUserRequestType;
    return(False);
  }
}

int toFcpTransPlaceRequestType(PlaceRequestType, FCPPlaceRequestType)
     DoorsPlaceRequestType PlaceRequestType;
     heapP FCPPlaceRequestType;
{
  switch (PlaceRequestType) {
  case DOORS_PLACE_CONNECT_REQUEST:
  case DOORS_PLACE_VIEW_EVENTS_REQUEST:
  case DOORS_PLACE_CACHE_REQUEST:
  case DOORS_PLACE_PRESENCES_REQUEST:
  case DOORS_PLACE_CONNECT_TO_PRESENCE_REQUEST:
  case DOORS_PLACE_CONNECT_TO_OBJECT_REQUEST:
  case DOORS_PLACE_OBJECT_CONNECTIONS_REQUEST:
  case DOORS_PLACE_CREATE_OBJECT_REQUEST:
    *FCPPlaceRequestType = Word(PlaceRequestType, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransPlaceRequestType;
    return(False);
  }
}

int toFcpTransServerRequestType(ServerRequestType, FCPServerRequestType)
     DoorsServerRequestType ServerRequestType;
     heapP FCPServerRequestType;
{
  switch (ServerRequestType) {
  case DOORS_SERVER_PRESENCES_COUNT_REQUEST:
  case DOORS_SERVER_PRESENCES_LIST_REQUEST:
    *FCPServerRequestType = Word(ServerRequestType, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransServerRequestType;
    return(False);
  }
}

int toFcpTransUserObjectList(UserObjectList, FCPUserObjectList)
     DoorsUserObjectList *UserObjectList;
     heapP FCPUserObjectList;
{
  register DoorsUserObjectList *UserObjectListP = UserObjectList;

  register heapP First = Null, Last = Null;

  while (UserObjectListP != ((DoorsUserObjectList *) Null)) {
    register heapP P;
    
    doorsfcp_check_fcp_memory(1);
    
    P = HP;
    if (First == Null) {
      First = HP;
    }
    
    HP += 2;
    
    {
      register heapP Pa = HP;

      HP += 3;

      *(Pa+0) = Word(2, TplTag);

      if (!toFcpTransString((UserObjectListP->net_name), (Pa+1))) {
	return(False);
      }

      if (!toFcpTransString((UserObjectListP->object_name), (Pa+2))) {
	return(False);
      }
    
      *(P+0) = L_Ref_Word(Pa);
    }

    if (Last != Null) {
      *Last = Ref_Word(P);
    }
    Last = (P+1);
    UserObjectListP = UserObjectListP->next;
  }

  if (Last == Null) {
    *FCPUserObjectList = Word(0, NilTag);
  }
  else {
    *Last = Word(0, NilTag);
    *FCPUserObjectList = Ref_Word(First);
  }
  return(True);
}

int toFcpTransEventCategoryList(EventCategoryList, FCPEventCategoryList)
     DoorsEventCategoryList *EventCategoryList;
     heapP FCPEventCategoryList;
{
  register DoorsEventCategoryList *EventCategoryListP = EventCategoryList;

  register heapP First = Null, Last = Null;

  while (EventCategoryListP != ((DoorsEventCategoryList *) Null)) {
    register heapP P;
    
    doorsfcp_check_fcp_memory(1);
    
    P = HP;
    if (First == Null) {
      First = HP;
    }
    
    HP += 2;
    
    if (!toFcpTransEventCategory((EventCategoryListP->category), (P+0))) {
      return(False);
    }
    *(P+0) = Set_List((*(P+0)));

    if (Last != Null) {
      *Last = Ref_Word(P);
    }
    Last = (P+1);
    EventCategoryListP = EventCategoryListP->next;
  }

  if (Last == Null) {
    *FCPEventCategoryList = Word(0, NilTag);
  }
  else {
    *Last = Word(0, NilTag);
    *FCPEventCategoryList = Ref_Word(First);
  }
  return(True);
}

int toFcpTransUserRequest(UserRequest, FCPUserRequest)
     DoorsUserRequest *UserRequest;
     heapP FCPUserRequest;
{
  if (UserRequest == ((DoorsUserRequest *) Null)) {
    *FCPUserRequest = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 3;

    *(P+0) = Word(2, TplTag);

    if (!toFcpTransUserRequestType((UserRequest->type), (P+1))) {
      return(False);
    }

    if (!toFcpTransUserObjectList((UserRequest->query_list), (P+2))) {
      return(False);
    }

    *FCPUserRequest = Ref_Word(P);
  }
  return(True);
}

int toFcpTransPlaceRequest(PlaceRequest, FCPPlaceRequest)
     DoorsPlaceRequest *PlaceRequest;
     heapP FCPPlaceRequest;
{
  if (PlaceRequest == ((DoorsPlaceRequest *) Null)) {
    *FCPPlaceRequest = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 3;

    *(P+0) = Word(2, TplTag);

    if (!toFcpTransPlaceRequestType((PlaceRequest->type), (P+1))) {
      return(False);
    }

    switch (PlaceRequest->type) {
    case DOORS_PLACE_CONNECT_REQUEST:
      {
	register heapP Pa = HP;

	HP += 6;

	*(Pa+0) = Word(5, TplTag);

	if (!toFcpTransPlaceState(
	       (PlaceRequest->data.place_connect_request.place_state),
	       (Pa+1))) {
	  return(False);
	}

	if (!toFcpTransPresenceState(
               (PlaceRequest->data.place_connect_request.presence_state),
               (Pa+2))) {
	  return(False);
	}

	if (!toFcpTransDoor((PlaceRequest->data.place_connect_request.from),
			    (Pa+3))) {
	  return(False);
	}

	if (!toFcpTransDoor((PlaceRequest->data.place_connect_request.to),
			    (Pa+4))) {
	  return(False);
	}

	if (!toFcpTransBusinessCard((PlaceRequest->
              data.place_connect_request.business_card),
	      (Pa+5))) {
	  return(False);
	}

	*(P+2) = Ref_Word(Pa);
      }
      break;
    case DOORS_PLACE_VIEW_EVENTS_REQUEST:
      {
	register heapP Pa = HP;

	HP += 3;

	*(Pa+0) = Word(2, TplTag);

	if (!toFcpTransBoolean(
               (PlaceRequest->data.place_view_events_request.snapshot),
               (Pa+1))) {
	  return(False);
	}

	if (!toFcpTransEventCategoryList(
               (PlaceRequest->data.place_view_events_request.categories),
               (Pa+2))) {
	  return(False);
	}

	*(P+2) = Ref_Word(Pa);
      }
      break;
    case DOORS_PLACE_CACHE_REQUEST:
      {
	register heapP Pa = HP;

	HP += 5;

	*(Pa+0) = Word(4, TplTag);

	if (!toFcpTransEventCategoryList(
               (PlaceRequest->data.place_cache_request.ctgrs), (Pa+1))) {
	  return(False);
	}

	if (!toFcpTransUlong(
	       (PlaceRequest->data.place_cache_request.number), (Pa+2))) {
	  return(False);
	}

	if (!toFcpTransDate((PlaceRequest->data.place_cache_request.from),
			    (Pa+3))) {
	  return(False);
	}

	if (!toFcpTransDate((PlaceRequest->data.place_cache_request.until),
			    (Pa+4))) {
	  return(False);
	}

	*(P+2) = Ref_Word(Pa);
      }
      break;
    case DOORS_PLACE_PRESENCES_REQUEST:
      *(P+2) = Word(0, NilTag);
      break;
    case DOORS_PLACE_CONNECT_TO_PRESENCE_REQUEST:
      {
	register heapP Pa = HP;

	HP += 4;

	*(Pa+0) = Word(3, TplTag);

	if (!toFcpTransPresenceId((
	  PlaceRequest-> 	
		data.place_connect_to_presence_request.connectee_presence_id),
		(Pa+1))) {
	  return(False);
	}
	if (!toFcpTransPosition((
	  PlaceRequest-> 	
		data.place_connect_to_presence_request.connectee_position),
		(Pa+2))) {
	  return(False);
	}

	if (!toFcpTransPosition((
	  PlaceRequest-> 	
		data.place_connect_to_presence_request.new_position),
		(Pa+3))) {
	  return(False);
	}

	*(P+2) = Ref_Word(Pa);
      }
      break;
    case DOORS_PLACE_CONNECT_TO_OBJECT_REQUEST:

      {
	register heapP Pa = HP;

	HP += 5;

	*(Pa+0) = Word(4, TplTag);

	if (!toFcpTransPresenceId((
	  PlaceRequest-> 	
		data.place_connect_to_object_request.connectee_presence_id),
		(Pa+1))) {
	  return(False);
	}

	if (!toFcpTransObjectId((
	  PlaceRequest-> 	
		data.place_connect_to_object_request.connectee_object_id),
		(Pa+2))) {
	  return(False);
	}

	if (!toFcpTransPosition((
	  PlaceRequest-> 	
		data.place_connect_to_object_request.connectee_position),
		(Pa+3))) {
	  return(False);
	}

	if (!toFcpTransPosition((
	  PlaceRequest-> 	
		data.place_connect_to_object_request.new_position),
		(Pa+4))) {
	  return(False);
	}

	*(P+2) = Ref_Word(Pa);
      }
      break;
    case DOORS_PLACE_OBJECT_CONNECTIONS_REQUEST:
      {
	register heapP Pa = HP;

	HP += 3;

	*(Pa+0) = Word(2, TplTag);

	if (!toFcpTransPresenceId((
	  PlaceRequest-> 	
		data.place_object_connections_request.presence_id),
		(Pa+1))) {
	  return(False);
	}

	if (!toFcpTransObjectId((
	  PlaceRequest-> 	
		data.place_object_connections_request.object_id),
		(Pa+2))) {
	  return(False);
	}

	*(P+2) = Ref_Word(Pa);
      }
      break;
    case DOORS_PLACE_CREATE_OBJECT_REQUEST:
      {
	register heapP Pa = HP;

	HP += 3;

	*(Pa+0) = Word(2, TplTag);

	if (!toFcpTransObjectId((
	  PlaceRequest-> 	
		data.place_create_object_request.object_id),
		(Pa+1))) {
	  return(False);
	}

	if (!toFcpTransObjectState((
	  PlaceRequest-> 	
		data.place_create_object_request.object_state),
		(Pa+2))) {
	  return(False);
	}

	*(P+2) = Ref_Word(Pa);
      }
      break;
    }

    *FCPPlaceRequest = Ref_Word(P);
  }
  return(True);
}

int toFcpTransServerRequest(ServerRequest, FCPServerRequest)
     DoorsServerRequest *ServerRequest;
     heapP FCPServerRequest;
{
  if (ServerRequest == ((DoorsServerRequest *) Null)) {
    *FCPServerRequest = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 2;

    *(P+0) = Word(1, TplTag);

    if (!toFcpTransServerRequestType((ServerRequest->type), (P+1))) {
      return(False);
    }

    *FCPServerRequest = Ref_Word(P);
  }
  return(True);
}

int toFcpTransRequest(Request, FCPRequest)
     DoorsRequest *Request;
     heapP FCPRequest;
{
  if (Request == ((DoorsRequest *) Null)) {
    *FCPRequest = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 4;

    *(P+0) = Word(3, TplTag);

    if (!toFcpTransBytes((Request->client_data), (P+1))) {
      return(False);
    }

    if (!toFcpTransRequestCategory((Request->category), (P+2))) {
      return(False);
    }

    switch (Request->category) {
    case DOORS_USER_REQUEST_CATEGORY:
      if (!toFcpTransUserRequest((Request->content.user_request), (P+3))) {
	return(False);
      }
      break;
    case DOORS_PLACE_REQUEST_CATEGORY:
      if (!toFcpTransPlaceRequest((Request->content.place_request), (P+3))) {
	return(False);
      }
      break;
    case DOORS_SERVER_REQUEST_CATEGORY:
      if (!toFcpTransServerRequest(
           (Request->content.server_request), (P+3))) {
	return(False);
      }
      break;
    }

    *FCPRequest = Ref_Word(P);
  }
  return(True);
}

/* doorsReq.h - end */

/* doorsRes.h - start */

int toFcpTransResponseCategory(ResponseCategory, FCPResponseCategory)
     DoorsResponseCategory ResponseCategory;
     heapP FCPResponseCategory;
{
  switch (ResponseCategory) {
  case DOORS_USER_RESPONSE_CATEGORY:
  case DOORS_PLACE_RESPONSE_CATEGORY:
  case DOORS_SERVER_RESPONSE_CATEGORY:
    *FCPResponseCategory = Word(ResponseCategory, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransResponseCategory;
    return(False);
  }
}

int toFcpTransUserResponseType(UserResponseType, FCPUserResponseType)
     DoorsUserResponseType UserResponseType;
     heapP FCPUserResponseType;
{
  switch (UserResponseType) {
  case DOORS_USER_DATA_RESPONSE:
  case DOORS_USER_FACE_RESPONSE:
  case DOORS_USER_ALL_RESPONSE:
    *FCPUserResponseType = Word(UserResponseType, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransUserResponseType;
    return(False);
  }
}

int toFcpTransPlaceResponseType(PlaceResponseType, FCPPlaceResponseType)
     DoorsPlaceResponseType PlaceResponseType;
     heapP FCPPlaceResponseType;
{
  switch (PlaceResponseType) {
  case DOORS_PLACE_SNAPSHOT_RESPONSE:
  case DOORS_PLACE_CACHE_RESPONSE:
  case DOORS_PLACE_PRESENCES_RESPONSE:
  case DOORS_PLACE_OBJECT_CONNECTIONS_RESPONSE:
    *FCPPlaceResponseType = Word(PlaceResponseType, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransPlaceResponseType;
    return(False);
  }
}

int toFcpTransServerResponseType(ServerResponseType, FCPServerResponseType)
     DoorsServerResponseType ServerResponseType;
     heapP FCPServerResponseType;
{
  switch (ServerResponseType) {
  case DOORS_SERVER_PRESENCES_COUNT_RESPONSE:
  case DOORS_SERVER_PRESENCES_LIST_RESPONSE:
    *FCPServerResponseType = Word(ServerResponseType, IntTag);
    return(True);
  default:
    DoorsFcpError = DOORSFCP_toFcpTransServerResponseType;
    return(False);
  }
}

int toFcpTransPlacePresenceList(PlacePresenceList, FCPPlacePresenceList)
     DoorsPlacePresenceList *PlacePresenceList;
     heapP FCPPlacePresenceList;
{
  register DoorsPlacePresenceList *PlacePresenceListP = PlacePresenceList;

  register heapP First = Null, Last = Null;

  while (PlacePresenceListP != ((DoorsPlacePresenceList *) Null)) {
    register heapP P;
    
    doorsfcp_check_fcp_memory(1);
    
    P = HP;
    if (First == Null) {
      First = HP;
    }
    
    HP += 2;
    
    {
      register heapP Pa = HP;

      HP += 3;

      *(Pa+0) = Word(2, TplTag);

      if (!toFcpTransDoor((PlacePresenceListP->door), (Pa+1))) {
	return(False);
      }

      if (!toFcpTransPresenceList((PlacePresenceListP->presences), (Pa+2))) {
	return(False);
      }
    
      *(P+0) = L_Ref_Word(Pa);
    }

    if (Last != Null) {
      *Last = Ref_Word(P);
    }
    Last = (P+1);
    PlacePresenceListP = PlacePresenceListP->next;
  }

  if (Last == Null) {
    *FCPPlacePresenceList = Word(0, NilTag);
  }
  else {
    *Last = Word(0, NilTag);
    *FCPPlacePresenceList = Ref_Word(First);
  }
  return(True);
}

int toFcpTransUserDataList(UserDataList, FCPUserDataList)
     DoorsUserDataList *UserDataList;
     heapP FCPUserDataList;
{
  register DoorsUserDataList *UserDataListP = UserDataList;

  register heapP First = Null, Last = Null;

  while (UserDataListP != ((DoorsUserDataList *) Null)) {
    register heapP P;
    
    doorsfcp_check_fcp_memory(1);
    
    P = HP;
    if (First == Null) {
      First = HP;
    }
    
    HP += 2;
    
    {
      register heapP Pa = HP;

      HP += 3;

      *(Pa+0) = Word(2, TplTag);

      if (!toFcpTransString((UserDataListP->net_name), (Pa+1))) {
	return(False);
      }

      if (!toFcpTransBusinessCard((UserDataListP->business_card), (Pa+2))) {
	return(False);
      }
    
      *(P+0) = L_Ref_Word(Pa);
    }

    if (Last != Null) {
      *Last = Ref_Word(P);
    }
    Last = (P+1);
    UserDataListP = UserDataListP->next;
  }

  if (Last == Null) {
    *FCPUserDataList = Word(0, NilTag);
  }
  else {
    *Last = Word(0, NilTag);
    *FCPUserDataList = Ref_Word(First);
  }
  return(True);
}

int toFcpTransUserFaceList(UserFaceList, FCPUserFaceList)
     DoorsUserFaceList *UserFaceList;
     heapP FCPUserFaceList;
{
  register DoorsUserFaceList *UserFaceListP = UserFaceList;

  register heapP First = Null, Last = Null;

  while (UserFaceListP != ((DoorsUserFaceList *) Null)) {
    register heapP P;
    
    doorsfcp_check_fcp_memory(1);
    
    P = HP;
    if (First == Null) {
      First = HP;
    }
    
    HP += 2;
    
    {
      register heapP Pa = HP;

      HP += 5;

      *(Pa+0) = Word(4, TplTag);

      if (!toFcpTransDate((UserFaceListP->date), (Pa+1))) {
	return(False);
      }

      if (!toFcpTransString((UserFaceListP->net_name), (Pa+2))) {
	return(False);
      }

      if (!toFcpTransString((UserFaceListP->object_name), (Pa+3))) {
	return(False);
      }

      if (!toFcpTransFace((UserFaceListP->face), (Pa+4))) {
	return(False);
      }
    
      *(P+0) = L_Ref_Word(Pa);
    }

    if (Last != Null) {
      *Last = Ref_Word(P);
    }
    Last = (P+1);
    UserFaceListP = UserFaceListP->next;
  }

  if (Last == Null) {
    *FCPUserFaceList = Word(0, NilTag);
  }
  else {
    *Last = Word(0, NilTag);
    *FCPUserFaceList = Ref_Word(First);
  }
  return(True);
}

int toFcpTransUserAllList(UserAllList, FCPUserAllList)
     DoorsUserAllList *UserAllList;
     heapP FCPUserAllList;
{
  register DoorsUserAllList *UserAllListP = UserAllList;

  register heapP First = Null, Last = Null;

  while (UserAllListP != ((DoorsUserAllList *) Null)) {
    register heapP P;
    
    doorsfcp_check_fcp_memory(1);
    
    P = HP;
    if (First == Null) {
      First = HP;
    }
    
    HP += 2;
    
    {
      register heapP Pa = HP;

      HP += 5;

      *(Pa+0) = Word(4, TplTag);

      if (!toFcpTransDate((UserAllListP->date), (Pa+1))) {
	return(False);
      }

      if (!toFcpTransString((UserAllListP->net_name), (Pa+2))) {
	return(False);
      }

      if (!toFcpTransBusinessCard((UserAllListP->business_card), (Pa+3))) {
	return(False);
      }

      if (!toFcpTransFace((UserAllListP->face), (Pa+4))) {
	return(False);
      }
    
      *(P+0) = L_Ref_Word(Pa);
    }

    if (Last != Null) {
      *Last = Ref_Word(P);
    }
    Last = (P+1);
    UserAllListP = UserAllListP->next;
  }

  if (Last == Null) {
    *FCPUserAllList = Word(0, NilTag);
  }
  else {
    *Last = Word(0, NilTag);
    *FCPUserAllList = Ref_Word(First);
  }
  return(True);
}

int toFcpTransPresenceStateList(PresenceStateList, FCPPresenceStateList)
     DoorsPresenceStateList *PresenceStateList;
     heapP FCPPresenceStateList;
{
  register DoorsPresenceStateList *PresenceStateListP = PresenceStateList;

  register heapP First = Null, Last = Null;

  while (PresenceStateListP != ((DoorsPresenceStateList *) Null)) {
    register heapP P;
    
    doorsfcp_check_fcp_memory(1);
    
    P = HP;
    if (First == Null) {
      First = HP;
    }
    
    HP += 2;
    
    {
      register heapP Pa = HP;

      HP += 3;

      *(Pa+0) = Word(2, TplTag);

      if (!toFcpTransPresenceId((PresenceStateListP->id), (Pa+1))) {
	return(False);
      }

      if (!toFcpTransPresenceState((PresenceStateListP->state), (Pa+2))) {
	return(False);
      }
    
      *(P+0) = L_Ref_Word(Pa);
    }

    if (Last != Null) {
      *Last = Ref_Word(P);
    }
    Last = (P+1);
    PresenceStateListP = PresenceStateListP->next;
  }

  if (Last == Null) {
    *FCPPresenceStateList = Word(0, NilTag);
  }
  else {
    *Last = Word(0, NilTag);
    *FCPPresenceStateList = Ref_Word(First);
  }
  return(True);
}

int toFcpTransPlaceSnapShot(PlaceSnapShot, FCPPlaceSnapShot)
     DoorsPlaceSnapShot *PlaceSnapShot;
     heapP FCPPlaceSnapShot;
{
  if (PlaceSnapShot == ((DoorsPlaceSnapShot *) Null)) {
    *FCPPlaceSnapShot = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 3;

    *(P+0) = Word(2, TplTag);

    if (!toFcpTransPlaceState((PlaceSnapShot->place_state), (P+1))) {
      return(False);
    }

    if (!toFcpTransPresenceStateList(
	   (PlaceSnapShot->presence_state_list), (P+2))) {
      return(False);
    }

    *FCPPlaceSnapShot = Ref_Word(P);
  }
  return(True);
}

int toFcpTransEventList(EventList, FCPEventList)
     DoorsEventList *EventList;
     heapP FCPEventList;
{
  register DoorsEventList *EventListP = EventList;

  register heapP First = Null, Last = Null;

  while (EventListP != ((DoorsEventList *) Null)) {
    register heapP P;
    
    doorsfcp_check_fcp_memory(1);
    
    P = HP;
    if (First == Null) {
      First = HP;
    }
    
    HP += 2;
    
    {
      register heapP Pa = HP;

      HP += 3;

      *(Pa+0) = Word(2, TplTag);

      if (!toFcpTransEvent((EventListP->event), (Pa+1))) {
	return(False);
      }

      if (!toFcpTransHeader((EventListP->header), (Pa+2))) {
	return(False);
      }
    
      *(P+0) = L_Ref_Word(Pa);
    }

    if (Last != Null) {
      *Last = Ref_Word(P);
    }
    Last = (P+1);
    EventListP = EventListP->next;
  }

  if (Last == Null) {
    *FCPEventList = Word(0, NilTag);
  }
  else {
    *Last = Word(0, NilTag);
    *FCPEventList = Ref_Word(First);
  }
  return(True);
}

int toFcpTransUserResponse(UserResponse, FCPUserResponse)
     DoorsUserResponse *UserResponse;
     heapP FCPUserResponse;
{
  if (UserResponse == ((DoorsUserResponse *) Null)) {
    *FCPUserResponse = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 3;

    *(P+0) = Word(2, TplTag);

    if (!toFcpTransUserResponseType((UserResponse->type), (P+1))) {
      return(False);
    }

    switch (UserResponse->type) {
    case DOORS_USER_DATA_RESPONSE:
      if (!toFcpTransUserDataList((UserResponse->data.user_data_list),
				  (P+2))) {
	return(False);
      }
      break;
    case DOORS_USER_FACE_RESPONSE:
      if (!toFcpTransUserFaceList((UserResponse->data.user_face_list),
				  (P+2))) {
	return(False);
      }
      break;
    case DOORS_USER_ALL_RESPONSE:
      if (!toFcpTransUserAllList((UserResponse->data.user_all_list), (P+2))) {
	return(False);
      }
      break;
    }

    *FCPUserResponse = Ref_Word(P);
  }
  return(True);
}

int toFcpTransPlaceResponse(PlaceResponse, FCPPlaceResponse)
     DoorsPlaceResponse *PlaceResponse;
     heapP FCPPlaceResponse;
{
  if (PlaceResponse == ((DoorsPlaceResponse *) Null)) {
    *FCPPlaceResponse = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 3;

    *(P+0) = Word(2, TplTag);

    if (!toFcpTransPlaceResponseType((PlaceResponse->type), (P+1))) {
      return(False);
    }

    switch (PlaceResponse->type) {
    case DOORS_PLACE_SNAPSHOT_RESPONSE:
      if (!toFcpTransPlaceSnapShot((PlaceResponse->data.place_snapshot),
				   (P+2))) {
	return(False);
      }
      break;
    case DOORS_PLACE_CACHE_RESPONSE:
      if (!toFcpTransEventList((PlaceResponse->data.event_list), (P+2))) {
	return(False);
      }
      break;
    case DOORS_PLACE_PRESENCES_RESPONSE:
      if (!toFcpTransPresenceList((PlaceResponse->data.presences), (P+2))) {
	return(False);
      }
      break;
    case DOORS_PLACE_OBJECT_CONNECTIONS_RESPONSE:
      {
	register heapP Pa = HP;

	HP += 4;

	*(Pa+0) = Word(3, TplTag);

	if (!toFcpTransPresenceId(
           (PlaceResponse->data.place_object_connections_response.presence_id),
	   (Pa+1))) {
	  return(False);
	}

	if (!toFcpTransObjectId(
           (PlaceResponse->data.place_object_connections_response.object_id),
           (Pa+2))) {
	  return(False);
	}

	if (!toFcpTransPresenceList(
           (PlaceResponse->data.place_object_connections_response.presences),
	   (Pa+3))) {
	  return(False);
	}

	*(P+2) = Ref_Word(Pa);
      }
      break;
    }

    *FCPPlaceResponse = Ref_Word(P);
  }
  return(True);
}

int toFcpTransServerResponse(ServerResponse, FCPServerResponse)
     DoorsServerResponse *ServerResponse;
     heapP FCPServerResponse;
{
  if (ServerResponse == ((DoorsServerResponse *) Null)) {
    *FCPServerResponse = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 3;

    *(P+0) = Word(2, TplTag);

    if (!toFcpTransServerResponseType((ServerResponse->type), (P+1))) {
      return(False);
    }

    switch(ServerResponse->type) {
    case DOORS_SERVER_PRESENCES_COUNT_RESPONSE:
      if (!toFcpTransUlong((ServerResponse->data.presences_number), (P+2))) {
	return(False);
      }
      break;
    case DOORS_SERVER_PRESENCES_LIST_RESPONSE:
      if (!toFcpTransPresenceList((ServerResponse->data.place_presence_list),
				  (P+2))) {
	return(False);
      }
      break;
    }

    *FCPServerResponse = Ref_Word(P);
  }
  return(True);
}

int toFcpTransResponse(Response, FCPResponse)
     DoorsResponse *Response;
     heapP FCPResponse;
{
  if (Response == ((DoorsResponse *) Null)) {
    *FCPResponse = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += 4;

    *(P+0) = Word(3, TplTag);

    if (!toFcpTransBytes((Response->client_data), (P+1))) {
      return(False);
    }

    if (!toFcpTransResponseCategory((Response->category), (P+2))) {
      return(False);
    }

    switch(Response->category) {
    case DOORS_USER_RESPONSE_CATEGORY:
      if (!toFcpTransUserResponse((Response->content.user_response), (P+3))) {
	return(False);
      }
      break;
    case DOORS_PLACE_RESPONSE_CATEGORY:
      if (!toFcpTransPlaceResponse((Response->content.place_response),
				   (P+3))) {
	return(False);
      }
      break;
    case DOORS_SERVER_RESPONSE_CATEGORY:
      if (!toFcpTransServerResponse((Response->content.server_response),
				    (P+3))) {
	return(False);
      }
      break;
    }

    *FCPResponse = Ref_Word(P);
  }
  return(True);
}

/* doorsRes.h - end */

#ifdef A13434234

/* Translate list template */

int toFcpTrans@(@, FCP@)
     Doors@ *@;
     heapP FCP@;
{
  register Doors@ *@P = @;

  register heapP First = Null, Last = Null;

  while (@P != ((Doors@ *) Null)) {
    register heapP P;
    
    doorsfcp_check_fcp_memory(1);
    
    P = HP;
    if (First == Null) {
      First = HP;
    }
    
    HP += 2;
    
    {
      register heapP Pa = HP;

      HP += #;

      *(Pa+0) = Word(#, TplTag);

      trans elements;
    
      *(P+0) = L_Ref_Word(Pa);
    }

    if (Last != Null) {
      *Last = Ref_Word(P);
    }
    Last = (P+1);
    @P = @P->next;
  }

  if (Last == Null) {
    *FCP@ = Word(0, NilTag);
  }
  else {
    *Last = Word(0, NilTag);
    *FCP@ = Ref_Word(First);
  }
  return(True);
}

/* Translate structure template */

int toFcpTrans@(@, FCP@)
     Doors@ *@;
     heapP FCP@;
{
  if (@ == ((Doors@ *) Null)) {
    *FCP@ = Word(0, NilTag);
    return(True);
  }

  doorsfcp_check_fcp_memory(1);

  {
    register heapP P = HP;

    HP += #;

    *(P+0) = Word(#, TplTag);

    trans elements;

    *FCP@ = Ref_Word(P);
  }
  return(True);
}

#endif
