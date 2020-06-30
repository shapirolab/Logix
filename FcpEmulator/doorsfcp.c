/*
** This module is part of EFCP.
**

     Copyright 2007 Avshalom Houri, Alon Kleinman
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
   Communicate:
     alon:
       - DoorsSetNotificationFile function cannot be called seperately.
       - How to set node and port ids of the server.
*/

#include <stdio.h>
#include <sgtty.h>
#include <errno.h>
#include <string.h>
#include <sys/file.h>
#include <sys/ioctl.h>

#include "doors.h"

#include "fcp.h"
#include "codes.h"
#include "global.h"
#include "macros.h"

#include "doorsfcp.h"

int *AllocP, *EndAllocP;

int HeapSpaceError;

int DoorsFcpError;

static heapP WakeupStreamList;
static heapP OutputStream, OutputStreamPtr;

static int CurrentFd;
static int RemovedFd;

static void add_doors_wakeup_file();
static void remove_doors_wakeup_file();

static void error_routine();
static void event_routine();
static void request_routine();
static void response_routine();

static int handle_doors();

static int doors_have_revolved();

int doorsfcp(InputTuple)
     heapP InputTuple;

{
  register heapP P = InputTuple;
  register heapP T, E;

  deref_ptr(P);
  if (!(IsTpl(*P) && (Arity_of(*P) == 2))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    return(False);
  }
  
  P = InputTuple+1;
  deref_ptr(P);
  if (!(IsTpl(*P))) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    return(False);
  }
  T = P;

  P = InputTuple+2;
  deref_ptr(P);
  if (!IsWrt(*P)) {
    if (IsRo(*P)) {
      sus_tbl_add(P);
    }
    return(False);
  }
  E = P;

  P = T+1;
  deref_ptr(P);
  if (!IsStr(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    return(False);
  }

  doorsfcp_start();

  switch (Arity_of(*T)) {
  case 1:
    if (strcmp((char *) (P+StrHdrWords), "doorsAbort") == 0) {
      doorsfcp_return(Call_DoorsAbort(T, E));
    }
    if (strcmp((char *) (P+StrHdrWords), "doorsExit") == 0) {
      doorsfcp_return(Call_DoorsExit(T, E));
    }
    doorsfcp_return(False);
  case 4:
    if (strcmp((char *) (P+StrHdrWords), "doorsInitialize") == 0) {
      doorsfcp_return(Call_DoorsInitialize(T, E));
    }
    if (strcmp((char *) (P+StrHdrWords), "doorsPropagateError") == 0) {
      doorsfcp_return(Call_DoorsPropagateError(T, E));
    }
    if (strcmp((char *) (P+StrHdrWords), "doorsPropagateEvent") == 0) {
      doorsfcp_return(Call_DoorsPropagateEvent(T, E));
    }
    if (strcmp((char *) (P+StrHdrWords), "doorsPropagateRequest") == 0) {
      return(Call_DoorsPropagateRequest(T, E));
    }
    if (strcmp((char *) (P+StrHdrWords), "doorsPropagateResponse") == 0) {
      doorsfcp_return(Call_DoorsPropagateResponse(T, E));
    }
    doorsfcp_return(False);
  default:
    doorsfcp_return(False);
  }
}

/* {"doorsInitialize", WakeupStreams, TokenFile, WarningVar}, ErrorValVar */
int Call_DoorsInitialize(T, E)
     heapP T, E;
{
  register heapP P;

  register heapP WakeupStreams, TokenFile, WarningVar;

  if (ended_heap((HP+(3*MaxFileDescriptors)))) {
    err_tbl_add(MACHINE, ErHPSPACE);
    HeapSpaceError = True;
    return(DOORS_FALSE);
  }

  P = T+2;
  deref_ptr(P);
  if (!IsWrt(*P)) {
    if (IsRo(*P)) {
      sus_tbl_add(P);
    }
    return(False);
  }
  WakeupStreams = P;

  P = T+3;
  deref_ptr(P);
  if (!IsStr(*P)) {
    if (IsVar(*P)) {
      sus_tbl_add(P);
    }
    return(False);
  }
  TokenFile = P;

  P = T+4;
  deref_ptr(P);
  if (!IsWrt(*P)) {
    if (IsRo(*P)) {
      sus_tbl_add(P);
    }
    return(False);
  }
  WarningVar = P;

  {
    register DoorsErrorVal ErrorVal;
    
    ErrorVal = doorsSetErrorHandler(((DoorsPresenceId) Null), error_routine);
    if (ErrorVal != DOORS_OK_ERROR_VAL) {
      asgn(*E, E, Word(ErrorVal, IntTag));
      return(True);
    }

    ErrorVal = doorsSetEventHandler(((DoorsPresenceId) Null), event_routine);
    if (ErrorVal != DOORS_OK_ERROR_VAL) {
      asgn(*E, E, Word(ErrorVal, IntTag));
      return(True);
    }

    ErrorVal =
      doorsSetRequestHandler(((DoorsPresenceId) Null), request_routine);
    if (ErrorVal != DOORS_OK_ERROR_VAL) {
      asgn(*E, E, Word(ErrorVal, IntTag));
      return(True);
    }

    ErrorVal =
      doorsSetResponseHandler(((DoorsPresenceId) Null), response_routine);
    if (ErrorVal != DOORS_OK_ERROR_VAL) {
      asgn(*E, E, Word(ErrorVal, IntTag));
      return(True);
    }

    WakeupStreamList = Nil;

    {
      DoorsWarning Warning;
      heapT WarningT;

      ErrorVal =
	doorsInitializeServer(
			      add_doors_wakeup_file,
			      remove_doors_wakeup_file,
			      ((DoorsString) (TokenFile + StrHdrWords)),
			      &Warning);
    
      toFcpTransWarning(Warning, &WarningT);
      asgn(*WarningVar, WarningVar, WarningT);

      if (DoorsFcpError != 0) {
	asgn(*E, E, Word(DoorsFcpError, IntTag));
	return(True);
      }
      if (ErrorVal != DOORS_OK_ERROR_VAL) {
	asgn(*E, E, Word(ErrorVal, IntTag));
	return(True);
      }

    }
    
    asgn(*WakeupStreams, WakeupStreams, Ref_Word(WakeupStreamList));
    asgn(*E, E, Word(ErrorVal, IntTag));
  }
  return(True);
}

/* {"doorsAbort"}, ErrorValVar */
int Call_DoorsAbort(T, E)
     heapP T, E;
{
  register heapP P;

  {
    register DoorsErrorVal ErrorVal;
    
    ErrorVal = doorsAbort();

    asgn(*E, E, Word(ErrorVal, IntTag));
  }
  
  return(True);
}

/* {"doorsExit"}, E */
int Call_DoorsExit(T, E)
     heapP T, E;
{
  register heapP P;

  {
    register DoorsErrorVal ErrorVal;
    
    ErrorVal = doorsExit();

    asgn(*E, E, Word(ErrorVal, IntTag));
  }
  
  return(True);
}

/* {doorsPropagateError, Recipients, Header, Error}, ErrorValVar */
int Call_DoorsPropagateError(T, E)
     heapP T, E;
{
  register heapP P;

  {
    DoorsPresenceList *List = ((DoorsPresenceList *) Null);
    DoorsHeader *Header = ((DoorsHeader *) Null);
    DoorsError *Error = ((DoorsError *) Null);

    call_fromFcpTrans_routine(fromFcpTransPresenceList((T+2), &(List)), E);
    call_fromFcpTrans_routine(fromFcpTransHeader((T+3), &(Header)), E);
    call_fromFcpTrans_routine(fromFcpTransError((T+4), &(Error)), E);

    {
      register DoorsErrorVal ErrorVal;
    
      ErrorVal = doorsPropagateError(List, Header, Error);
      asgn(*E, E, Word(ErrorVal, IntTag));
      return(True);
    }
  }
}

/* {doorsPropagateEvent, Recipients, Header, Event}, ErrorValVar */
int Call_DoorsPropagateEvent(T, E)
     heapP T, E;
{
  register heapP P;

  {
    DoorsPresenceList *List = ((DoorsPresenceList *) Null);
    DoorsHeader *Header = ((DoorsHeader *) Null);
    DoorsEvent *Event = ((DoorsEvent *) Null);

    call_fromFcpTrans_routine(fromFcpTransPresenceList((T+2), &(List)), E);
    call_fromFcpTrans_routine(fromFcpTransHeader((T+3), &(Header)), E);
    call_fromFcpTrans_routine(fromFcpTransEvent((T+4), &(Event)), E);

    {
      register DoorsErrorVal ErrorVal;
    
      ErrorVal = doorsPropagateEvent(List, Header, Event);
      asgn(*E, E, Word(ErrorVal, IntTag));
      return(True);
    }
  }
}

/* {doorsPropagateRequest, Recipients, Header, Request}, ErrorValVar */
int Call_DoorsPropagateRequest(T, E)
     heapP T, E;
{
  register heapP P;

  {
    DoorsPresenceList *List = ((DoorsPresenceList *) Null);
    DoorsHeader *Header = ((DoorsHeader *) Null);
    DoorsRequest *Request = ((DoorsRequest *) Null);

    call_fromFcpTrans_routine(fromFcpTransPresenceList((T+2), &(List)), E);
    call_fromFcpTrans_routine(fromFcpTransHeader((T+3), &(Header)), E);
    call_fromFcpTrans_routine(fromFcpTransRequest((T+4), &(Request)), E);

    {
      register DoorsErrorVal ErrorVal;
    
      ErrorVal = doorsPropagateRequest(List, Header, Request);
      asgn(*E, E, Word(ErrorVal, IntTag));
      return(True);
    }
  }
}

/* {doorsPropagateResponse, Recipients, Header, Response}, ErrorValVar */
int Call_DoorsPropagateResponse(T, E)
     heapP T, E;
{
  register heapP P;

  {
    DoorsPresenceList *List = ((DoorsPresenceList *) Null);
    DoorsHeader *Header = ((DoorsHeader *) Null);
    DoorsResponse *Response = ((DoorsResponse *) Null);

    call_fromFcpTrans_routine(fromFcpTransPresenceList((T+2), &(List)), E);
    call_fromFcpTrans_routine(fromFcpTransHeader((T+3), &(Header)), E);
    call_fromFcpTrans_routine(fromFcpTransResponse((T+4), &(Response)), E);

    {
      register DoorsErrorVal ErrorVal;
    
      ErrorVal = doorsPropagateResponse(List, Header, Response);
      asgn(*E, E, Word(ErrorVal, IntTag));
      return(True);
    }
  }
}


static int handle_doors(Event)
     int Event;
{
  switch (Event) {
  case ResumeC:
    break;
  case PauseC:
    break;
  case ExitC:
    (void) doorsAbort();
    break;
  }
}

static void add_doors_wakeup_file(Fd)
     int Fd;
{
  register heapP WakeupStream;

  WakeupStream = HP;
  *HP++ = ZeroedWrt;

  if (!(set_select_entry(ReadC, Fd, WakeupStream, doors_have_revolved,
			 handle_doors))) {
    DoorsFcpError = DOORSFCP_ADD_DOORS_WAKEUP_FILE;
    return;
  }

  deref_ptr(WakeupStream);
  *HP = L_Ref_Word(WakeupStream);
  *(HP+1) = Ref_Word(WakeupStreamList);
  WakeupStreamList = HP;
  HP += 2;

  return;
}

static void remove_doors_wakeup_file(Fd)
     int Fd;
{
  RemovedFd = True;
}

static int doors_have_revolved(Fd)
     int Fd;
{
  register int GC_Done = False;

  if (ended_heap(HP) && (GC_Done == False)) {
    do_gc();
    GC_Done = True;
  }
  if (ended_heap(HP)) {
    do_exit("doors_have_revolved:1", MACHINE, ErHPOVFL, True);
  }

  CurrentFd = Fd;
  RemovedFd = False;

  {
    register DoorsErrorVal ErrorVal;
    
    ErrorVal = doorsProcessInput(Fd);

    if (ended_heap(HP) && (GC_Done == False)) {
      do_gc();
      GC_Done = True;
    }
    if (ended_heap(HP)) {
      do_exit("doors_have_revolved:2", MACHINE, ErHPOVFL, True);
    }

    check_error_variable(ErrorVal, DOORS_OK_ERROR_VAL, "doors_api_error",
			 CurrentFd);

    if (RemovedFd == False) {
      next_select_token(ReadC, Fd);
    }
    else {
      if (!reset_select_entry(ReadC, Fd)) {
	DoorsFcpError = DOORSFCP_REMOVE_DOORS_WAKEUP_FILE;
      }
    }
  }
}

static void error_routine(
  DoorsPresenceId  id,
  DoorsHeader	  *header,
  DoorsError      *error
)
{
  register int GC_Done = False;
  register int Result;

  if (ended_heap(HP) && (GC_Done == False)) {
    do_gc();
    GC_Done = True;
  }
  if (ended_heap(HP)) {
    return;
  }

  Result = translate_error(id, header, error);
  if (!Result) {
    if ((HeapSpaceError == True) && (GC_Done == False)) {
      do_gc();
      GC_Done = True;
      if (ended_heap(HP)) {
	return;
      }
      Result =  translate_error(id, header, error);
    }
  }
  if (!Result) {
    check_error_variable(DoorsFcpError, 0, "doors_translation_error",
			 CurrentFd);
  }
  return;
}

int translate_error(
  DoorsPresenceId  id,
  DoorsHeader	  *header,
  DoorsError      *error
)
{
  heapP Header, Error;

  if (!toFcpTransHeader(header, &(Header))) {
    return(False);
  }

  if (!toFcpTransError(error, &(Error))) {
    return(False);
  }

  {
    register heapP Element = HP;

    HP += 4;
 
    *(Element+0) = Word(3, TplTag);
    *(Element+1) = Ref_Word((Constants[DoorsErrorC]));
    *(Element+2) = Ref_Word(Header);
    *(Element+3) = Ref_Word(Error);

    return(add_to_device_stream(ReadC, CurrentFd, Element));
  }
}

static void event_routine(
  DoorsPresenceId  id,
  DoorsHeader	  *header,
  DoorsEvent      *event
)
{
  register int GC_Done = False;
  register int Result;

  if (ended_heap(HP) && (GC_Done == False)) {
    do_gc();
    GC_Done = True;
  }
  if (ended_heap(HP)) {
    return;
  }

  Result = translate_event(id, header, event);
  if (!Result) {
    if ((HeapSpaceError == True) && (GC_Done == False)) {
      do_gc();
      GC_Done = True;
      if (ended_heap(HP)) {
	return;
      }
      Result =  translate_event(id, header, event);
    }
  }
  if (!Result) {
    check_error_variable(DoorsFcpError, 0, "doors_translation_event",
			 CurrentFd);
  }
  return;
}

int translate_event(
  DoorsPresenceId  id,
  DoorsHeader	  *header,
  DoorsEvent      *event
)
{
  heapP Header, Event;

  if (!toFcpTransHeader(header, &(Header))) {
    return(False);
  }

  if (!toFcpTransEvent(event, &(Event))) {
    return(False);
  }

  {
    register heapP Element = HP;

    HP += 4;
 
    *(Element+0) = Word(3, TplTag);
    *(Element+1) = Ref_Word((Constants[DoorsEventC]));
    *(Element+2) = Ref_Word(Header);
    *(Element+3) = Ref_Word(Event);

    return(add_to_device_stream(ReadC, CurrentFd, Element));
  }
}

static void request_routine(
  DoorsPresenceId  id,
  DoorsHeader	  *header,
  DoorsRequest      *request
)
{
  register int GC_Done = False;
  register int Result;

  if (ended_heap(HP) && (GC_Done == False)) {
    do_gc();
    GC_Done = True;
  }
  if (ended_heap(HP)) {
    return;
  }

  Result = translate_request(id, header, request);
  if (!Result) {
    if ((HeapSpaceError == True) && (GC_Done == False)) {
      do_gc();
      GC_Done = True;
      if (ended_heap(HP)) {
	return;
      }
      Result =  translate_request(id, header, request);
    }
  }
  if (!Result) {
    check_error_variable(DoorsFcpError, 0, "doors_translation_request",
			 CurrentFd);
  }
  return;
}

int translate_request(
  DoorsPresenceId  id,
  DoorsHeader	  *header,
  DoorsRequest      *request
)
{
  heapP Header, Request;

  if (!toFcpTransHeader(header, &(Header))) {
    return(False);
  }

  if (!toFcpTransRequest(request, &(Request))) {
    return(False);
  }

  {
    register heapP Element = HP;

    HP += 4;
 
    *(Element+0) = Word(3, TplTag);
    *(Element+1) = Ref_Word((Constants[DoorsRequestC]));
    *(Element+2) = Ref_Word(Header);
    *(Element+3) = Ref_Word(Request);

    return(add_to_device_stream(ReadC, CurrentFd, Element));
  }
}

static void response_routine(
  DoorsPresenceId  id,
  DoorsHeader	  *header,
  DoorsResponse      *response
)
{
  register int GC_Done = False;
  register int Result;

  if (ended_heap(HP) && (GC_Done == False)) {
    do_gc();
    GC_Done = True;
  }
  if (ended_heap(HP)) {
    return;
  }

  Result = translate_response(id, header, response);
  if (!Result) {
    if ((HeapSpaceError == True) && (GC_Done == False)) {
      do_gc();
      GC_Done = True;
      if (ended_heap(HP)) {
	return;
      }
      Result =  translate_response(id, header, response);
    }
  }
  if (!Result) {
    check_error_variable(DoorsFcpError, 0, "doors_translation_response",
			 CurrentFd);
  }
  return;
}

int translate_response(
  DoorsPresenceId  id,
  DoorsHeader	  *header,
  DoorsResponse      *response
)
{
  heapP Header, Response;

  if (!toFcpTransHeader(header, &(Header))) {
    return(False);
  }

  if (!toFcpTransResponse(response, &(Response))) {
    return(False);
  }

  {
    register heapP Element = HP;

    HP += 4;
 
    *(Element+0) = Word(3, TplTag);
    *(Element+1) = Ref_Word((Constants[DoorsResponseC]));
    *(Element+2) = Ref_Word(Header);
    *(Element+3) = Ref_Word(Response);

    return(add_to_device_stream(ReadC, CurrentFd, Element));
  }
}

