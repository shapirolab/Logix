/*
** This module is part of EFCP.
**

     Copyright 2007 Marilyn Safran
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

#ifndef __DOORS_H
#define __DOORS_H

/* ANSI C C++ C ubique.h */
/* version control */

/* Enums */

typedef enum {
  DOORS_FALSE = 0,
  DOORS_TRUE  = 1
} DoorsBoolean;

typedef enum  {
  DOORS_FACE_PRESENT     = 1,
  DOORS_FACE_CONVERSING  = 2,
  DOORS_FACE_ICONIZED    = 3,
  DOORS_FACE_DISABLED    = 4
} DoorsFaceState;

typedef enum {
  DOORS_CONNECTION_NONE    = 0,
  DOORS_CONNECTION_TOURIST = 1,
  DOORS_CONNECTION_MIKE    = 2
} DoorsConnectionType;

typedef enum {
  DOORS_OBJECT_BUS  = 1,
  DOORS_OBJECT_MIKE = 2
} DoorsObjectType;

typedef enum  {
  DOORS_POSITION_NONE     = 0, /* peeking */
  DOORS_POSITION_DOCUMENT = 1,
  DOORS_POSITION_GALLERY  = 2
} DoorsPositionType;

typedef enum  {
  DOORS_ANCHOR_NONE     = 0,
  DOORS_ANCHOR_IMAGE    = 1,
  DOORS_ANCHOR_CHAR     = 2,
  DOORS_ANCHOR_PRESENCE = 3
} DoorsAnchorType;

typedef enum {
  DOORS_ERROR    = 1,
  DOORS_EVENT    = 2,
  DOORS_REQUEST  = 3,
  DOORS_RESPONSE = 4
} DoorsMessageType;

typedef enum {
  DOORS_COMMUNICATION_ERROR_CATEGORY = 1
} DoorsErrorCategory;

typedef enum {
  DOORS_COMMUNICATION_FAILED_ERROR = 1
} DoorsCommunicationErrorType;

typedef enum {
  DOORS_PRESENCE_EVENT_CATEGORY  = 1,
  DOORS_PLACE_EVENT_CATEGORY     = 2,
  DOORS_TRANSIENT_EVENT_CATEGORY = 3,
  DOORS_ALERT_EVENT_CATEGORY     = 4,
  DOORS_MESSAGE_EVENT_CATEGORY   = 5,
  DOORS_OTHER_EVENT_CATEGORY     = 6
} DoorsEventCategory;

typedef enum {
  DOORS_PRESENCE_ENTERED_EVENT = 1,
  DOORS_PRESENCE_LEFT_EVENT    = 2,
  DOORS_PRESENCE_CRASHED_EVENT = 3
} DoorsPresenceEventType;

typedef enum {
  DOORS_PLACE_CRASHED_EVENT       = 1,
  DOORS_PLACE_CLOSED_EVENT        = 2,
  DOORS_PLACE_INVITE_EVENT        = 3,
  DOORS_PLACE_UPDATE_EVENT        = 4,
  DOORS_PLACE_EDIT_STARTED_EVENT  = 5,
  DOORS_PLACE_EDIT_FAILED_EVENT   = 6,
  DOORS_PLACE_EDIT_FINISHED_EVENT = 7,
  DOORS_PLACE_POST_EVENT          = 8,
  DOORS_PLACE_MAIL_EVENT          = 9
} DoorsPlaceEventType;

typedef enum {
  DOORS_TRANSIENT_CLICKED_EVENT             = 1,
  DOORS_TRANSIENT_MOVED_EVENT               = 2,
  DOORS_TRANSIENT_CREATED_OBJECT_EVENT      = 3,
  DOORS_TRANSIENT_DELETED_OBJECT_EVENT      = 4,
  DOORS_TRANSIENT_CONNECTED_OBJECT_EVENT    = 5,
  DOORS_TRANSIENT_DISCONNECTED_OBJECT_EVENT = 6,
  DOORS_TRANSIENT_FACE_STATE_EVENT          = 7
} DoorsTransientEventType;

typedef enum {
  DOORS_ALERT_ALERT_EVENT = 1
} DoorsAlertEventType;

typedef enum {
  DOORS_MESSAGE_TEXT_EVENT = 1
} DoorsMessageEventType;

typedef enum {
  DOORS_OTHER_TEXT_EVENT = 1
} DoorsOtherEventType;

typedef enum {
  DOORS_USER_REQUEST_CATEGORY   = 1,
  DOORS_PLACE_REQUEST_CATEGORY  = 2,
  DOORS_SERVER_REQUEST_CATEGORY = 3
} DoorsRequestCategory;

typedef enum {
  DOORS_USER_DATA_REQUEST = 1,
  DOORS_USER_FACE_REQUEST = 2,
  DOORS_USER_ALL_REQUEST  = 3
} DoorsUserRequestType;

typedef enum {
  DOORS_PLACE_CONNECT_REQUEST     = 1,
  DOORS_PLACE_VIEW_EVENTS_REQUEST = 2,
  DOORS_PLACE_CACHE_REQUEST       = 3,
  DOORS_PLACE_PRESENCES_REQUEST   = 4
} DoorsPlaceRequestType;

typedef enum {
  DOORS_SERVER_PRESENCES_COUNT_REQUEST = 1,
  DOORS_SERVER_PRESENCES_LIST_REQUEST  = 2
} DoorsServerRequestType;

typedef enum {
  DOORS_USER_RESPONSE_CATEGORY   = 1,
  DOORS_PLACE_RESPONSE_CATEGORY  = 2,
  DOORS_SERVER_RESPONSE_CATEGORY = 3,
} DoorsResponseCategory;

typedef enum {
  DOORS_USER_DATA_RESPONSE = 1,
  DOORS_USER_FACE_RESPONSE = 2,
  DOORS_USER_ALL_RESPONSE  = 3
} DoorsUserResponseType;

typedef enum {
  DOORS_PLACE_SNAPSHOT_RESPONSE  = 1,
  DOORS_PLACE_CACHE_RESPONSE     = 2,
  DOORS_PLACE_PRESENCES_RESPONSE = 3
} DoorsPlaceResponseType;

typedef enum {
  DOORS_SERVER_PRESENCES_COUNT_RESPONSE = 1,
  DOORS_SERVER_PRESENCES_LIST_RESPONSE  = 2
} DoorsServerResponseType;

typedef enum {
  DOORS_OK_ERROR_VAL                    = 0,
  DOORS_BAD_ARGUMENT_ERROR_VAL		= 1,
  DOORS_MALLOC_ERROR_VAL                = 2,
  DOORS_COMMUNICATION_INIT_ERROR_VAL    = 3,
  DOORS_COMMUNICATION_CRASHED_ERROR_VAL = 4,
  DOORS_COMMUNICATION_BAD_ARG_ERROR_VAL = 5,
  DOORS_SET_NOTIFY_FILE_ERROR_VAL       = 6
} DoorsErrorVal;

typedef enum {
  DOORS_SERVER = 1,
  DOORS_PLACE  = 2,
  DOORS_CLIENT = 3,
  DOORS_LIST   = 4
} DoorsDestination;

/* Simple Types */

typedef char          *DoorsChars;
typedef char          *DoorsString;
typedef int            DoorsInteger;
typedef long           DoorsLong;
typedef unsigned long  DoorsUlong;

typedef DoorsString    DoorsVersion;

typedef DoorsString    DoorsNodeId;
typedef DoorsLong      DoorsPortId;
typedef DoorsString    DoorsPlaceId;

typedef DoorsString DoorsPresenceId;
typedef DoorsUlong  DoorsObjectId;

/* Struct Types */

typedef struct {
  DoorsUlong length;
  DoorsChars data;
} DoorsBytes;

typedef DoorsBytes     DoorsFace;

typedef struct {
  DoorsLong seconds;
  DoorsLong micro_seconds;
} DoorsDate, *DoorsDateP;

typedef struct {
  DoorsNodeId  server;
  DoorsPortId  port;
  DoorsPlaceId place;
} DoorsDoor, *DoorsDoorP;

typedef struct {
  DoorsString  login_name;
  DoorsString  domain;
  DoorsString  client_node;
  DoorsString  client_id;
  DoorsDate   *client_date;
  DoorsDate   *presence_date;
  DoorsUlong   presence_incarnation;
} DoorsPresenceInfo, *DoorsPresenceInfoP;

typedef struct {
  DoorsInteger number;      /* image number or character number */
  DoorsInteger char_offset; /* offset from beginning of the text */
} DoorsHtmlAnchor;

typedef struct {
  DoorsPresenceId presence_id; /* if position is on a presence */
  DoorsObjectId   object_id;   /* Object id in the presence */
} DoorsPresenceAnchor;

typedef struct {
  DoorsUlong x_offset; /* x offset (pixels in image/bus) */
  DoorsUlong y_offset; /* y offset (pixels in image/bus) */
  DoorsUlong z_offset; /* z offset (pixels in image/bus) */
} DoorsAbsoluteOffset;

typedef struct {
  DoorsUlong x_proportion; /* x proportions in top/bot margins */
  DoorsUlong y_proportion; /* y proportions in top/bot margins */
  DoorsUlong z_proportion; /* z proportions in top/bot margins */
} DoorsProportionalOffset;

typedef struct {
  DoorsPositionType        position_type;   /* document, gallery */
  DoorsDoor               *door;            /* url position is in */
  DoorsAnchorType          anchor_type;     /* image, character, presence */
  union {
    DoorsHtmlAnchor        html_anchor;
    DoorsPresenceAnchor    presence_anchor;
  } anchor_id;
  DoorsAbsoluteOffset      abs_offset;      /* Absolute Offset in anchor */
  DoorsProportionalOffset  prop_offset;     /* Prop. offset in anchor */
} DoorsPresencePosition;

typedef struct {
  DoorsObjectType        type;         /* Type of object */
  DoorsString            name;         /* Name of the object */
  DoorsPresencePosition *position;     /* Position of the object */
  DoorsDate             *details_date; /* object details time */
  DoorsBoolean           has_icon;     /* flag */
} DoorsObjectState;

typedef struct _DoorsObjectInPresence {
  DoorsObjectId                  object_id;    /* object id (in presence) */
  DoorsObjectState              *object_state; /* state of the object */
  struct _DoorsObjectInPresence *next;
} DoorsObjectInPresence;

typedef struct {
  DoorsPresenceId     presence_id; /* Presence to which connected */
  DoorsObjectId       object_id;   /* Object id in the connected presence */
  DoorsConnectionType type;        /* Connection type: none/tourist/mike */
} DoorsPresenceConnection;

typedef struct _DoorsPresenceConnections {
  DoorsPresenceConnection          *presence_connection;
  struct _DoorsPresenceConnections *next;
} DoorsPresenceConnections;

typedef struct {
  DoorsString               nick_name;      /* nick name of user */
  DoorsVersion              version;        /* of presence's client */
  DoorsPresencePosition    *position;       /* NULL == no position */
  DoorsPresencePosition    *click_position; /* NULL == no position */
  DoorsFaceState            state;          /* chat, present, icon */
  DoorsDate                *edit_date;      /* NULL - not editing */
  DoorsDate                *alert_date;     /* NULL - not alerted */
  DoorsDate                *joined_date;    /* date presence joined */
  DoorsDate                *details_date;   /* presence details time */
  DoorsBoolean              has_icon;       /* flag */
  DoorsObjectInPresence    *objects;        /* owned objects */
  DoorsPresenceConnections *connections;    /* connected objects */
} DoorsPresenceState;

typedef struct {
  DoorsString  net_name;     /* key - user@domain a unique id */
  DoorsBoolean has_icon;     /* flag */
  DoorsString  legal_name;   /* The legal name of the user */
  DoorsString  address;      /* turtle mail address */
  DoorsString  e_mail;	     /* electronic mail address */
  DoorsString  affiliation;  /* affiliation */
  DoorsString  home;         /* URL of home page if it exists */
  DoorsString  phone_no;     /* phone number */
  DoorsString  fax_no;       /* fax number */
  DoorsString  free_text;    /* free text */
  DoorsDate   *details_date; /* Each agent record is time stamped */
} DoorsAgentData;

typedef struct {
  DoorsUlong  document_size;      /* Size of document in bytes */
  DoorsUlong  document_hash;      /* Hash of document */
  DoorsDate  *document_load_date; /* Document loading date */
  DoorsDate  *document_date;      /* Date on disk - future */ 
} DoorsPlaceState;

typedef struct _DoorsPresenceList {
  DoorsPresenceId            id;
  struct _DoorsPresenceList *next;
} DoorsPresenceList, *DoorsPresenceListP;

typedef struct _DoorsPlacePresenceList {
  DoorsDoor                       *door;
  DoorsPresenceList               *presences;
  struct _DoorsPlacePresenceList *next;
} DoorsPlacePresenceList;

typedef struct _DoorsUserDataList {
  DoorsDate                 *details_date;
  DoorsString                net_name;
  DoorsAgentData            *agent_data;
  struct _DoorsUserDataList *next;
} DoorsUserDataList;

typedef struct _DoorsUserFaceList {
  DoorsDate                 *details_date;
  DoorsString                net_name;
  DoorsString                object_name;
  DoorsFace                 *face;
  struct _DoorsUserFaceList *next;
} DoorsUserFaceList;

typedef struct _DoorsUserAllList {
  DoorsDate           *details_date;
  DoorsString          net_name;
  DoorsAgentData      *agent_data;
  DoorsFace           *face;
  struct _DoorsUserAllList *next;
} DoorsUserAllList;

typedef struct _DoorsUserObjectList {
  DoorsString                  net_name;
  DoorsString                  object_name;
  struct _DoorsUserObjectList *next;
} DoorsUserObjectList;

typedef struct _DoorsPresenceStateList {
  DoorsPresenceId                 id;
  DoorsPresenceState             *state;
  struct _DoorsPresenceStateList *next;
} DoorsPresenceStateList;

typedef struct {
  DoorsPlaceState        *place_state;
  DoorsPresenceStateList *presence_state_list;
} DoorsPlaceSnapShot;

typedef struct {
  DoorsCommunicationErrorType type;
} DoorsCommunicationErrorContent, *DoorsCommunicationErrorContentP;

typedef struct {
  DoorsErrorCategory category;
  union {
    /* DOORS_COMMUNICATION_ERROR_CATEGORY */
    DoorsCommunicationErrorContent *communication_error_content;
  } content;
} DoorsErrorContent, *DoorsErrorContentP;

typedef struct {
  DoorsPresenceId    id;
  DoorsDestination   destination;
  DoorsPresenceList *list;
  DoorsDoor         *door;
  DoorsDate         *error_date;
  DoorsErrorContent *error_content;
} DoorsError;

typedef struct {
  DoorsPresenceEventType type;
  union {
    /* DOORS_PRESENCE_ENTERED_EVENT */
    struct {
      DoorsPlaceState    *place_state;
      DoorsPresenceState *presence_state;
      DoorsDoor          *from;
    } presence_entered_event;
    /* DOORS_PRESENCE_LEFT_EVENT */
    DoorsDoor *left_to;
  } data;
} DoorsPresenceEventContent;

typedef struct {
  DoorsPlaceEventType type;
  DoorsString         text_data;
  union {
    /* DOORS_PLACE_CRASHED_EVENT */
    /* DOORS_PLACE_CLOSED_EVENT */
    DoorsDoor *door;
  } data;
} DoorsPlaceEventContent;

typedef struct {
  DoorsTransientEventType type;
  union {
    /* DOORS_TRANSIENT_CLICKED_EVENT */
    struct {
      DoorsPresencePosition *position;
      DoorsDoor             *door;
    } transient_clicked_event;
    /* DOORS_TRANSIENT_MOVED_EVENT */
    struct {
      DoorsObjectId          object_id;
      DoorsPresencePosition *position;
    } transient_moved_event;
    /* DOORS_TRANSIENT_CREATED_OBJECT_EVENT */
    struct {
      DoorsObjectId     object_id;
      DoorsObjectState *object_state;
    } transient_created_object_event;
    /* DOORS_TRANSIENT_DELETED_OBJECT_EVENT */
    DoorsObjectId object_id;
    /* DOORS_TRANSIENT_CONNECTED_OBJECT_EVENT */
    /* DOORS_TRANSIENT_DISCONNECTED_OBJECT_EVENT */
    DoorsPresenceConnection *presence_connection;
    /* DOORS_TRANSIENT_FACE_STATE_EVENT */
    DoorsFaceState new_face_state;
  } data;
} DoorsTransientEventContent;

typedef struct {
  DoorsAlertEventType type;
} DoorsAlertEventContent;

typedef struct {
  DoorsMessageEventType type;
  union {
    /* DOORS_MESSAGE_TEXT_EVENT */
    DoorsString text;
  } data;
} DoorsMessageEventContent;

typedef struct {
  DoorsOtherEventType type;
  union {
    /* DOORS_OTHER_TEXT_EVENT */
    DoorsString text;
  } data;
} DoorsOtherEventContent;

typedef struct {
  DoorsEventCategory  category;
  union {
    /* DOORS_PRESENCE_EVENT_CATEGORY */
    DoorsPresenceEventContent  *presence_event_content;
    /* DOORS_PLACE_EVENT_CATEGORY */
    DoorsPlaceEventContent     *place_event_content;
    /* DOORS_TRANSIENT_EVENT_CATEGORY */
    DoorsTransientEventContent *transient_event_content;
    /* DOORS_ALERT_EVENT_CATEGORY */
    DoorsAlertEventContent     *alert_event_content;
    /* DOORS_MESSAGE_EVENT_CATEGORY */
    DoorsMessageEventContent   *message_event_content;
    /* DOORS_OTHER_EVENT_CATEGORY */
    DoorsOtherEventContent     *other_event_content;
  } content;
} DoorsEventContent;

typedef struct {
  DoorsPresenceId     id;
  DoorsDestination    destination;
  DoorsPresenceList  *list;
  DoorsDoor          *door;
  DoorsDate          *event_date;
  DoorsEventContent  *event_content;
} DoorsEvent;

typedef struct _DoorsEventCategoryList {
  DoorsEventCategory              category;
  struct _DoorsEventCategoryList *next;
} DoorsEventCategoryList;

typedef struct _DoorsEventList {
  DoorsEvent             *event;
  struct _DoorsEventList *next;
} DoorsEventList;

typedef struct {
  DoorsUserRequestType  type;
  DoorsUserObjectList  *query_list;
} DoorsUserRequestContent;

typedef struct {
  DoorsPlaceRequestType type;
  union {
    /* DOORS_PLACE_CONNECT_REQUEST */
    struct {
      DoorsDoor *old_door;
      DoorsDoor *new_door;
    } place_connect_request;
    /* DOORS_PLACE_VIEW_EVENTS_REQUEST */
    struct {
      DoorsBoolean            snapshot;
      DoorsEventCategoryList *categories;
    } place_view_events_request;
    /* DOORS_PLACE_CACHE_REQUEST */
    struct {
      DoorsEventCategoryList *ctgrs;
      DoorsUlong              number;
      DoorsDate              *from;
      DoorsDate              *until;
    } place_cache_request;
  } data;
} DoorsPlaceRequestContent;

typedef struct {
  DoorsServerRequestType type;
} DoorsServerRequestContent;

typedef struct {
  DoorsRequestCategory  category;
  union {
    /* DOORS_USER_REQUEST_CATEGORY */
    DoorsUserRequestContent   *user_request_content;
    /* DOORS_PLACE_REQUEST_CATEGORY */
    DoorsPlaceRequestContent  *place_request_content;
    /* DOORS_SERVER_REQUEST_CATEGORY */
    DoorsServerRequestContent *server_request_content;
  } content;
} DoorsRequestContent;

typedef struct {
  DoorsPresenceId       id;
  DoorsDestination      destination;
  DoorsPresenceList    *list;
  DoorsDoor            *door;
  DoorsDate            *request_date;
  DoorsBytes           *client_data;
  DoorsRequestContent  *request_content;
} DoorsRequest;

typedef struct {
  DoorsUserResponseType type;
  union {
    /* DOORS_USER_DATA_RESPONSE */
    DoorsUserDataList *user_data_list;
    /* DOORS_USER_FACE_RESPONSE */
    DoorsUserFaceList *user_face_list;
    /* DOORS_USER_ALL_RESPONSE */
    DoorsUserAllList  *user_all_list;
  } data;
} DoorsUserResponseContent;    

typedef struct {
  DoorsPlaceResponseType type;
  union {
    /* DOORS_PLACE_SNAPSHOT_RESPONSE */
    DoorsPlaceSnapShot *place_snapshot;
    /* DOORS_PLACE_CACHE_RESPONSE */
    DoorsEventList     *event_list;
    /* DOORS_PLACE_PRESENCES_RESPONSE */
    DoorsPresenceList  *presences;
  } data;
} DoorsPlaceResponseContent;

typedef struct {
  DoorsServerResponseType type;
  union {
    /* DOORS_SERVER_PRESENCES_COUNT_RESPONSE */
    DoorsUlong               presences_number;
    /* DOORS_SERVER_PRESENCES_LIST_RESPONSE */
    DoorsPlacePresenceList *place_presence_list;
  } data;
} DoorsServerResponseContent;

typedef struct {
  DoorsResponseCategory  category;
  union {
    /* DOORS_USER_RESPONSE_CATEGORY */
    DoorsUserResponseContent   *user_response_content;
    /* DOORS_PLACE_RESPONSE_CATEGORY */
    DoorsPlaceResponseContent  *place_response_content;
    /* DOORS_SERVER_RESPONSE_CATEGORY */
    DoorsServerResponseContent *server_response_content;
  } content;
} DoorsResponseContent;

typedef struct {
  DoorsPresenceId        id;
  DoorsDestination       destination;
  DoorsPresenceList     *list;
  DoorsDoor             *door;
  DoorsDate             *response_date;
  DoorsBytes	        *client_data;
  DoorsResponseContent  *response_content;
} DoorsResponse;

typedef struct {
  DoorsMessageType type;
  union {
    /* DOORS_ERROR */
    DoorsError    *error;
    /* DOORS_EVENT */
    DoorsEvent    *event;
    /* DOORS_REQUEST */
    DoorsRequest  *request;
    /* DOORS_RESPONSE */
    DoorsResponse *response;
  } data;
} DoorsMessage, *DoorsMessageP;

/* Prototype Functions */

typedef void (*DoorsErrorRoutine)(
  DoorsPresenceId  id,
  DoorsError      *error
);

typedef void (*DoorsEventRoutine)(
  DoorsPresenceId  id,
  DoorsEvent      *event
);

typedef void (*DoorsRequestRoutine)(
  DoorsPresenceId  id,
  DoorsRequest    *request
);

typedef void (*DoorsResponseRoutine)(
  DoorsPresenceId  id,
  DoorsResponse   *response
);

typedef DoorsBoolean (*DoorsSetNotificationFile)(
  int file_descriptor
);

typedef DoorsBoolean (*DoorsUnsetNotificationFile)(
  int file_descriptor
);

/* Functions Calls */

extern DoorsErrorVal DoorsInitialize(
  DoorsSetNotificationFile   set_notification_file,
  DoorsUnsetNotificationFile unset_notification_file
);

extern DoorsErrorVal DoorsProcessInput(
  int file_descriptor
);

extern DoorsErrorVal DoorsAbort();
extern DoorsErrorVal DoorsExit();

extern void DoorsPrintErrorVal(
  DoorsErrorVal error_value,
  DoorsString   error_message
);

extern DoorsErrorVal DoorsCreatePresence(
  DoorsPresenceId  *pid
);
extern DoorsErrorVal DoorsDeletePresence(
  DoorsPresenceId id
);
extern DoorsPresenceInfoP DoorsGetPresenceInfo(
  DoorsPresenceId              id
);

extern DoorsErrorVal DoorsNextObjectId(
  DoorsPresenceId  id,
  DoorsObjectId   *p_object_id
);

extern DoorsString DoorsLastErrorString();

extern char *DoorsMalloc(
  DoorsUlong size
);

extern DoorsErrorVal DoorsFree(
  char *area
);

extern DoorsErrorVal DoorsConnect(
  DoorsPresenceId     id,
  DoorsBytes         *client_data,
  DoorsPresenceState *presence_state,
  DoorsPlaceState    *place_state,
  DoorsDoor          *door
);
extern DoorsErrorVal DoorsDisConnect(
  DoorsPresenceId  id,
  DoorsBytes      *client_data,
  DoorsDoor       *new_door
);

extern DoorsErrorVal DoorsSetErrorHandler(
  DoorsPresenceId   id,
  DoorsErrorRoutine error_routine
);
extern DoorsErrorVal DoorsDefaultErrorHandler(
  DoorsPresenceId  id,
  DoorsString      description_message,
  DoorsError      *error
);
extern DoorsErrorVal DoorsSendError(
  DoorsPresenceId     id,
  DoorsDestination    destination,
  DoorsPresenceList  *list,
  DoorsErrorContent  *content
);
extern DoorsErrorVal DoorsPropagateError(
  DoorsPresenceList *list,
  DoorsError        *error
);

extern DoorsErrorVal DoorsSetEventHandler(
  DoorsPresenceId   id,
  DoorsEventRoutine event_routine
);
extern DoorsErrorVal DoorsDefaultEventHandler(
  DoorsPresenceId  id,
  DoorsString      description_message,
  DoorsEvent      *event
);
extern DoorsErrorVal DoorsViewEvents(
  DoorsPresenceId         id,
  DoorsBytes             *client_data,
  DoorsBoolean            snapshot,
  DoorsEventCategoryList *categories
);
extern DoorsErrorVal DoorsSendEvent(
  DoorsPresenceId     id,
  DoorsDestination    destination,
  DoorsPresenceList  *list,
  DoorsEventContent  *content
);
extern DoorsErrorVal DoorsPropagateEvent(
  DoorsPresenceList *list,
  DoorsEvent        *event
);

extern DoorsErrorVal DoorsSetRequestHandler(
  DoorsPresenceId     id,
  DoorsRequestRoutine request_routine
);
extern DoorsErrorVal DoorsDefaultRequestHandler(
  DoorsPresenceId  id,
  DoorsString      description_message,
  DoorsRequest    *request
);
extern DoorsErrorVal DoorsSendRequest(
  DoorsPresenceId       id,
  DoorsDestination      destination,
  DoorsPresenceList    *list,
  DoorsRequestContent  *content
);
extern DoorsErrorVal DoorsPropagateRequest(
  DoorsPresenceList *list,
  DoorsRequest      *request
);

extern DoorsErrorVal DoorsSetResponseHandler(
  DoorsPresenceId      id,
  DoorsResponseRoutine response_routine
);
extern DoorsErrorVal DoorsDefaultResponseHandler(
  DoorsPresenceId  id,
  DoorsString      description_message,
  DoorsResponse   *response
);
extern DoorsErrorVal DoorsSendResponse(
  DoorsPresenceId        id,
  DoorsDestination       destination,
  DoorsPresenceList     *list,
  DoorsResponseContent  *content
);
extern DoorsErrorVal DoorsPropagateResponse(
  DoorsPresenceList *list,
  DoorsResponse     *response
);

extern DoorsErrorVal DoorsQueryUserDetails(
  DoorsPresenceId        id,
  DoorsBytes            *client_data,
  DoorsUserResponseType  type,
  DoorsUserObjectList   *query_list
);

extern DoorsErrorVal DoorsQueryPlacePresences(
  DoorsPresenceId  id,
  DoorsBytes      *client_data
);
extern DoorsErrorVal DoorsQueryCache(
  DoorsPresenceId         id,
  DoorsBytes             *client_data,
  DoorsEventCategoryList *ctgrs,
  DoorsUlong              number,
  DoorsDate              *from,
  DoorsDate              *until
);

extern DoorsErrorVal DoorsQueryServerPresencesNumber(
  DoorsPresenceId  id,
  DoorsBytes      *client_data
);
extern DoorsErrorVal DoorsQueryServerPresencesList(
  DoorsPresenceId  id,
  DoorsBytes      *client_data
);

#endif	__DOORS_H
