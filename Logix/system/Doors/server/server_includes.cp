/*  $Header: /home/qiana/Repository/Logix/system/Doors/server/server_includes.cp,v 1.1 1999/07/09 07:03:28 bill Exp $ */
-language([nil]).
-mode(interrupt).

NO_DATE => [].
MINIMUM_TIME_IN_CACHE => 3600.		% in seconds = 1 hour
INDENT => 0.
LENGTH => 80.


PERSON_FULLNAME_ATTR => full_name.
PERSON_OBJECT_NAME_ATTR => object_name.
PERSON_TIMESTAMP_ATTR => details_date.
PERSON_ACCESS_DATE_ATTR => access_date.
PERSON_NEXT_ATTR => next.
PERSON_PREVIOUS_ATTR => previous.
PERSON_FACE_ATTR => face.
PERSON_DATA_ATTR => businesscard.
PERSON_OBJECTS_ATTR => objects.
PERSON_WAITING_LIST_ATTR => waiting_list.
PERSON_HAS_ICON_ATTR => has_face.

HAS_ICON_TRUE => DOORS_TRUE.
HAS_ICON_FALSE => DOORS_FALSE.

OBJECT_NAME_ATTR => object_name.
OBJECT_TIMESTAMP_ATTR => details_date.
OBJECT_FACE_ATTR => face.
OBJECT_ID_ATTR => object_id.
OBJECT_WAITING_LIST_ATTR => waiting_list.

PRESENCE_NICKNAME_ATTR => nickname.
PRESENCE_VERSION_ATTR => version.
PRESENCE_POSITION_ATTR => position.
PRESENCE_CLICK_POSITION_ATTR => click_position.
PRESENCE_CLICK_DOOR_ATTR => click_door.
PRESENCE_USER_STATE_ATTR => face_state.
PRESENCE_EDIT_ATTR => edit_state.
PRESENCE_ALERTED_ATTR => alerted.
PRESENCE_JOINED_DATE_ATTR => joined_date.
PRESENCE_TIMESTAMP_ATTR => details_date. 
PRESENCE_HAS_ICON_ATTR => has_icon.
PRESENCE_OBJECTS_ATTR => objects.
PRESENCE_OBJECT_OBJECT_ID_ATTR => object_id.
PRESENCE_OBJECT_STATE_TYPE_ATTR => object_type.
PRESENCE_OBJECT_STATE_NAME_ATTR => object_name.
PRESENCE_OBJECT_STATE_POSITION_ATTR => object_position.
PRESENCE_OBJECT_STATE_TIMESTAMP_ATTR => object_details_date.
PRESENCE_OBJECT_HAS_ICON_ATTR => object_has_icon.
PRESENCE_OBJECT_CONV_TYPE_ATTR => object_conversation_type.
PRESENCE_OBJECT_MAX_CAPACITY_ATTR => object_max_capacity.
PRESENCE_OBJECT_NUMBER_USED_ATTR => object_number_used.
PRESENCE_OBJECT_MBONE_CHANNEL_ATTR => object_mbone_channel.
PRESENCE_OBJECT_POSITION_ATTR => object_position.
PRESENCE_OBJECT_PASSENGERS_ATTR => passengers. % internal: passengers on the bus
PRESENCE_OBJECT_USED_SLOTS_ATTR => used_slots. % internal: list of used slot #s
PRESENCE_CONNECTION_ATTR => connection.
PRESENCE_CONNECTION_PRESENCEID_ATTR => presence_id.
PRESENCE_CONNECTION_OBJECT_ID_ATTR => object_id.
PRESENCE_CONNECTION_TYPE_ATTR => type.
PRESENCE_AUDIO_PORT_ATTR => audio_port.
PRESENCE_AUDIO_FOCUS_ATTR => audio_focus.
PRESENCE_TYPE_ATTR => type. % internal to server
PRESENCE_TYPE_STR => presence. % internal to server


PLACE_TIMESTAMP_ATTR => details_date.
PLACE_TYPE_ATTR => type. % internal to server
PLACE_TYPE_STR => place. % internal to server
PLACE_MASTER_ATTR => is_master. % internal to server
PLACE_MASTER_TRUE => true. % internal to server
SUBSCRIBED_EVENTS_ATTR => subscribed_events. % internal to server


% configuration file parameters
CONFIG_PLACE_IDLE_TIME => "PLACE_IDLE_TIME".
CONFIG_EVENTS_CACHE_MAX => "EVENTS_CACHE_MAX".
CONFIG_LOG => "LOG".
CONFIG_PEOPLE_CACHE_MAX => "PEOPLE_CACHE_MAX".
CONFIG_TIME_SET => [minutes, hours, days, weeks].
CONFIG_LOG_NONE => none.
CONFIG_LOG_ALL => all.
CONFIG_URL_MAPPING_SCRIPT => "URL_MAPPING_SCRIPT".
CONFIG_PRESENCES_PER_PLACE_MAX => "PRESENCES_PER_PLACE_MAX".
CONFIG_CONNECTIONS_PER_SERVER_MAX => "CONNECTIONS_PER_SERVER_MAX".
CONFIG_CLONES_TO_SERVER_MAX => "CLONES_TO_SERVER_MAX".
CONFIG_PRESENCE_IDLE_TIME => "PRESENCE_IDLE_TIME".

DOORS_ROOT_DEFAULT => "/usr/local/doors".
TOKEN_FILE => "lib/doors_license".
CONFIG_FILE => "lib/doors.conf".
LOG_FILE => "doors.server.log".
DOORS_MAPPED_URL_FILE => "lib/doors.mapped_url".
LOG_DIR => "LOG".
PLACE_IDLE_TIME_DEFAULT => minutes(20).
PLACE_IDLE_TIME_DEFAULT_STRING => "20 minutes". % must match previous def
EVENTS_CACHE_MAX_DEFAULT => 500. % transient events don't get cached
WHAT_TO_LOG_DEFAULT => [PRESENCE_EVENT_CATEGORY, PLACE_EVENT_CATEGORY].
WHAT_TO_LOG_DEFAULT_STRING =>  "people, place".

PRESENCES_PER_PLACE_MAX_DEFAULT => []. % no check to be done in the server
CONNECTIONS_PER_SERVER_MAX_DEFAULT => 0. % no check to be done in the server api
CLONES_TO_SERVER_MAX_DEFAULT => 0. % no check to be done in the server api
PRESENCE_IDLE_TIME_DEFAULT => []. % no timeout of presences
URL_MAPPING_SCRIPT_DEFAULT => []. % means no mapping function

WHAT_TO_LOG_ALL => [
			PRESENCE_EVENT_CATEGORY_STRING, 
			PLACE_EVENT_CATEGORY_STRING,
			ALERT_EVENT_CATEGORY_STRING, 
			TRANSIENT_EVENT_CATEGORY_STRING,
			OTHER_EVENT_CATEGORY_STRING].
WHAT_TO_LOG_ALL_MSG => "people, place, alert, transient, other".
PEOPLE_CACHE_MAX_DEFAULT => 100.

DOORS_CONNECT_FUNCTOR =>		doorsConnect.
DOORS_DISCONNECT_FUNCTOR =>		doorsDisconnect.
DOORS_EVENT_FUNCTOR =>			doorsEvent.
DOORS_ERROR_FUNCTOR =>			doorsError.
DOORS_QUERY_USER_DETAILS_FUNCTOR =>	doorsQueryUserDetails.
DOORS_VIEW_EVENTS_FUNCTOR =>		doorsViewEvents.
DOORS_QUERY_CACHE_FUNCTOR =>		doorsQueryCache.
DOORS_QUERY_PLACE_PRESENCES_FUNCTOR =>	doorsQueryPlacePresences.
DOORS_QUERY_SERVER_PRESENCES_COUNT_FUNCTOR => doorsQueryServerPresencesNumber.
DOORS_QUERY_SERVER_PRESENCES_LIST_FUNCTOR => doorsQueryServerPresencesList.
DOORS_UPDATE_USER_DETAILS_FUNCTOR => 	doorsUpdateUserDetails.
DOORS_CONNECT_TO_PRESENCE_FUNCTOR =>	doorsConnectToPresence.
DOORS_CONNECT_TO_OBJECT_FUNCTOR =>	doorsConnectToObject.
DOORS_QUERY_OBJECT_CONNECTIONS_FUNCTOR => doorsQueryObjectConnections.
DOORS_CREATE_OBJECT_FUNCTOR => 		doorsCreateObject.

CONNECT_CANNOT_CREATE_ERROR_STRING => "connect cannot create error".
CONNECT_CANNOT_CONNECT_ERROR_STRING => "connect cannot connect error".
CONNECT_CANNOT_COMPLETE_ERROR_STRING => "connect cannot complete error".

CACHE_OPERATION_INDEX => 1.
BROADCAST_OPERATION_INDEX => 2.
STATE_OPERATION_INDEX => 3.
LOG_OPERATION_INDEX => 4.

INITIAL_EVENTS_OPERATIONS => [MESSAGE_EVENT_CATEGORY - [cache, broadcast],
			    PRESENCE_EVENT_CATEGORY - [cache, broadcast, state],
			    PLACE_EVENT_CATEGORY - [cache, broadcast, state],
			    TRANSIENT_EVENT_CATEGORY - [broadcast, state],
			    ALERT_EVENT_CATEGORY - [cache, broadcast, state],
			    OTHER_EVENT_CATEGORY - [cache, broadcast]].
