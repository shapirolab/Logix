/*  $Header: /home/qiana/Repository/Logix/system/Doors/server/logging_monitor.cp,v 1.1.1.1 1999/07/09 07:03:28 bill Exp $ */

-language(dfcp).
-monitor(serve).
-include([api_includes,server_includes]).

serve(In) + (File, WhatToLog, FileStream) :-

  finally |
	FileStream = [];

  In ? log_file(File'), string(File') |
	File = _,
	self;

  In ? log(WhatToLog'), ground(WhatToLog') |
	WhatToLog = _,
	self;

  In ? start, string(File), ground(WhatToLog) |
	FileStream = _,
	file#put_file(File,FileStream'?,append,Ok),
        ok(Ok?, logging_file),
	self;

  In ? start, unknown(File) |
	computation#display("Cannot start logging - Log file name not defined"),
	self;

  In ? start, unknown(WhatToLog) |
	computation#display("Cannot start logging - Event types not defined"),
	self;
	
  In ? log_event(URL, PresenceId, Date, EventCategory, EventType,
		To, Contents, RecipientsList), 
  string(URL), constant(EventCategory), string(Date), ground(Contents),
  ground(RecipientsList), ground(WhatToLog) |
	codes#category_code_to_string(EventCategory, EventCategoryString),
	codes#event_code_to_string(EventCategory, EventType, EventTypeString),
	destination_code_to_recipients(To, RecipientsList, R),
	processor#interface(gmt2date(Date, Date')),
	LoggedEvent = [URL, PresenceId, R?, Date'?,
		EventCategoryString?, EventTypeString?, Contents],
	member(EventCategory, WhatToLog, Ok),
        In'' = [do_log_event(Ok?, LoggedEvent?) | In'?],
	self;

  In ? do_log_event(false, _) |
        self;

  In ? do_log_event(true, LoggedEvent), ground(LoggedEvent) |
        widgets#pretty#module([LoggedEvent],Strings),
        FileStream!Strings?,
        self;

  In ? terminate(Ok) |
	FileStream' = [], FileStream = _, In' = _,
        In'' = [set_termination(Ok)],
        self;

  In ? set_termination(Ok) |
	File = _, WhatToLog = _, FileStream = _, In' = _,
        Ok = true;

  In = [], writable(FileStream) |
	File = _, WhatToLog = _,
	FileStream = [];

  In = [], FileStream = [] |
	File = _, WhatToLog = _.


member(Elem, List, Ok) :-

    List = all | Ok = true, Elem = _;
    List ? Elem  | Ok = true, List' = _;
    List = [] | Ok = false, Elem = _;
    List ? NotElem, NotElem =\= Elem | self.

destination_code_to_recipients(To, RecipientsList, R) :-
  To = DOORS_LIST |
	R = RecipientsList;

  To = DOORS_CONVERSATION |
	R = DOORS_CONVERSATION_STRING,
	RecipientsList = _;

  To = DOORS_SERVER |
	R = DOORS_SERVER_STRING,
	RecipientsList = _;

  To = DOORS_PLACE |
	R = DOORS_PLACE_STRING,
	RecipientsList = _.
