/*  $Header: /home/qiana/Repository/Logix/system/Doors/server/sci_room.cp,v 1.1.1.1 1999/07/09 07:03:27 bill Exp $ */


-language(dfcp).
-mode(interrupt).
-include([api_includes, server_includes]).

create(IdCard,Door,Ok) :-
        +basic_room#create.

room(Rcv, Doors, Name, Type) :-
	room1(Rcv, Doors, Name, Type, _, _, _, off, done).

room1(Rcv, Doors, Name, Type, InputStream, OutputStream, I, Trace, Done) :-
        +basic_room#room;
        +sci_messages;

  finally |
	OutputStream = [].

sci_messages(Rcv, Doors, Name, Type, InputStream, OutputStream, I, Trace, Done) :-


  Rcv ? (_ : server_api_initialize(I'?, Args, Ok)) |
	I = _, InputStream = _, OutputStream = _,
	processor#doors_api(initialize(OutputStream'?, InputStream',
		Args, Reply), Ok0),
	InputStream'' = [initialized_ok(Ok0?, Reply?, Ok) | InputStream'?],
	self;

  Rcv ? (_ : input(Message)) |
	I ! Message,
	self;


  Rcv ? (_ : trace_on) |
	Trace' = on, Trace = _,
	self;

  Rcv ? (_ : trace_off) |
	Trace' = off, Trace = _,
	self;


  Rcv ? (_ : doorsPropagateEvent(URL, PresenceId, Date, 
			EventCategory, EventType, To, Contents, Recipients)), 
  string(PresenceId), ground(To), ground(Contents), ground(Recipients),
  string(URL), string(Date), constant(EventType), constant(EventCategory)  |
	add_presenceid(To, PresenceId, Recipients, Recipients1),
	Rcv'' = [doorsPropagateEvent1(URL, PresenceId, Date, 
		    EventCategory, EventType, To, Contents, Recipients1?)
	| Rcv'?],
	self;

  Rcv ? (doorsPropagateEvent1(URL, PresenceId, Date, 
			EventCategory, EventType, To, Contents, Recipients)),
  Recipients =\= [],
  string(PresenceId), ground(To), ground(Contents), ground(Recipients),
  string(URL), string(Date), constant(EventType), constant(EventCategory),
  string(Trace), string(Done)  |
	server_api#make_presences_list(PresenceId, To, Recipients, List),
	server_api#make_event_content(EventCategory, EventType, Contents,
								EventContent),
%	computation#display(term, List??, type(ground)),
	HeaderTuple = {PresenceId, To, List?, {[], [], URL}, Date},
	EventTuple = {EventCategory, EventContent?},
	listen2(EventTuple?, E1, E2),
	listen2(HeaderTuple?, H1, H2),
	OutputStream ! doorsPropagateEvent(Recipients, H1?, E1?),
	trace_event(Trace, Done, Done', Recipients, H2?, E2?),
	self;

  Rcv ? (doorsPropagateEvent1(_URL, _PresenceId, _Date, 
			_EventCategory, _EventType, _To, _Contents, [])) |
%	computation#display("Event for  no recipients not transmitted"),
	self;

  Rcv ? (_ : doorsPropagateError(ErrorCategory, ErrorData, ErrorSeverity,URL, 
				PresenceId, Contents, Recipients)),
  constant(ErrorCategory), constant(ErrorData), string(Done),
  constant(ErrorSeverity), string(URL), ground(Contents),
  constant(PresenceId), ground(Recipients), string(Trace) |
%	computation#display(term, {"server sent error: ", H3?, E3?}, 
%								type(ground)),
	processor#interface(gmtime(Date)),
	HeaderTuple = {PresenceId, DOORS_LIST, 
					[PresenceId], {[], [], URL}, Date?},
	ErrorTuple = {ErrorSeverity, ErrorCategory, ErrorData},
	listen3(ErrorTuple?, E1, E2, _E3),
	listen3(HeaderTuple?, H1, H2, _H3),
	OutputStream ! doorsPropagateError(Recipients, H1?, E1?),
	trace_error(Trace, Done, Done', Recipients, H2?, E2?),
	self;

  Rcv ? (_ : doorsPropagateRequest(PresenceId, RequestCategory,
					RequestType, Request)),
  string(PresenceId), constant(RequestType), string(Done),
  ground(Request), constant(RequestCategory), string(Trace) |
	processor#interface(gmtime(Date)),
	HeaderTuple = {PresenceId, DOORS_LIST, 
					[PresenceId], {[], [], []}, Date?},
	RequestTuple = {[], RequestCategory,{RequestType, Request}},
	listen2(RequestTuple?, R1, R2),
	listen2(HeaderTuple?, H1, H2),
	OutputStream ! doorsPropagateRequest([PresenceId], H1?, R1?),
	trace_request(Trace, Done, Done', [PresenceId], H2?, R2?),
	self;

  Rcv ? (_ : doorsPropagateResponse(URL, PresenceId, ClientData, 
		ResponseCategory, ResponseType, Response)),
  string(PresenceId), constant(ClientData), constant(ResponseType),
  ground(Response), constant(ResponseCategory), string(Trace), string(Done) |
	processor#interface(gmtime(Date)),
	HeaderTuple = {PresenceId, DOORS_LIST, 
					[PresenceId], {[], [], URL}, Date?},
	ResponseTuple = {ClientData, ResponseCategory,{ResponseType, Response}},
	listen2(ResponseTuple?, R1, R2),
	listen2(HeaderTuple?, H1, H2),
	OutputStream ! doorsPropagateResponse([PresenceId], H1?, R1?),
	trace_response(Trace, Done, Done', [PresenceId], H2?, R2?),
	self;


  Rcv ? Other, otherwise |
	date#get_date(Date),
	utils#append_strings(["Received unexpected internal message on ", 
					Date?, " : ", Other, "."], Msg),
	computation#display(term, Msg?, type(ground)),
	self;

  known(InputStream) |
	sci_messages1.

sci_messages1(Rcv, Doors, Name, Type, InputStream, OutputStream, I, Trace, Done) :-

  InputStream ? initialized_ok(Error, _, Ok), Error =\= true |
	Ok = false,
	InputStream''= [report_error(Error) | InputStream'?],
	room1;

  InputStream ? initialized_ok(true, 
			reply(DOORS_OK_ERROR_VAL, DOORS_NO_WARNING), Ok?) |
	Ok = true,
	room1;

  InputStream ? initialized_ok(true, reply(DOORS_OK_ERROR_VAL, Warning), Ok?),
  Warning =\= DOORS_NO_WARNING |
	Ok = true,
	InputStream'' = [report_warning(Warning) | InputStream'?],
	room1;

  InputStream ? initialized_ok(true, reply(ApiError, DOORS_NO_WARNING), Ok?),
  ApiError =\= DOORS_OK_ERROR_VAL |
	Ok = false,
	InputStream'' = [report_error(ApiError) | InputStream'?],
	room1;

  InputStream ? initialized_ok(true, reply(ApiError, Warning), Ok?),
  ApiError =\= DOORS_OK_ERROR_VAL,  Warning =\= DOORS_NO_WARNING |
	Ok = false,
	InputStream'' = [report_error(ApiError), report_warning(Warning)
	| InputStream'?],
	room1;

  InputStream ? doors_api_error(ErrorCode), integer(ErrorCode) |
	InputStream'' = [report_error(ErrorCode) | InputStream'?],
	room1;

  InputStream ? report_error(ApiError) | 
        codes#api_error_code_to_string(ApiError, String, Fatal),
%       processor#terminal(output(String?), End),
	computation#display(term, String?, [type(ground), close(end, End)]),
        InputStream'' = [abort_if_fatal(End?, Fatal?) | InputStream'?],
        room1;

  InputStream ? report_warning(Warning) | 
        codes#api_warning_code_to_string(Warning, String),
        processor#terminal(output(String?), End),
        InputStream'' = [abort_if_fatal(End?, false) | InputStream'?],
        room1;
 
  InputStream ? doors_translation_event(Code) |
	date#get_date(Date),
	utils#append_strings([Date?, " INTERNAL event translation error ", 
				Code, "."], Msg),
	computation#display(Msg?),
	room1;

  InputStream ? doors_translation_error(Code) |
	date#get_date(Date),
	utils#append_strings([Date?, " INTERNAL error translation error ", 
							Code, "."], Msg),
	computation#display(Msg?),
	room1;

  InputStream ? doors_translation_request(Code) |
	date#get_date(Date),
	utils#append_strings([Date?, " INTERNAL request translation error ", 
							Code, "."], Msg),
	computation#display(Msg?),
	room1;

  InputStream ? doors_translation_response(Code) |
	date#get_date(Date),
	utils#append_strings([Date?, "INTERNAL response translation error ", 
							Code, "."], Msg),
	computation#display(Msg?),
	room1;

  InputStream ? doorsError(HeaderTuple, ErrorTuple),
  HeaderTuple = {PresenceId, _To, _List, _Door, Date},
  ErrorTuple = {ErrorSeverity, ErrorCategory,  ErrorType(_)} |
	utils#append_strings(["Received error message on ", Date, 
		" : PresenceId = ",
		PresenceId, " ErrorSeverity = ", ErrorSeverity,
		"  ErrorCategory = ",  ErrorCategory, " ErrorType = ",
		ErrorType], Msg),
	computation#display(Msg?),
	room1;

  InputStream ? doorsError(HeaderTuple, ErrorTuple),
  HeaderTuple = {PresenceId, _To, _List, _Door, Date},
  ErrorTuple = {ErrorSeverity, ErrorCategory,  ErrorType} |
	utils#append_strings(["Received error message on ", Date, 
		" : PresenceId = ",
		PresenceId, " ErrorSeverity = ", ErrorSeverity,
		"  ErrorCategory = ",  ErrorCategory, " ErrorType = ",
		ErrorType], Msg),
	computation#display(Msg?),
	room1;

  InputStream ? doorsEvent(HeaderTuple, EventTuple),
  ground(HeaderTuple), ground(EventTuple), string(Trace), string(Done) |
	server_api#translate_event(HeaderTuple, EventTuple, Term),
	InputStream'' = [pass_term(action, HeaderTuple, Term?) | InputStream'?],
	trace_event(Trace, Done, Done', server, HeaderTuple, EventTuple),
	room1;

  InputStream ? doorsRequest(HeaderTuple, RequestTuple),
  ground(HeaderTuple), ground(RequestTuple), string(Trace), string(Done) |
	server_api#translate_request(HeaderTuple, RequestTuple, Term, Action),
	InputStream'' = [pass_term(Action?, HeaderTuple, Term?)
	 | InputStream'?],
	trace_request(Trace, Done, Done', server, HeaderTuple, RequestTuple),
	room1;

  InputStream ? doorsResponse(HeaderTuple, ResponseTuple),
  ground(HeaderTuple), ground(ResponseTuple), string(Trace), string(Done) |
	server_api#translate_response(HeaderTuple, ResponseTuple, Term),
	InputStream'' = [pass_term(no_action, HeaderTuple, Term?)
	 | InputStream'?],
	trace_response(Trace, Done, Done', server, HeaderTuple, ResponseTuple),
	room1;

  InputStream ? pass_term(action, HeaderTuple, Term), 
  HeaderTuple = {PresenceId, _To, _List, _Door, _Date},
  Term =\= [] |
	I = [action(PresenceId), Term | I'?],
	room1;

  InputStream ? pass_term(no_action, HeaderTuple, Term), 
  HeaderTuple = {_PresenceId, _To, _List, _Door, _Date},
  Term =\= [] |
	I ! Term,
	room1;

  InputStream ? pass_term(_, _, []) |
	room1;

  InputStream ? abort_if_fatal(End, false), ground(End) |
	room1;

  InputStream ? abort_if_fatal(End, abort), ground(End) |
	InputStream' = _,
	logging_monitor#[terminate(Ok)],
	InputStream'' = [abort(Ok?)],
	room1;

  InputStream ? abort(Ok), known(Ok) |
	room1;

  InputStream = [] |
	I = _, OutputStream = [], Rcv = _, Done =_, Trace = _,
	Doors = _, Name = _, Type = _,
	doors_server#abort;

  InputStream ? abort_if_fatal(End, exit), ground(End) |
	InputStream' = _,
	logging_monitor#[terminate(Ok)],
	InputStream'' = [exit(Ok?)],
	room1;

  InputStream = [exit(Ok)], known(Ok) |
	doors_server#exit,
	Doors = _, Name = _, Type = _,
	I = _, OutputStream = [], Rcv = _, Done =_, Trace = _;	

  InputStream ? Other, otherwise |
	date#get_date(Date),
	utils#append_strings([Date?, " INTERNAL unexpected message ", 
							Other, "."], Msg),
	computation#display(term, Msg?, type(ground)),
	room1.

	

trace_event(Trace, Done, Done1, Recipients, HeaderTuple, EventTuple):-
  HeaderTuple = {PresenceId, To, List, Door, Date},
  EventTuple = {EventCategory, EventContent}, ground(Recipients),
  constant(EventCategory),  constant(To), Trace = on, string(Done) |
	get_event_type_and_contents(EventContent, EventType, Contents),
	listen2(EventType?, EventType1, EventType2),
	get_title(Recipients, event, Title),	
	codes#category_code_to_string(EventCategory, CategoryString),
	codes#event_code_to_string(EventCategory, EventType1?, TypeString),
	codes#destination_code_to_string(To, DestString),
	Msg =  ["
", Title?, " 
  Header:
    ",			("PresenceId" : PresenceId), 
			("Destination" : (To : DestString?)), "  ",
			("PresencesList" : List),"  ",
			("Door" : Door), "  ",
			("Date" : Date), "
  Event:
    ",
			("Category" : (EventCategory:CategoryString?)), "  ",
			("Type" : (EventType2? : TypeString?)), "  ",
			("Contents" : Contents?), "
  Recipients:
     ",
			Recipients],
	terms_to_string#acyclic_grounded_terms_to_string(Msg?,
			INDENT, LENGTH, MsgStr),
	computation#display(term, MsgStr?, [close(Done, Done1), type(ground)]);

  Trace =\= on, string(Done) |
	Done1 = Done,
	Recipients = _, HeaderTuple = _, EventTuple = _.

trace_error(Trace, Done, Done1, Recipients, HeaderTuple, ErrorTuple) :-
  HeaderTuple = {PresenceId, To, List, Door, Date},
  ErrorTuple = {ErrorSeverity, ErrorCategory, {ErrorType, ClientData}},
  constant(ErrorCategory), constant(ErrorSeverity), constant(ErrorType),
  ground(Recipients), constant(To), Trace = on, string(Done) |
	get_title(Recipients, error, Title),
	codes#destination_code_to_string(To, DestString),
	codes#error_category_to_string(ErrorCategory, CategoryString),
	codes#error_code_to_string(ErrorCategory, ErrorType, TypeString),
	codes#error_severity_to_string(ErrorSeverity, SeverityString),
	Msg = ["
", Title?, "
   Header:
      ", 	("PresenceId" : PresenceId), 
		("Destination" : (To : DestString?)), "  ",
		("PresencesList" : List), "  ",
		("Door" : Door), "  ",
		("Date" : Date), "
  Error:
    ",
		("ErrorSeverity" : (ErrorSeverity : SeverityString?)), "  ",
		("ErrorCategory" : (ErrorCategory : CategoryString?)), "  ",
		("ErrorType" : (ErrorType : TypeString?)), "  ",
		("ClientData" : ClientData), "  ",
		("Recipients" : Recipients)
		],
	terms_to_string#acyclic_grounded_terms_to_string(Msg?, 
			INDENT, LENGTH, MsgStr),
	computation#display(term, MsgStr?, [close(Done, Done1), type(ground)]);

  HeaderTuple = {PresenceId, To, List, Door, Date},
  ErrorTuple = {ErrorSeverity, ErrorCategory, ErrorType},
  constant(ErrorCategory), constant(ErrorSeverity), constant(ErrorType),
  ground(Recipients), constant(To), Trace = on, string(Done) |
	get_title(Recipients, error, Title),
	codes#destination_code_to_string(To, DestString),
	codes#error_category_to_string(ErrorCategory, CategoryString),
	codes#error_code_to_string(ErrorCategory, ErrorType, TypeString),
	codes#error_severity_to_string(ErrorSeverity, SeverityString),
	Msg = ["
", Title?, "
   Header:
      ", 	("PresenceId" : PresenceId), 
		("Destination" : (To : DestString?)), "  ",
		("PresencesList" : List), "  ",
		("Door" : Door), "  ",
		("Date" : Date), "
  Error:
    ",
		("ErrorSeverity" : (ErrorSeverity : SeverityString?)), "  ",
		("ErrorCategory" : (ErrorCategory : CategoryString?)), "  ",
		("ErrorType" : (ErrorType : TypeString?)), "  ",
		("Recipients" : Recipients)
		],
	terms_to_string#acyclic_grounded_terms_to_string(Msg?, 
			INDENT, LENGTH, MsgStr),
	computation#display(term, MsgStr?, [close(Done, Done1), type(ground)]);

    Trace =\= on, string(Done) |
	Done1 = Done,
	Recipients = _, HeaderTuple = _, ErrorTuple = _.


trace_request(Trace, Done, Done1, Recipients, HeaderTuple, RequestTuple) :-

  HeaderTuple = {PresenceId, To, List, Door, Date},
  RequestTuple = {ClientData, RequestCategory,{RequestType, Request}},
  constant(RequestCategory), constant(RequestType), ground(Recipients),
  constant(To), Trace = on, string(Done) |
	get_title(Recipients, request, Title),
	codes#destination_code_to_string(To, DestString),
	codes#request_category_code_to_string(RequestCategory,CategoryString),
	codes#request_code_to_string(RequestCategory, 
						RequestType, TypeString),
	Msg =   ["
", Title?, "
  Header:
      ", 	("PresenceId" : PresenceId), 
		("Destination" : (To : DestString?)), "  ",
		("PresencesList" : List), "  ",
		("Door" : Door), "  ",
		("Date" : Date), "
  Request:
    ",		("ClientData" : ClientData), "  ",
		("RequestCategory" : (RequestCategory:CategoryString?)), "  ",
		("RequestType" : (RequestType:TypeString?)), "  ",
		("Request" : Request), "  ",
		("Recipients" : Recipients)],
	terms_to_string#acyclic_grounded_terms_to_string(Msg?, 
			INDENT, LENGTH, MsgStr),
	computation#display(term, MsgStr?, [close(Done, Done1), type(ground)]);

  HeaderTuple = {PresenceId, To, List, Door, Date},
  RequestTuple = {ClientData, RequestCategory,{RequestType}},
  constant(RequestCategory), constant(RequestType), ground(Recipients),
  constant(To), Trace = on, string(Done) |
	get_title(Recipients, request, Title),
	codes#destination_code_to_string(To, DestString),
	codes#request_category_code_to_string(RequestCategory,CategoryString),
	codes#request_code_to_string(RequestCategory, 
						RequestType, TypeString),
	Msg =   ["
", Title?, "
  Header:
      ", 	("PresenceId" : PresenceId), 
		("Destination" : (To : DestString?)), "  ",
		("PresencesList" : List), "  ",
		("Door" : Door), "  ",
		("Date" : Date), "
  Request:
    ",		("ClientData" : ClientData), "  ",
		("RequestCategory" : (RequestCategory:CategoryString?)), "  ",
		("RequestType" : (RequestType:TypeString?)), "  ",
		("Recipients" : Recipients)],
	terms_to_string#acyclic_grounded_terms_to_string(Msg?, 
			INDENT, LENGTH, MsgStr),
	computation#display(term, MsgStr?, [close(Done, Done1), type(ground)]);


  Trace =\= on, string(Done) |
	Done1 = Done,
	Recipients = _, HeaderTuple = _, RequestTuple = _.

	

trace_response(Trace, Done, Done1, Recipients, HeaderTuple, ResponseTuple) :-
  HeaderTuple = {PresenceId, To, List, Door, Date},
  ResponseTuple = {ClientData, ResponseCategory,{ResponseType, Response}},
  constant(ResponseCategory), constant(ResponseType), ground(Recipients),
  constant(To), Trace = on, string(Done) |
	get_title(Recipients, response, Title),
	codes#destination_code_to_string(To, DestString),
	codes#response_category_code_to_string(ResponseCategory,CategoryString),
	codes#response_code_to_string(ResponseCategory, 
						ResponseType, TypeString),
	Msg =  [ "
", Title?, "
  Header:
      ", 	("PresenceId" : PresenceId),
		("Destination" : (To : DestString?)), "  ",
		("PresencesList" : List), "  ",
		("Door" : Door), "  ",
		("Date" : Date), "
  Response:
    ",		("ClientData" : ClientData), " , ",
		("ResponseCategory":(ResponseCategory:CategoryString?)), " , ",
		("ResponseType" : (ResponseType:TypeString?)), " , ",
		("Response" : Response), " , ",
		("Recipients" : Recipients)], 
	terms_to_string#acyclic_grounded_terms_to_string(Msg?, 
			INDENT, LENGTH, MsgStr),
	computation#display(term, MsgStr?, [close(Done, Done1), type(ground)]);

  Trace =\= on, string(Done) |
	Done1 = Done,
	Recipients = _, HeaderTuple = _, ResponseTuple = _.


get_title(Recipients, Type, Title) :-
  Recipients = server |
	utils#append_strings(["RECEIVED ", Type, " "], Title);

  Recipients =\= server |
	utils#append_strings(["PROPAGATED ", Type, " "], Title).

get_event_type_and_contents(EventContent, EventType, Contents) :-
  EventContent = {E, C} |
	EventType = E,
	Contents = C;

  EventContent = {E} |
	EventType = E,
	Contents = none;

  otherwise |
	EventType = unknown,
	Contents = EventContent.

add_presenceid(To, PresenceId, Recipients, Recipients1) :-

 To =\= DOORS_LIST |
	PresenceId = _,
	Recipients1 = Recipients;

 To = DOORS_LIST, constant(PresenceId), ground(Recipients) |
	sets#element(PresenceId, Recipients, Result),
	add_presenceid1(Result?, PresenceId, Recipients, Recipients1).

add_presenceid1(Result, PresenceId, Recipients, Recipients1) :-
  Result = true |
	PresenceId = _,
	Recipients1 = Recipients;

  Result =\= true |
	Recipients1 = [PresenceId | Recipients].
