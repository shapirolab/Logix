-monitor(serve).
-mode(system).
-language(compound).

serve(In) :-
    In = [] | true;

    list(In) :
      link(["file.o"], Offset) |
	processor_server # start(Queue?, Ready),
	server(In, Ready?, Offset, Queue).

server(In, Ready, Offset, Queue) :-

    unknown(Ready),
    In ? {file(directory(Name), Ok'), Common} :
      Common = reply(Ok, Ok, Ok', done) |
	server,
	directory(Offset, Name, Ok);

    unknown(Ready),
    In ? {file(get_module(FileName, Data'), Ok'), Common} :
      Common = reply(Ok, Data, Data', Common'),
      Common' = reply(Ok, Ok, Ok', done) |
	server,
	get_module(Offset, FileName, Data, Ok);

    unknown(Ready),
    In ? {file(info(FileName, FileDate'), Ok'), Common} :
      Common = reply(Ok, FileDate, FileDate', Common'),
      Common' = reply(Ok, Ok, Ok', done) |
	server,
	info(Offset, FileName, FileDate, Ok);

    unknown(Ready),
    In ? {file(working_directory(WorkingDirectory), Ok'), Common} :
      Common = reply(WD, WD, WorkingDirectory, Common'),
      Common' = reply(Ok, Ok, Ok', done) |
	server,
	working_directory(Offset, WD, Ok);

    unknown(Ready),
    otherwise,
    In ? {Other, Common} :
      Queue ! {Other, Common} |
	server;

    Ready = ready : Offset = _,
      Queue = In .


info(Offset, FileName, FileDate, Ok) :-
    known(FileName) :
      execute(Offset,{7, FileName, FileDate, _}) |
	check_filedate(FileDate, Ok).

get_module(Offset, FileName, Data, Ok) :-
    known(FileName) :
      execute(Offset,{2, FileName, Data, Reply}) |
	check_reply(Reply, 0, Ok).

directory(Offset, DirName, Ok) :-
    known(DirName) :
      execute(Offset,{8, DirName, Status, _}) |
	check_reply(Status, 1, Ok).

check_reply(Reply, Good, Ok) :-

    Reply = Good :
      Ok = true ;

    Reply =\= Good :
      Ok = false(Reply) .

check_filedate(FileDate, Ok) :-

    string(FileDate) :
      Ok = true ;

    otherwise :
      Ok = false(FileDate) .

working_directory(Offset, WorkingDirectory, Ok) :-
    true :
      execute(Offset, {6, _, WD, _}),
      Ok = true |
	add_slash(WD, WorkingDirectory).

add_slash(WD, WorkingDirectory) :-

    WD = '/' :
      WorkingDirectory = WD ;

    string_to_dlist(WD, LeadingChars, TrailingSlash),
    LeadingChars = [Slash, _ | _] :
      TrailingSlash = [Slash] |
	list_to_string(LeadingChars, WorkingDirectory).
