/*

File server
William Silverman - 3/89

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:02:56 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/system/file.cp,v $

Copyright (C) 1989, Weizmann Institute of Science - Rehovot, ISRAEL

*/
-export([execute_in_context/2,
	 get_source/5, get_file/4,
	 put_file/4, put_module/2, put_module/3,
	 fileinfo/2, working_directory/1, isdirectory/2, pwd/1,
	 bread/3, bwrite/3, bappend/3
	]
).
-mode(interrupt).
-language(compound).

procedure execute_in_context(Path, FileExecute).
procedure get_file(FileId, GetFileData, GetFileOptions, GetStatus).
procedure get_source(FileId, Chars, GetStatus, AbsFileName, Date).
procedure put_file(FileId, PutFileData, PutFileOptions, PutStatus).
procedure put_module(FileId, Module).
procedure put_module(FileId, Module, ModuleResult).
procedure fileinfo(FileId, Date).
procedure working_directory(DirectoryName).
procedure isdirectory(DirectoryName, SystemReply).
procedure bread(FileName, FileType, Stream).
procedure bwrite(FileName, FileType, Stream).
procedure bappend(FileName, FileType, Stream).

FileExecute ::= execute_in_context(Path, NamedFileCommand) ; FileCommand.

Path ::= String ; [String] ; context(Vector) ; context(Vector, Any, Any) ;
	 {`"#", Path, Path}.

FileCommand ::=	get_file(FileId, GetFileData, GetFileOptions, GetStatus) ;
		get_source(FileId, Chars, GetStatus, AbsFileName, Date) ;
		put_file(FileId, PutFileData, PutFileOptions, PutStatus) ;
		put_module(FileId, Module) ;
		put_module(FileId, Module, ModuleResult) ;
		fileinfo(FileId, Date) ;
		isdirectory(DirectoryName, SystemReply) ;
		pwd(DirectoryName) ;
		working_directory(DirectoryName).

NamedFileCommand ::=
		get_file(FileName, GetFileData, GetFileOptions, GetStatus) ;
		get_source(FileName, Chars, GetStatus, AbsFileName, Date) ;
		put_file(FileName, PutFileData, PutFileOptions, PutStatus) ;
		put_module(FileName, Module) ;
		put_module(FileName, Module, ModuleResult) ;
		fileinfo(FileName, Date) ;
		isdirectory(DirectoryName, SystemReply) ;
		pwd(DirectoryName) ;
		working_directory(DirectoryName).

FileId ::= FileName ; FileName | Path.
FileName, DirectoryName ::= String.
FileType ::= constant ; characters(Eol).
Eol	::= Integer.
GetFileData ::= String.
GetOption ::= chars ; string.
GetFileOptions ::= [GetOption].
PutOption ::= chars ; string ; append.
PutFileOptions ::= [PutOption].
Chars ::= [Integer].
PutFileData ::= String ; Integer ; [(String ; Integer ; [])].
AbsFileName, Date ::= String.
GetStatus ::= found ; not_found.
PutStatus ::= true ; write_error.
ModuleResult ::= import ; module(Module, AbsFileName, Date).
Module ::= String.
SystemReply ::= true ; false(Any).

procedure execute_in_context(Path, FileExecute).

execute_in_context(Context, Goal) :-
    arg(2, Goal, Name) |
	computation # Context # '_unique_id'(UID),
	select(Goal, UID, Name).

select(Command, UID, Name) :-

    Command = get_source(_, Chars?, Status?, NameCp?, Date?) |
	get_source1(Name, Chars, Status, NameCp, UID),
	fileinfo1(NameCp, Date);

    Command = get_file(_, Data?, Options, Status?) |
	get_file_operation(Options, Operation),
	make_absolute(Name, UID, get_file, FileName),
	do_get_file(FileName, {Status, Data}, Operation);

    Command = put_file(_, Data, Options, Status?) |
	put_file_operation(Options, Operation),
	make_absolute(Name, UID, put_file, FileName),
	do_put_file(Operation, FileName, Data, Status);

    Command = put_module(_, Module) |
	put_module(Name, UID, Module, _);

    Command = put_module(_, Module, ModuleResult?) |
	put_module(Name, UID, Module, ModuleResult);

    Command = bread(_File, Type, Out) |
	buffered_io(read, Name, Type, Out, UID);

    Command = bwrite(_File, Type, Out) |
	buffered_io(write, Name, Type, Out, UID);

    Command = bappend(_File, Type, Out) |
	buffered_io(append, Name, Type, Out, UID);

    Command = fileinfo(_, Date?) |
	fileinfo(UID, Name, Date);

    Command = isdirectory(_, Reply?) |
	isdirectory(UID, Name, Reply);

    Command = pwd(UID?) : Name = _ ;

    Command = working_directory(Directory) : UID = _, Name = _ |
	processor # file(working_directory(Directory));

    otherwise : UID = _, Name = _ |
	fail(unknown, Command).			% invalid command

/*
** file_path - Path  represents a scoped  Name .  Find the  Name
**             and the unique identifier (UID) of the scope.
*/

file_path(Path, UID, Name) :-

    string(Path),
    Path =\= self, Path =\= super, Path =\= computation, Path =\= processor :
      Name = Path |
	computation # '_unique_id'(UID);

    Path = [] :
      UID = '',
      Name = [] ;

    Path = _#_ |
	computation_utils # call_id_goal(Path, PID, Goal, Ok),
	self,
	string_name(Path, PID, Goal, Ok, Path');

    Path = [String | SID],
    string(String) :
      Name = String |
	SID # '_unique_id'(UID);

    otherwise |
	computation # Path # service_id(Path'),
	self.

/*
** string_name - verify that  Path1  is legitimately scoped, and return
**               the working path,  Path2 .  This is a service identifier
**               when  Target  is a string, and  Target  itself, otherwise.
*/

string_name(Path1, PID, Target, Ok, Path2) :-

    Ok = true,
    string(Target) : Path1 = _,
      Path2 = [Target | PID] ;

    Ok = false(_) : PID = _, Target = _,
      Path2 = ['' | Path1] ;

    otherwise /* Ok = true */ : Path1 = _, PID = _, Ok = _,
      Target = Path2 .

/*
** select - recognize  Command  and continue with appropriate action.
*/

get_source(Path, Chars?, Status?, NameCp?, Date?) :-
	file_path(Path, UID, Name),
	get_source1(Name, Chars, Status, NameCp, UID),
	fileinfo1(NameCp, Date).

get_file(Path, Data?, Options, Status?) :-
	file_path(Path, UID, Name),
	get_file_operation(Options, Operation),
	make_absolute(Name, UID, get_file, FileName),
	do_get_file(FileName, {Status, Data}, Operation).

put_file(Path, Data, Options, Status?) :-
	file_path(Path, UID, Name),
	put_file_operation(Options, Operation),
	make_absolute(Name, UID, put_file, FileName),
	do_put_file(Operation, FileName, Data, Status).

put_module(Path, Module) :-
	file_path(Path, UID, Name),
	put_module(Name, UID, Module, _).

put_module(Path, Module, ModuleResult?) :-
	file_path(Path, UID, Name),
	put_module(Name, UID, Module, ModuleResult).

bread(Path, Type, Out) :-
	file_path(Path, UID, Name),
	buffered_io(read, Name, Type, Out, UID).

bwrite(Path, Type, Out) :-
	file_path(Path, UID, Name),
	buffered_io(write, Name, Type, Out, UID).

bappend(Path, Type, Out) :-
	file_path(Path, UID, Name),
	buffered_io(append, Name, Type, Out, UID).

fileinfo(Path, Date?) :-
	file_path(Path, UID, Name),
	fileinfo(UID, Name, Date).

isdirectory(Path, Reply?) :-
	file_path(Path, UID, Name),
	isdirectory(UID, Name, Reply).

pwd(UID?) :-
	computation # '_unique_id'(UID).

working_directory(Directory) :-
	processor # file(working_directory(Directory)).

/***************************** S T A T U S ***********************************/

/*
** fileinfo - Find the  Date  when file  Name  was last modified.
*/

fileinfo(UID, Name, Date) :-
	processor # file(info(Abs, Date), Ok),
	make_absolute(Name, UID, fileinfo, Abs),
	date_ok(Date, Ok).

fileinfo1(AbsFile, Date) :-
    known(AbsFile) |
	processor # file(info(AbsFile, Date), Ok),
	date_ok(Date, Ok).

date_ok(Date, Ok) :-

    known(Date) : Ok = _ ;

    Ok = false(_) |
	unify_without_failure(0, Date).

/*
** isdirectory - Determine whether file  Name  is a directory.
*/

isdirectory(UID, Name, SystemReply) :-

    Name =\= [] |
	processor # file(directory(AbsDir), SystemReply),
	make_absolute(Name, UID, isdirectory, AbsDir);

    Name = [] : UID = _,
      SystemReply = true .

/****************************** I N P U T ************************************/

/*
** get_file_operation - Select the appropriate operation, according to
**                      Options .
*/

get_file_operation(Options, Operation) + (AccessType = get) :-

    Options ? chars : Options' = _, AccessType=_,
      Operation = get ;

    Options ? string : Options' = _, AccessType=_,
      Operation = get_module ;

    Options ? characters : AccessType = _,
      AccessType' = characters(256) |
	self;

    Options ? characters(Eol) : AccessType = _,
      AccessType' = characters(Eol) |
	self;

    Options ? constants : AccessType = _,
      AccessType' = constants |
	self;

    Options ? string : Options' = _, AccessType = _,
      Operation = get_module ;

    Options ? _,
    otherwise |
	self;

    Options = []  :
      Operation = AccessType ;

    Options =\= [_|_], Options =\= [] :
      Options' = [Options] |
	self.

/*
** get_source1 - Get the list of  Chars  for source file  Name .
*/

get_source1(Name, Chars, Status, NameCp, UID) :-
	processor # file(get(NameCp, String), Ok),
	make_absolute(Name, UID, get_source, NewName),
	concatenate(NewName, '.cp', NameCp),
	file_chars(Ok, String, Chars, Status).

  file_chars(Ok, String, Chars, Reply) :-

    Ok = true :
      Reply = found |
	string_to_list(String, Chars);

    Ok =\= true : String = _,
      Chars = [],
      Reply = not_found.

/***************************** O U T P U T ***********************************/

/*
** put_file_operation - Select the appropriate operation, according to
**                      Options .
*/

put_file_operation(Options, Operation) + (AccessType = write) :-

    Options ? chars : Options' = _, AccessType = _,
      Operation = write ;

    Options ? string : Options' = _, AccessType = _,
      Operation = put_string ;

    Options ? append, string(AccessType) :
      AccessType' = append |
	self;

    Options ? write, string(AccessType) :
      AccessType' = write |
	self;

    Options ? append, AccessType = _(Modifier) :
      AccessType' = append(Modifier) |
	self;

    Options ? write, AccessType = _(Modifier) :
      AccessType' = write(Modifier) |
	self;

    Options ? characters, string(AccessType) : 
      AccessType' = AccessType(characters(256)) |
	self;

    Options ? characters(Eol), string(AccessType) : 
      AccessType' = AccessType(characters(Eol)) |
	self;

    Options ? constants, string(AccessType) : 
      AccessType' = AccessType(constants) |
	self;

    Options ? _,
    otherwise |
	self;

    Options = [] :
      Operation = AccessType ;

    Options =\= [_|_], Options =\= [] :
      Options' = [Options] |
	self.

/*
** write_buffers - Serve buffered output, writing  Buffers  as  Data
**                 becomes available.
*/

write_buffers(FileName, Last, Data, Buffers) :-

    Last = true,
    Data =\= [] |
	data_string_a(Data, String, Data', Reply),
	write_buffer(FileName, Reply, String, Last', Buffers, Buffers'),
	self;

    Last = Failure(Because) : FileName = _, Data = _,
      Buffers = [] |
	computation # diagnostic((FileName : Failure, Because)) ;

    Last = true,
    Data = [] : FileName = _,
      Buffers = [] ;

    Last = false : Data = _, FileName = _,
      Buffers = [] .

write_buffer(Name, Ok, String, Status, Bs1, Bs2) :-

    Ok = ok,
    known(String) :
      Bs1 = [{String, Reply} | Bs2] |
	write_reply(Reply, Name, Status) ;

    Ok = ng(Reply) : Name = _, String = _,
      Status = Reply, Bs2 = Bs1 .

write_reply(N, Name, Reply) :-

    N = 0 : Name = _,
      Reply = true;

    N = true : Name = _,
      Reply = true;

    N =\= 0, N =\= true, N =\= false(_) :
      Reply = false |
	not_written(Name, N);

    N = false(Because) :
      Reply = false |
	not_written(Name, Because).

/*
** put_module - Write  Module  to (order of preference) the  Bin
**              subdirectory or to the directory identified by  UID .
**              If neither is writable, try the working directory.
**              The  Module  is written to file  Name.bin .
*/

put_module(Name, UID, Module, ModuleResult) :-

    module(Module),
    string(Name) |
	processor # Puts?,
	make_absolute('', UID, put_module, UDir),
	concatenate(Name, '.bin', NameBin),
	concatenate(UDir, NameBin, UNameBin),
	concatenate('Bin/', NameBin, BinNameBin),
	concatenate(UDir, BinNameBin, UBinNameBin),
	put_module_file([UBinNameBin, UNameBin, NameBin], Module,
			ModuleResult, Puts
	);

    otherwise :  UID = _, ModuleResult = _ |
	make_absolute(Name, UID, put_module, _),	% diagnostic
	put_module_error(Name, Module).

put_module_error(Name, Module) :-

    module(Module) : Name = _ ;

    otherwise |
	fail("not module" - put_module(Name,Module)).


put_module_file(Names, Module, ModuleResult, Puts)
		+ (IOReply = false, Name = _) :-

    IOReply = true : Names = _,
      ModuleResult = module(Module, Name, Dated?),
      Puts = file(info(Name, Dated)) |
	written(Name);

    IOReply =\= true,
    Names ? Name' : Name = _,
      Puts ! file(put_module(Name', Module), IOReply') |
	self;

    IOReply =\= true,
    Names = [] :
      ModuleResult = module(Module, Name, 0),
      Puts = [] |
	write_reply(IOReply, Name, _).

/******************************* put displays ********************************/

written(Name) :-
    known(Name) |
	computation # comment((file : Name - written)).


not_written(Name, Because) :-
	computation # diagnostic((Name : write_failed, Because)).

/******************** D A T A   O P E R A T I O N S **************************/

/* Improved  K L U G E  for put_file/4*/

data_string_a(Data, String, Rest, Reply) :-

    Data = [] :
      String = "",
      Rest = [],
      Reply = ok ;

    Data ? List,
    List ? Datum :
      Data'' = [Datum, List' | Data'] |
	self;

    Data ? [] |
	self;

    Data ? Constant,
    constant(Constant),
    convert_to_string(Constant, String^) :
      Rest = Data',
      Reply = ok ;

    constant(Data), Data =\= [],
    convert_to_string(Data, String^) :
      Rest = [],
      Reply = ok ;

    otherwise :
      String = '',
      Rest = [],
      Reply = ng(data_error(Data)) .

/*************************** File Name Operations ****************************/

make_absolute(Name, UID, OpCode, NewName) :-

    string_to_dlist(Name, [First | _], []),
    First =:= ascii('/') : UID = _, OpCode = _,
    Name = NewName ;

    otherwise,
    string_to_dlist(Name, NL, []),
    string_to_dlist(UID, UIDNL, NL) : OpCode = _ |
	list_to_string(UIDNL, NewName);

    otherwise,
    string(UID) : Name' = '?' |
	fail("bad file name" - OpCode(Name)),
	self;

    UID = false(Reason) :
      NewName = '?' |
	fail(Reason - OpCode(Name)).


concatenate(Name, Suffix, NameSuffix) :-
    string_to_dlist(Name, AsciiName, AsciiSuffix),
    string_to_dlist(Suffix, AsciiSuffix, []) |
	list_to_string(AsciiName, NameSuffix).


do_get_file(FileName, GetFileInfo, Operation) :-
  Operation =\= constants,
  Operation =\= characters(_) :
    GetFileInfo = {StatusOut?, StringOut} |
	processor # file(Operation(FileName, String1), Ok),
	file_string(Ok, String1, StringOut, StatusOut);

  otherwise,
  GetFileInfo = {Status, FileData} |
	set_status(FileData, Status),
	buffered_io(read, FileName, Operation, FileData, "").

  file_string(Ok, String1, String2, Reply) :-

    Ok = true :
      String2 = String1,
      Reply = true;

    Ok =\= true : String1 = _,
      String2 = "",
      Reply = not_found.

do_put_file(Operation, FileName, Data, Status) :-

  Operation = put_string |
	do_put_string(FileName, Data, Status);

  string(Operation),
  Operation =\= put_string :
     Operation' = Operation(characters(256)) |
	write_buffers(FileName, OpenStatus, Data, Data'),
	open_write(Data'', Data', OpenStatus),
	self;

    Operation = FileAccess(DataAccess) |
	set_status(Data, Status),
	buffered_io(FileAccess, FileName, DataAccess, Data, "").

do_put_string(FileName, Data, Status) :-

    string(Data) |
	processor # file(put_module(FileName, Data), Ok),
	write_reply(Ok, FileName, Status);

    otherwise :
      Status = write_error |
	fail("not a string" - put_file(FileName, Data, [string])).

open_write(Data1, Data2, OpenStatus) :-

    Data1 ? open :
      Data2 = Data1',
      OpenStatus = true ;

    Data1 ? false(Diagnostic) :
      Data2 = Data1',
      OpenStatus = cant_open(Diagnostic) .
      
set_status(Stream, Status) :-

    Stream = [open] :
      Status = true ;

    Stream = [false] :
      Status = false(unexpected_file_error) ;

    Stream = [FileStatus], FileStatus =\= open, FileStatus =\= false :
      Status = FileStatus? ;

  Stream ? _Junk, list(Stream') |
	set_status1(Stream', Status).

  set_status1(Stream, Status) :-

    Stream = [_Data(FileStatus)] :
      Status = FileStatus? ;

    Stream ? _Junk, list(Stream') |
	self.



buffered_io(Operation, FileName, AccessType, DataStream, UID) :-

    AccessType = constants |
	buffered_io1(Operation, FileName, -1, DataStream, UID);

    AccessType = characters(Eol),
    integer(Eol),
    Eol =\= -1 |
	buffered_io1(Operation, FileName, Eol, DataStream, UID);

    AccessType = characters(-1) |
	buffered_io1(Operation, FileName, 255, DataStream, UID);

    otherwise :
      Operation = _, FileName = _, AccessType = _, DataStream = _, UID = _ |
	fail("invalid data access" -
			 received(AccessType) -
				 expected([constants,characters(eol)])
	).


buffered_io1(Operation, FileName, Access, DataStream, UID) :-
	make_absolute(FileName, UID, Operation, FileName'),
	processor # file(Operation(FileName', Access, Data), Ok),
	opened_file(DataStream, Data, Ok).

opened_file(DataStream, Data, Ok) :-

    Ok = true :
      DataStream ! open,
      DataStream' = Data ;

    Ok =\= true : DataStream' = _,
      DataStream ! Ok,
      Data = [] .
