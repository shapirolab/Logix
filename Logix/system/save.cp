/*

William Silverman: 09-07-85

Last update by		$Author: bill $
		       	$Date: 1999/07/09 07:02:54 $
Currently locked by 	$Locker:  $
			$Revision: 1.1 $
			$Source: /home/qiana/Repository/Logix/system/save.cp,v $

Copyright (C) 1985, Weizmann Institute of Science - Rehovot, ISRAEL

*/

-mode(trust).
-export([save/3, quiet/2]).
-language(compound).


procedure save(String, String, done).
procedure quiet(String, done).


save(FileName, Announcement, Ready) :-
    true : Ready = Done? |
	processor # interface(date_time(_, Date, Time)),
	save_announcement(FileName, Announcement, Date, Time, Done).


save_announcement(FileName, Announcement, Date, Time, Done) :-
    known(Date),
    known(Announcement) |
	processor # machine(idle_queue(Idle, 10)),
	annunciate(Idle, FileName, Announcement, Date, Time, Done).
	

annunciate(Idle, FileName, Announcement, SaveDate, SaveTime, Done) :-
    known(Idle) |
	computation # display(term, 
		['
Weizmann Institute Logix 2.2 ', SaveDate - SaveTime, '
Copyright (C) 1991, Weizmann Institute of Science - Rehovot, ISRAEL
', Announcement, '
'		],
		[known(Saved), list, close(done, Announced)]
	),
	prepare_save(FileName, Done, Saved),
	restart_date_time(Saved, Date, Time),
	computation # display(term, Date-Time,
			      [known(Announced), known(Time),
			       close(Saved, Done)]
		 ).

restart_date_time(Saved, Date, Time) :-
    known(Saved) |
	processor # interface(date_time(_, Date, Time)).


quiet(FileName, Ready) :-
    string(FileName) : Ready = Done? |
	prepare_save(FileName, Done, Done).


prepare_save(FileName, Done, Saved) :-
	processor # machine(idle_queue(Idle, 10)),
	save_system(Idle, FileName, Answer),
	processor # machine(boot_wait(Booted)),
	check_answer(Answer, Booted, FileName, Saved),
	start_saved_system(Done, Booted).

check_answer(Answer, Booted, FileName, Saved) :-

    Answer = done : Booted = _, FileName = _,
      Saved = done ;

    Answer =\= done : Booted = _,
      Saved = done |
	fail(save(FileName), Answer);

    known(Booted) : Answer = _, FileName = _,
      Saved = done .

save_system(Idle1, FileName, Answer) :-
    known(Idle1) |
	processor # machine(request(save_state(FileName), Answer)).


start_saved_system(Done, Restarted) :-

    known(Done),
    known(Restarted) |
	processor # device(restart),
	super # '_close'(save, _);

    known(Done),
    unknown(Restarted) |
	super # '_close'(save, _).
