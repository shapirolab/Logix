/* $Header: /home/qiana/Repository/Logix/system/Doors/date.cp,v 1.1 1999/07/09 07:03:24 bill Exp $ */

-language(compound).
-mode(interrupt).
-export([today/2, get_date/1, date_to_key/3, key_to_date/3,
	 date_to_system_format/3, format_date/2]
).

get_date(Date) :- today("string_name", Date). 		% compatability


today("string_name", Date) :-		% give to today in : "15 Jan 1992 12:30	
	processor#interface(date(D)),
	format_date(D?, Date).


today(components, Date) :-		%give today in a 6 tuple of integers
	separate(Date, 48).


separate(Comp, ZERO) :-
    	Comp = {Y, M, D, H, Min, Sec},
	processor#interface(date(Date)),
  	string_to_dlist(Date, [Y1, Y2, M1, M2, D1, D2,
                               H1, H2, I1, I2, S1, S2
                              ], []),
	Y := 10 * (Y1 - ZERO) + Y2 - ZERO,
	M := 10 * (M1 - ZERO) + M2 - ZERO,
	D := 10 * (D1 - ZERO) + D2- ZERO,
	H := 10 * (H1 - ZERO) + H2 - ZERO,
	Min := 10 * (I1 - ZERO) + I2- ZERO,
	Sec := 10 * (S1 - ZERO) + S2- ZERO.




format_date(D, Date) :-		% rigid formatting - should be generalized
  string(D), 
  string_to_dlist(D, [Y1, Y2, M1, M2, D1, D2,
                               H1, H2, I1, I2, _S1, _S2
                              ],
                    []
    ) :
    ascii(' ', B),  % blank
    ascii(':', C) |
	month_to_name(M1, M2, N1, N2, N3),
        list_to_string([N1, N2, N3, B, D1, D2, B, H1, H2, C, I1, I2],
			Date);

  otherwise |
	Date = D. 

month_to_name(M1, M2, N1, N2, N3) :-
  M1 =:= ascii("0"), M2 =:= ascii("1") :
    ascii('J', N1), ascii('a', N2), ascii('n', N3);

  M1 =:= ascii("0"), M2 =:= ascii("2") :
    ascii('F', N1), ascii('e', N2), ascii('b', N3);

  M2 =:= ascii("3") : M1 = _,
    ascii('M', N1), ascii('a', N2), ascii('r', N3);

  M2 =:= ascii("4") : M1 = _,
    ascii('A', N1), ascii('p', N2), ascii('r', N3);

  M2 =:= ascii("5") : M1 = _,
    ascii('M', N1), ascii('a', N2), ascii('y', N3);

  M2 =:= ascii("6") : M1 = _,
    ascii('J', N1), ascii('u', N2), ascii('n', N3);

  M2 =:= ascii("7") : M1 = _,
    ascii('J', N1), ascii('u', N2), ascii('l', N3);

  M2 =:= ascii("8") : M1 = _,
    ascii('A', N1), ascii('u', N2), ascii('g', N3);

  M2 =:= ascii("9") : M1 = _,
    ascii('S', N1), ascii('e', N2), ascii('p', N3);

  M2 =:= ascii("0") : M1 = _,
    ascii('O', N1), ascii('c', N2), ascii('t', N3);

  M1 =:= ascii("1"), M2 =:= ascii("1") :
    ascii('N', N1), ascii('o', N2), ascii('v', N3);

  M1 =:= ascii("1"), M2 =:= ascii("2") :
    ascii('D', N1), ascii('e', N2), ascii('c', N3).


  





% transform "12/02/92" to "92/02/12"  (also check date vailidty)

date_to_key(Date, OrderedDate, Ok) :-
	string_to_dlist(Date, List, []),
 	date_to_key1(List?, Components, Ok),
	components_to_string(Components?, OrderedDate).





% transform "12/02/92" to "920212235959" (also check date vailidty)

date_to_system_format(Date, SystemDate, Ok) :-
	string_to_dlist(Date, List, []),
 	date_to_key1(List?, Components, Ok),
	components_to_string2(Components?, SystemDate).


components_to_string2(Components, SystemDate) :-
    Components = {Y1, Y2, M1, M2, D1, D2} |
	list_to_string([Y1, Y2, M1, M2, D1, D2, 50, 51, 53, 57, 53, 57], SystemDate);

    Components = false : SystemDate = false.
    



components_to_string(Components, OrderedDate) + (Slash = 47) :-
    Components = {Y1, Y2, M1, M2, D1, D2} |
	list_to_string([Y1, Y2, Slash, M1, M2, Slash, D1, D2], OrderedDate);

    Components = false : Slash = _,
	OrderedDate = false.


date_to_key1(List, Components, Ok) + (Slash = 47, Zero = 48) :-
		% ascii("/")

    List = [D1, D2,  Slash, M1, M2, Slash, Y1, Y2],
    integer(D1), integer(D2), integer(M1), integer(M2),
    integer(Y1), integer(Y2) : Zero = _,
      Components = {Y1, Y2, M1, M2, D1, D2} |
	two_digits(D1, D2, Day, Ok1),
	two_digits(M1, M2, Month, Ok2),
	two_digits(Y1, Y2, Year, Ok3),
	days_in_month(Day?, Month?, Year?, Ok4),
	and([Ok1?, Ok2?, Ok3?, Ok4?], Ok);

    List = [D2, Slash, M1, M2, Slash, Y1, Y2],
    integer(Zero),
    integer(D2), integer(M1), integer(M2), integer(Y1), integer(Y2) :
      Components = {Y1, Y2, M1, M2, Zero, D2} |
	two_digits(Zero, D2, Day, Ok1),
	two_digits(M1, M2, Month, Ok2),
	two_digits(Y1, Y2, Year, Ok3),
	days_in_month(Day?, Month?, Year?, Ok4),
	and([Ok1?, Ok2?, Ok3?, Ok4?], Ok);
 
    List = [D1, D2,  Slash, M2, Slash, Y1, Y2],
    integer(Zero),
    integer(D1),  integer(D2), integer(M2), integer(Y1), integer(Y2) :
      Components = {Y1, Y2, Zero, M2, D1, D2} |
	two_digits(D1, D2, Day, Ok1),
	two_digits(Zero, M2, Month, Ok2),
	two_digits(Y1, Y2, Year, Ok3),
	days_in_month(Day?, Month?, Year?, Ok4) ,
	and([Ok1?, Ok2?, Ok3?, Ok4?], Ok);

    List = [D2,  Slash, M2, Slash, Y1, Y2],
    integer(Zero),
    integer(D2), integer(M2), integer(Y1), integer(Y2) :
      Components = {Y1, Y2, Zero, M2, Zero, D2} |
	two_digits(Zero, D2, Day, Ok1),
	two_digits(Zero, M2, Month, Ok2),
	two_digits(Y1, Y2, Year, Ok3),
	days_in_month(Day?, Month?, Year?, Ok4) ,
	and([Ok1?, Ok2?, Ok3?, Ok4?], Ok);
 
    otherwise : Slash = _, Zero = _, List = _,
      Components = false, Ok = false.



two_digits(D1, D2, Day, Ok) :-
    C1 := D1 - 48, C2 := D2 - 48,
    C1 < 10, C1 > -1, C2 < 10, C2 > -1,
    Day^ := 10*C1 + C2 :
      Ok = true ;

    otherwise : D1 = _, D2 = _ , Day = 0,
      Ok = false .

 


% transform "92/02/12" to  "12/02/92" (DO NOT check date vailidty)

key_to_date(OrderedDate, Date, Ok) :-
	string_to_dlist(OrderedDate, List, []),
 	key_to_date1(List?, Date, Ok).



key_to_date1(List, Date, Ok) + (Slash = 47) :-		% ascii("/")

    List = [Y1, Y2,  Slash, M1, M2, Slash, D1, D2],
    integer(D1), integer(D2), 
    integer(M1), integer(M2), 
    integer(Y1), integer(Y2) :
      Ok = true |
	list_to_string([D1, D2, Slash, M1, M2, Slash, Y1, Y2], Date);


    otherwise :  Date = _, Slash = _, List = _,
      Ok = false.





days_in_month(Day, Month, Year, Ok) 
	+ (Table = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}) :-
    Month >= 1, Month =< 12,
    arg(Month, Table, I) |
	day_in_month1(I?, Day, Month, Year, Ok);
   
    otherwise : Day = _, Month = _, Year = _, Table = _,
      Ok = false.


day_in_month1(I, Day, Month, Year, Ok) :-
    I >= Day :  Month = _, Year = _,
      Ok = true;

    Month = 2, Day = 29, Mod := Year \ 4, Mod = 0 :  I = _,
      Ok = true;

    otherwise : Day = _, Month = _, Year = _, I = _,
      Ok = false.




