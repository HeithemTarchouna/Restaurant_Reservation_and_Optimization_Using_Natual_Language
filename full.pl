
:- use_module(library(date_time)). % use pack_install(date_time). to install this external library
:- use_module([library(clpfd),library(lists)]).
:- expects_dialect( sicstus ).


% lexical rules
a --> [a].
a --> [].
the --> [the].
the --> [].
of --> [of].
of --> [].
th --> [th].
th --> [nd].
th --> [st].
th --> [].
at --> [at].
at --> [].
month_separator --> ['/'].
month_separator --> ['-'].
pm --> [pm].
pm --> [].
oclock --> [oclock].
oclock --> [].




% logic rules
customer_number(CustomerNumber) --> [CustomerNumber], {integer(CustomerNumber)}.
done --> ['.'].
done --> ['!'].
done --> ['?'].
done --> ['please'].
done --> [].
preferably --> [preferably].
preferably --> [].

people --> [people].
people --> [person].
people --> [of,us,in].
people --> [].
party_of --> [a,party,of].
party_of --> [].

for --> [for].
for --> [of].
for --> [a].
for --> [].


%---------------------------------------------------Helper predicates for time and date-------------------------------%

% time rules : parse time in 24 hour format while checking it's valid

res_time(Hour,Minute) --> [Hour], [':'], [Minute], {time(Hour,Minute)}.
res_time(S_H,S_M) --> [Hour], [':'], [Minute], {time_pm(Hour,Minute,S_H,S_M)}.

res_time(Hour,0) --> [Hour], oclock, {time(Hour,0)}.
res_time(S_H,0) --> [Hour],pm, oclock,pm, {time_pm(Hour,0,S_H,0)}.

res_time(S_H,S_M) --> [Hour], [':'], [Minute],pm, {time_pm(Hour,Minute,S_H,S_M)}.% convert to 24 hour format
res_time(S_H,0) --> [Hour], pm, {time_pm(Hour,0,S_H,0)}.% convert to 24 hour format / assume minute is 0 if not specified
res_time(S_H,S_M) --> []. 

% date rules : parses date while checking it's valid
res_date(Y,MonthNum,D) --> the,[D],month_separator, [M],month_separator,[Y], {valid_date(Y, M, D,MonthNum)}.
res_date(Y,MonthNum,D) --> the,[D],month_separator, [M],{tomorrow_date(Y,_,_),valid_date(Y, M, D,MonthNum)}. % assume it's the year of tommorrow's date if year is not specified 
res_date(Y,MonthNum,D) --> the,[D],th,of,[M],of, [Y],{valid_date(Y, M, D,MonthNum)}.
res_date(Y,MonthNum,D) --> the,[D],th,of,[M],{tomorrow_date(Y,_,_),valid_date(Y, M, D,MonthNum)}. % assume it's the year of tommorrow's date if year is not specified 


res_date(Y,MonthNum,D) --> [M],the,[D],th,of,[Y],{valid_date(Y, M, D,MonthNum)}.
res_date(Y,MonthNum,D) --> [M],the,[D],th,{tomorrow_date(Y,_,_),valid_date(Y, M, D,MonthNum)}.% assume it's the year of tommorrow's date if year is not specified 
res_date(Y,MonthNum,D) --> [],{tomorrow_date(Y,MonthNum,D)}. % assume tomorrow if date is not specified



%---------------------------------------Greetings and booking starters-------------------------------------------%

intro --> [Atom], { member(Atom,[a,at,table]) }.
intro --> [Atom], { \+integer(Atom) }.

%---------------------------------------------------------------------------------------------------------------%
%table_reservation(T,[a,table,for,3,people],[]).
%table_reservation(T,[a,table,for,3],[]).   
%table_reservation(T,[a,table, for, 3],[]).  

reservation_time(Hour,Minute) --> preferably,at, res_time(Hour,Minute).


reservation_date(Year,Month,Day) --> [on], res_date(Year,Month,Day).


table_reservation(Customers_number) --> a,[table],for,party_of, customer_number(Customers_number), people.
table_reservation(Customers_number) --> for,party_of,customer_number(Customers_number), people.


reservation_meal(Meal) --> preferably,for,the,[Meal],{member(Meal,['standard','theater'])},[menu].
reservation_meal(standard) --> [].







%---------------------------------------------------------------------------------------------------------------%
% valid reservations
reservation([Year,Month,Day,Hour,Minute,Meal,Customers_number]) --> intro,reservation([Year,Month,Day,Hour,Minute,Meal,Customers_number]) .
reservation([Year,Month,Day,Hour,Minute,Meal,Customers_number])  --> table_reservation(Customers_number), reservation_time(Hour,Minute),reservation_date(Year,Month,Day),reservation_meal(Meal),done.
reservation([Year,Month,Day,Hour,Minute,Meal,Customers_number])  --> table_reservation(Customers_number), reservation_time(Hour,Minute),reservation_meal(Meal),reservation_date(Year,Month,Day),done.
reservation([Year,Month,Day,Hour,Minute,Meal,Customers_number])  --> table_reservation(Customers_number), reservation_date(Year,Month,Day),reservation_time(Hour,Minute),reservation_meal(Meal),done.


reservation([Year,Month,Day,Hour,Minute,Meal,Customers_number])  --> table_reservation(Customers_number),reservation_meal(Meal), reservation_time(Hour,Minute),reservation_date(Year,Month,Day),done.

reservation([Year,Month,Day,Hour,Minute,Meal,Customers_number])  --> table_reservation(Customers_number),reservation_date(Year,Month,Day),reservation_meal(Meal),reservation_time(Hour,Minute),done. 

reservation([Year,Month,Day,Hour,Minute,Meal,Customers_number])  --> reservation_time(Hour,Minute),table_reservation(Customers_number),reservation_date(Year,Month,Day),reservation_meal(Meal),done.
reservation([Year,Month,Day,Hour,Minute,Meal,Customers_number])  --> a,[table],reservation_date(Year,Month,Day),for,party_of,customer_number(Customers_number), people,at,res_time(Hour,Minute),reservation_meal(Meal),done.


%------------------------------------Helper predicates for time and time processing------------------------------%

% time predicates
time(Hour, Minute) :-
    integer(Hour),
    %Hour >= 19,
    %Hour =< 22,
    integer(Minute),
    Hour >= 12.
    %Minute >= 0,
    %Minute =< 59.

% tests if valid time (when the restaurant is open) and then converts to 24 hour format and stores it in S_H:S_M : S_H is the hour in 24 hour format and S_M is the minute
time_pm(Hour, Minute,S_H,S_M) :-
    integer(Hour),
    %Hour >= 7,
    Hour =< 12,
    integer(Minute),
    %Minute >= 0,
    %Minute =< 59,
    S_H is Hour + 12,
    S_M = Minute.

    
% convert_to_standard_time(Hour, Minute,StandardHour,StandardMinute): converts Hour:Minute to 24 hour format and stores it in StandardHour:StandardMinute
convert_to_standard_time(Hour, Minute, StandardHour, StandardMinute) :-
    integer(Hour),
    between(1, 12, Hour),
    integer(Minute),
    between(0, 59, Minute),
    StandardHour is Hour + 12,
    StandardMinute = Minute,
    between(19, 22, StandardHour).

%---------------------------------------------------------------------------------------------------------------%

% date predicates

% valid_date(Year, Month, Day) :- checks if Year:Month:Day is a valid date and converts it to MonthNum:Day:Year if the month is specified as an atom.
valid_date(Year, Month, Day,MonthNum) :-
    ((number(Month),MonthNum = Month) ; convert_month(MonthNum, Month)),
    number(Year),
    number(Day).





convert_month(1, january).
convert_month(2, february).
convert_month(3, march).
convert_month(4, april).
convert_month(5, may).
convert_month(6, june).
convert_month(7, july).
convert_month(8, august).
convert_month(9, september).
convert_month(10, october).
convert_month(11, november).
convert_month(12, december).


% days_in_month(Year, Month, NumDays)
days_in_month(_, Month, NumDays) :-
    month_length(Month, NumDays).
    


days_in_month(Year, 2, NumDays) :-

    leap_year(Year),
    NumDays = 29.

days_in_month(Year, 2, NumDays) :-
    \+ leap_year(Year),
    NumDays = 28.


leap_year(Year) :-
    Year mod 4 =:= 0.



% month_length(Month, NumDays)
month_length(1, 31).
month_length(3, 31).
month_length(4, 30).
month_length(5, 31).
month_length(6, 30).
month_length(7, 31).
month_length(8, 31).
month_length(9, 30).
month_length(10, 31).
month_length(11, 30).
month_length(12, 31).


tomorrow_date(Y,M,D) :-
    get_time(Timestamp),
    stamp_date_time(Timestamp, _, local),
    NewTimestamp is Timestamp + 86400, % 86400 seconds in a day
    stamp_date_time(NewTimestamp, NewDateTime, local),
    date_time_value(date, NewDateTime, date(Y,M,D)).


today_date(Y,M,D) :-

    get_time(Timestamp),
    stamp_date_time(Timestamp, DateTime, local),
    date_time_value(date, DateTime, date(Y,M,D)).




%---------------------------------------------------------------------------------------------------------------%
% test_dcg


test_dcg(SMS,[Year,Month,Day,Hour, Minute, TimeInMinutes, ExpectedEnd, Meal, Customers_number, TableNumber]) :-
    phrase(reservation([Year,Month,Day,Hour,Minute,Meal,Customers_number]), SMS,[]).
    %write('Hour : '),write(Hour),nl,
    %write('Minute : '),write(Minute),nl,
    %write('Year : '),write(Year),nl,
    %write('Month : '),write(Month),nl,
    %write('Day : '),write(Day),nl,
    %write('Customers_number : '),write(Customers_number),nl,
    %write('Meal : '),write(Meal),nl, !. % the reason for this cut here is because we want to stop the program from backtracking and printing the same result multiple times.(the reason for this behaviour is the reservation predicate is recursive, so it can continue looking for more matches until the intro words are all found.)

%---------------------------------------------------------------------------------------------------------------%
%reservation([Hour,Minute,Year,Month,Day,Customers_number,Meal],[please,can,we,have,a,table,for,3,for,the,theatre,menu,on,march,18,th],[]).

test_dcg_list(SMSList,ResultList) :-
    maplist(test_dcg,SMSList,ResultList).


%---------------------------------------------------------------------------------------------------------------%

 %Constrint system module and  reservation scheduler

% The table/2 predicate defines the number of seats for each table
table(1, 2).
table(2, 3).
table(3, 4).

% The duration/2 predicate defines the duration of each meal type
duration(standard, 60).
duration(theater, 30).

% reservation predicate is going to be used as a datastructure for storing the reservation information : it also contains the constraints for the reservations information.
reservation([Year,Month,Day,StartHour, StartMinute, TimeInMinutes, ExpectedEnd, MealType, NumberOfPeople, TableNumber]) :-
  % reservation opening time is 19:00 and closing time is 23:00
  % 19:00 = 19*60 + 0 = 1140
  % 23:00 = 23*60 + 0 = 1380
  StartHour in 19..23,
  StartMinute in 0..59,
  TimeInMinutes #= StartHour*60 + StartMinute,
  % meal type is either standard or theater
  member(MealType, [standard, theater]),
  % number of people is between 1 and 4 (inclusive)
  NumberOfPeople in 1..4,
  % table number is between 1 and 3 (inclusive)
  TableNumber in 1..3,
  % the number of people must be less than or equal to the number of seats for the table
  table(TableNumber, NumberOfSeats),
  NumberOfPeople #=< NumberOfSeats,
  % the duration of the meal must be less than or equal to the time left before closing time
  duration(MealType, Duration),
  ExpectedEnd #= TimeInMinutes + Duration,
  ExpectedEnd #=< 1380,
  % the reservation must be for today or tomorrow
  Day in 1..31,
  Month in 1..12,
  Year in 2020..2025,
  % this is to make sure that the reservation is not for today (because we are not going to accept reservations for the same day as the reservation must be made at least one day in advance)
  today_date(TodayYear,TodayMonth,TodayDay),
  date(Year,Month,Day) \= date(TodayYear,TodayMonth,TodayDay), 
  % this is to make sure that someone doesn't order a reservation for a day that doesn't exist (29th of February for example for a non-leap year or 31st of April for example)
  days_in_month(Year, Month, NumDays),
  Day #=< NumDays,

  label([StartHour, StartMinute, TimeInMinutes, ExpectedEnd,TableNumber]).


 % this is a datastrucutre for storing the reservations  (list of reservation list)
  reservations(AllReservations):-
    maplist(reservation, AllReservations).
    


% ---------------------------------------------------------------------------------------------------------------%
% scheduler module : this module is going to be used for scheduling the reservations


% The overlap/4 predicate checks if two reservations overlap in time
overlap(Year,Month,Day,Start1, End1,Year,Month,Day, Start2, End2) :-
  Start1 #=< End2, End1 #>= Start2.


% Schedule all reservations that overlap to different tables
schedule(AllReservations) :-
  reservations(AllReservations),
  % For all pairs of reservations, check if they overlap in time
  % and, if they do, ensure that they are assigned to different tables
  forall(combination(2, AllReservations, [Reservation1, Reservation2]),
        ensure_tables(AllReservations,[Reservation1,Reservation2])
         ).


% Ensure that all reservations that overlap in terms of time are not assigned to the same table during that peroid
ensure_tables(AllReservations, [Reservation1, Reservation2]) :-
    Reservation1 = [Year1,Month1,Day1,_, _, TimeInMinutes1, ExpectedEnd1, _, _, TableNumber1],
    Reservation2 = [Year2,Month2,Day2,_, _, TimeInMinutes2, ExpectedEnd2, _, _, TableNumber2],
    overlap(Year1,Month1,Day1,TimeInMinutes1, ExpectedEnd1,Year2,Month2,Day2, TimeInMinutes2, ExpectedEnd2),
    TableNumber1 #\= TableNumber2.
% if two tables don't overlap, then they can be assigned to the same table
ensure_tables(AllReservations, [Reservation1, Reservation2]) :-
    Reservation1 = [Year1,Month1,Day1,_, _, TimeInMinutes1, ExpectedEnd1, _, _, TableNumber1],
    Reservation2 = [Year2,Month2,Day2,_, _, TimeInMinutes2, ExpectedEnd2, _, _, TableNumber2],
      \+overlap(Year1,Month1,Day1,TimeInMinutes1, ExpectedEnd1,Year2,Month2,Day2, TimeInMinutes2, ExpectedEnd2).

combination(0, _, []).
combination(N, [X|Xs], [X|Ys]) :-
  N > 0,
  N1 is N - 1,
  combination(N1, Xs, Ys).
  combination(N, [_|Xs], Ys) :-
  N > 0,
  combination(N, Xs, Ys).

test(Reservations, MaxList) :-
    % https://stackoverflow.com/questions/8231762/swi-prolog-show-long-list 
    set_prolog_flag(answer_write_options,[max_depth(0)]), % disable answer depth limit for printing the whole results
    % actual code
    subseq0(Reservations, MaxList),
    reservations(MaxList),
    schedule(MaxList),
    total_seats(MaxList, TotalSeats),
    findall(Var, member([_,_,_,_, _, _, _, _, _, Var], MaxList), Vars),
    labeling([max(TotalSeats)], Vars).

total_seats(Reservations, TotalSeats) :-
  maplist(number_of_seats, Reservations, Seats),
  sumlist(Seats, TotalSeats).

number_of_seats(Reservation, NumberOfSeats) :-
  Reservation = [_,_,_,_, _, _, _, _, NumberOfSeats, _].

% subseq0(List, Subsets): true if Subsets is a subset of List
% credits to 
%https://stackoverflow.com/questions/4912869/subsets-in-prolog
subseq0(List, List).
subseq0(List, Rest) :-
 subseq1(List, Rest).

subseq1([_|Tail], Rest) :-
 subseq0(Tail, Rest).
subseq1([Head|Tail], [Head|Rest]) :-
 subseq1(Tail, Rest).


% ---------------------------------------------------------------------------------------------------------------%

 %test([[2021,06,17,19,20,Start,End,theater,2,Table2],[2021,06,17,X,Y,Start2,End2,standard,2,Table]],Reservation).


% a predicate to convert the list of reservations to a list of readable reservations that are displayed to the user
readable_reservation([Year,Month,Day,Hour,Minute,StartInMinutes,EndInMinutes,Meal,NumberOfPeople,TableNumber],[date(Year,Month,Day),start_Time(Hour,Minute),end_Time(EndHour,EndMinute),meal(Meal),numberOfCustomers(NumberOfPeople),tableNumber(TableNumber)]) :-
    
    EndHour #= EndInMinutes // 60,
    EndMinute #= EndInMinutes mod 60,
    format(' ~w | between : ~w and ~w | for the ~w menu | on table ~w |for ~w customers  ~n~n',[date(Year,Month,Day) , start_Time(Hour,Minute), end_Time(EndHour,EndMinute),Meal,TableNumber,NumberOfPeople]), nl.
    






% ---------------------------------------------------------------------------------------------------------------%
% test module : this module is going to be used for testing the scheduler module
% combines all code together
% uses the test_dcg_list predicate to convert the list of sms to a list of reservations
% which can then be used by the scheduler module
full_test(SMSList,BestSchedule):-

    test_dcg_list(SMSList,AllReservations),
    % generates all possible combinations of reservations
    test(AllReservations,OptimalReservations),
    length(OptimalReservations, LengthOptimalReservations),
    length(AllReservations, LengthAllReservations),
    


    write('The best schedule is : '),nl,
    write('----------------------------------------------------------------------------------------------------------------------------------'),nl,
    maplist(readable_reservation,OptimalReservations,BestSchedule),
    write('----------------------------------------------------------------------------------------------------------------------------------'),nl.






    % cut to avoid backtracking and printing multiple schedules (all valid schedules but not needed , if you backtrack you get the second best optimal schedule)

%full_test([[please,can,we,have,a,table,for,3,for,the,theater,menu,on,march,18,th],[can,i,book,a,table,at,9,pm,for,2,people,on,the,18,th,of,march,for,the,standard,menu,please]],Reservation). 
%full_test([[please,can,we,have,a,table,for,3,for,the,theater,menu,on,march,18,th],[can,i,book,a,table,at,9,pm,for,2,people,on,the,18,th,of,march,for,the,standard,menu,please]],Reservation). 