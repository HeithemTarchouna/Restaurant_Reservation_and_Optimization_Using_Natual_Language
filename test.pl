
:- use_module(library(date_time)). % use pack_install(date_time). to install this external library
:- use_module(library(clpfd)).

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
for --> [].


%---------------------------------------------------Helper predicates for time and date-------------------------------%

% time rules : parse time in 24 hour format while checking it's valid

res_time(Hour,Minute) --> [Hour], [':'], [Minute], {time(Hour,Minute)}.
res_time(Hour,0) --> [Hour], oclock, {time(Hour,0)}.

res_time(S_H,S_M) --> [Hour], [':'], [Minute],pm, {time_pm(Hour,Minute,S_H,S_M)}.% convert to 24 hour format
res_time(S_H,0) --> [Hour], pm, {time_pm(Hour,0,S_H,0)}.% convert to 24 hour format / assume minute is 0 if not specified

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


reservation_time(Hour,Minute) --> preferably,at, res_time(Hour,Minute).


reservation_date(Year,Month,Day) --> [on], res_date(Year,Month,Day).


table_reservation(Customers_number) --> a,[table],for,party_of, customer_number(Customers_number), people.
table_reservation(Customers_number) --> for,party_of,customer_number(Customers_number), people.


reservation_meal(Meal) --> preferably,for,the,[Meal],{member(Meal,['standard','theatre'])},[menu].
reservation_meal(standard) --> [].







%---------------------------------------------------------------------------------------------------------------%
% valid reservations
reservation([Hour,Minute,Year,Month,Day,Customers_number,Meal]) --> intro,reservation([Hour,Minute,Year,Month,Day,Customers_number,Meal]) .
reservation([Hour,Minute,Year,Month,Day,Customers_number,Meal])  --> table_reservation(Customers_number), reservation_time(Hour,Minute),reservation_date(Year,Month,Day),reservation_meal(Meal),done.
reservation([Hour,Minute,Year,Month,Day,Customers_number,Meal])  --> table_reservation(Customers_number), reservation_time(Hour,Minute),reservation_meal(Meal),reservation_date(Year,Month,Day),done.
reservation([Hour,Minute,Year,Month,Day,Customers_number,Meal])  --> table_reservation(Customers_number), reservation_date(Year,Month,Day),reservation_time(Hour,Minute),reservation_meal(Meal),done.


reservation([Hour,Minute,Year,Month,Day,Customers_number,Meal])  --> table_reservation(Customers_number),reservation_meal(Meal), reservation_time(Hour,Minute),reservation_date(Year,Month,Day),done.

reservation([Hour,Minute,Year,Month,Day,Customers_number,Meal])  --> table_reservation(Customers_number),reservation_date(Year,Month,Day),reservation_meal(Meal),reservation_time(Hour,Minute),done. 



reservation([Hour,Minute,Year,Month,Day,Customers_number,Meal])  --> reservation_time(Hour,Minute),table_reservation(Customers_number),reservation_date(Year,Month,Day),reservation_meal(Meal),done.

%------------------------------------Helper predicates for time and time processing------------------------------%

% time predicates
time(Hour, Minute) :-
    integer(Hour),
    Hour >= 19,
    Hour =< 22,
    integer(Minute),
    Minute >= 0,
    Minute =< 59.

% tests if valid time (when the restaurant is open) and then converts to 24 hour format and stores it in S_H:S_M : S_H is the hour in 24 hour format and S_M is the minute
time_pm(Hour, Minute,S_H,S_M) :-
    integer(Hour),
    Hour >= 7,
    Hour =< 10,
    integer(Minute),
    Minute >= 0,
    Minute =< 59,
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
    number(Day),
    Day >= 0,
    MonthNum >= 1,
    MonthNum =< 12,
    Year >= 1990,
    Year =< 2025,
    days_in_month(Year, MonthNum, NumDays),
    Day =< NumDays.




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
test_dcg(SMS) :-
    phrase(reservation([Hour,Minute,Year,Month,Day,Customers_number,Meal]), SMS,[]),
    write('Hour : '),write(Hour),nl,
    write('Minute : '),write(Minute),nl,
    write('Year : '),write(Year),nl,
    write('Month : '),write(Month),nl,
    write('Day : '),write(Day),nl,
    write('Customers_number : '),write(Customers_number),nl,
    write('Meal : '),write(Meal),nl, !. % the reason for this cut here is because we want to stop the program from backtracking and printing the same result multiple times.(the reason for this behaviour is the reservation predicate is recursive, so it can continue looking for more matches until the intro words are all found.)

%---------------------------------------------------------------------------------------------------------------%
restaurant_representation(Table1,Table2,Table3):-
    length(Table1,2),
    length(Table2,3),
    length(Table3,4).
%---------------------------------------------------------------------------------------------------------------%

% meal_duration(Meal,Duration) :- stores the duration of each meal in hours
meal_duration(standard,2).
meal_duration(theatre,1).

% restaurant_working_hours(Open,Close) :- stores the opening and closing hours of the restaurant
restaurant_working_hours(time(19,00),time(23,00)).

% converts a time to minutes
convert_to_minutes(Hour,Minute,Minutes) :-
    Minutes #= Hour*60 + Minute.

%---------------------------------------------------------------------------------------------------------------%
% schedule repressentation
% dictionary {}, where the key is the date
% and the value is the schedule for that date
% the schedule is a set, where each elemt a list of 3 elements (the 3 tables)
% each table is represented as a list of 240 elements
% table(1,List):-
%     length(List,240).
% table(2,List):-
%     length(List,240).
% table(3,List):-
%     length(List,240).
% max_table_size(1,2).
% max_table_size(2,3).
% max_table_size(3,4).

% after that we want to implement a constraint system that maximizes the number of reservations per day when receiving similar requests:
% [table,for,2,at,20,':',00,on,18,march]
% [can,i,book,a,table,at,9,pm,for,2,people,on,the,18,th,of,march,for,the,standard,menu,please]
