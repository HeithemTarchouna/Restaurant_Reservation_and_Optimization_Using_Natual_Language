module( stacking, [store_boxes/1, constrain_boxes/3, link_boxes/1, box/2] ).
:- use_module([library(clpfd),library(lists)]).
:- expects_dialect( sicstus ).
% The table/2 predicate defines the number of seats for each table
table(1, 2).
table(2, 3).
table(3, 4).

% The duration/2 predicate defines the duration of each meal type
duration(standard, 120).
duration(theater, 60).

regions(Starts, Ends, Tables, Reservations) :-
      % Constrain the length of each list to be the same
      length(Starts, Len),
      length(Ends, Len),
      length(Tables, Len),
      length(Reservations, Len),
      % Apply the diff/4 predicate to each element in the Lists
      maplist(diff, Starts, Ends, Tables, Reservations),
      % Constrain the start and end times of each reservation to be within the allowed range
      Starts ins  1140.. 1380,
      Ends ins 1140 .. 1380,
      % Check that there are no overlaps between reservations
      no_overlap(Starts, Ends, Tables).



% The diff/4 predicate calculates the start and end times of a reservation based on the reservation's hour and minute, and the meal type's duration
diff(Start, End, Table, [ReservationHour, ReservationMinute, MealType, NumSeats]) :-
      % Look up the duration of the meal type
      duration(MealType, Duration),
      % Calculate the start time of the reservation
      Start #= ReservationHour * 60 + ReservationMinute,
      ReservationHour #= Start // 60,
      ReservationMinute #= Start mod 60,
      % Calculate the end time of the reservation
      End #= Start + Duration,
      % Look up the number of seats at the table
      table(Table, TableCapacity),
      % Constrain the number of seats required to be less than or equal to the table capacity
      NumSeats #=< TableCapacity.

% The no_overlap/3 predicate checks that there are no overlaps between reservations
no_overlap([], [], []).
no_overlap([Start1|Starts], [End1|Ends], [Table1|Tables]) :-
      no_overlap(Starts, Ends, Tables),
      maplist(different_reservation(Start1, End1, Table1), Starts, Ends, Tables).

% The different_reservation/5 predicate checks that two reservations do not overlap if they are assigned to the same table,
% but they can overlap if they are assigned to different tables
different_reservation(Start1, End1, Table1, Start2, End2, Table2) :-
  (Table1 #= Table2, max(Start1, Start2) #> min(End1, End2)).

different_reservation(Start1, End1, Table1, Start2, End2, Table2) :-
  (Table1 #\= Table2).



test(NewStart,NewEnds,Tables,Reservations) :-
        regions(Starts,Ends,Tables, Reservations),
        label(Starts),
        label(Ends),
        label(Tables),
        maplist(timerConverter,Starts,NewStart),
        maplist(timerConverter,Ends,NewEnds).
  
  
timerConverter(X,Time) :-
        Hour #= X // 60,
        Minute #= X mod 60,
        Time = time(Hour,Minute).



/*- Can you update my code so that it takes a list of reservations, then tries to fit as many of them as possible into the schedule .
- it's not necessary that all reservation are placed into the schedule (as the restaurant has only so many tables and seats)
- the scheduler will selects the best reservations and fit them.
- The goal is to always make sure the number of seats filled in the restaurant is maximised.*/
