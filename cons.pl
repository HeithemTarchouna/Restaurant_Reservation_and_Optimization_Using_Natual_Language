module( stacking, [store_boxes/1, constrain_boxes/3, link_boxes/1, box/2] ).
:- use_module([library(clpfd),library(lists)]).
:- expects_dialect( sicstus ).

% The table/2 predicate defines the number of seats for each table
table(1, 2).
table(2, 3).
table(3, 4).

% The duration/2 predicate defines the duration of each meal type
duration(standard, 60).
duration(theater, 30).

% reservation predicate
reservation([StartHour, StartMinute, TimeInMinutes, ExpectedEnd, MealType, NumberOfPeople, TableNumber]) :-
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
  label([StartHour, StartMinute, TimeInMinutes, ExpectedEnd,TableNumber]).


no_intersections(S1, E1, S2, E2) :-
  S1 #< S2,
  E1 #< E2,
  S1 #< E1,
  S2 #< E2.

no_intersections(S1, E1, S2, E2) :-
  S1 #> S2,E1 #> E2,
  S1 #< E1,
  S2 #< E2.

% Schedule all reservations that overlap to different tables
schedule(AllReservations) :-
  maplist(reservation, AllReservations),
  % For all pairs of reservations, check if they overlap in time
  % and, if they do, ensure that they are assigned to different tables
  forall(combination(2, AllReservations, [Reservation1, Reservation2]),
         ( Reservation1 = [_, _, TimeInMinutes1, ExpectedEnd1, _, _, TableNumber1],
           Reservation2 = [_, _, TimeInMinutes2, ExpectedEnd2, _, _, TableNumber2],
           ( TimeInMinutes1 #=< ExpectedEnd2, ExpectedEnd1 #>= TimeInMinutes2
             -> TableNumber1 #\= TableNumber2
             ;  true
           )
         )).


         combination(0, _, []).
        combination(N, [X|Xs], [X|Ys]) :-
          N > 0,
          N1 is N - 1,
          combination(N1, Xs, Ys).
        combination(N, [_|Xs], Ys) :-
          N > 0,
          combination(N, Xs, Ys).

test(Reservations) :-
  schedule(Reservations).

test(Reservations) :-
  schedule(Reservations).
/*update my code to ensure that all reservations that overlap in terms of time are not assigned to the same table during that peroid*/