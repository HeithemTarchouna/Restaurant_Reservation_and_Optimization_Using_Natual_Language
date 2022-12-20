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




% The overlap/4 predicate checks if two reservations overlap in time
overlap(Start1, End1, Start2, End2) :-
  Start1 #=< End2, End1 #>= Start2.


% Schedule all reservations that overlap to different tables
schedule(AllReservations) :-
  maplist(reservation, AllReservations),
  % For all pairs of reservations, check if they overlap in time
  % and, if they do, ensure that they are assigned to different tables
  forall(combination(2, AllReservations, [Reservation1, Reservation2]),
        ensure_tables(AllReservations,[Reservation1,Reservation2])
         ).


% Ensure that all reservations that overlap in terms of time are not assigned to the same table during that peroid
ensure_tables(AllReservations, [Reservation1, Reservation2]) :-
    Reservation1 = [_, _, TimeInMinutes1, ExpectedEnd1, _, _, TableNumber1],
    Reservation2 = [_, _, TimeInMinutes2, ExpectedEnd2, _, _, TableNumber2],
    overlap(TimeInMinutes1, ExpectedEnd1, TimeInMinutes2, ExpectedEnd2),
    TableNumber1 #\= TableNumber2.
% if two tables don't overlap, then they can be assigned to the same table
ensure_tables(AllReservations, [Reservation1, Reservation2]) :-
      Reservation1 = [_, _, TimeInMinutes1, ExpectedEnd1, _, _, TableNumber1],
      Reservation2 = [_, _, TimeInMinutes2, ExpectedEnd2, _, _, TableNumber2],
      \+overlap(TimeInMinutes1, ExpectedEnd1, TimeInMinutes2, ExpectedEnd2).

combination(0, _, []).
combination(N, [X|Xs], [X|Ys]) :-
  N > 0,
  N1 is N - 1,
  combination(N1, Xs, Ys).
  combination(N, [_|Xs], Ys) :-
  N > 0,
  combination(N, Xs, Ys).





test(Reservations,MaxList) :-
  getOptimal(Reservations,MaxList),
  schedule(MaxList).
  %labeling(max(Sum),[Sum]). % the cut is used to stop the search after the first optimal is found, if you backtrack you get the second best and so on.



% subseq0(List, Subsets): true if Subsets is a subset of List
%https://stackoverflow.com/questions/4912869/subsets-in-prolog
subseq0(List, List).
subseq0(List, Rest) :-
   subseq1(List, Rest).

subseq1([_|Tail], Rest) :-
   subseq0(Tail, Rest).
subseq1([Head|Tail], [Head|Rest]) :-
   subseq1(Tail, Rest).

% sum(A single reservation of a list of reservations, Sum): Sum is the sum of seats filled in the reservation or reservations
% this first part handles the case of single reservation
sum_seats_filled([], 0).
sum_seats_filled([_, _, _, _, _, NumberOfPeople, _], NumberOfPeople).
sum_seats_filled([[_, _, _, _, _, NumberOfPeople, _]|T], Sum) :-
      sum_seats_filled(T, Sum1),
      Sum #= Sum1 + NumberOfPeople.
      sum_seats_filled([[_, _, _, _, _, NumberOfPeople, _]], NumberOfPeople).
% this second part handles the case of multiple reservations
sum_seats_filled([[[_, _, _, _, _, NumberOfPeople, _]]|T], Sum) :-
      sum_seats_filled(T, Sum1),
      write(Sum1),
    Sum #= Sum1 + NumberOfPeople.

% getOptimal(Reservations, MaxList): true if MaxList is the list of reservations that maximizes the number of seats filled
getOptimal(Reservations,MaxList):-
      %subseq0(Reservations, Subsets),
      find_max_sum(Reservations, MaxList).

% Find the maximum sum in a list of lists
find_max_sum(Lists, MaxList) :-
    %create a list of sums of seats filled in each list (possible subset of reservations)
    findall(Subsets, subseq0(Lists,Subsets), AllSublists),
    delete(AllSublists, [], Sublists),
    maplist(sum_seats_filled, Sublists, Sums),
    bubble_sort(Sums, SortedSums),
    %Gives you back the maximum sum in the list , if you backtrack it gives the seconds largest sum etc
    nth1(Index, Sums, MaxSum),
    %find the list of reservations that corresponds to that sum
    nth1(Index, Sublists, MaxList).


%http://kti.mff.cuni.cz/~bartak/prolog/sorting.html
bubble_sort(List,Sorted):-b_sort(List,[],Sorted).
b_sort([],Acc,Acc).
b_sort([H|T],Acc,Sorted):-bubble(H,T,NT,Max),b_sort(NT,[Max|Acc],Sorted).
   
bubble(X,[],[],X).
bubble(X,[Y|T],[Y|NT],Max):-X<Y,bubble(X,T,NT,Max).
bubble(X,[Y|T],[X|NT],Max):-X>=Y,bubble(Y,T,NT,Max).
