:- use_module(library(clpfd)). % for the #= operator


% subsets(List, Subsets): true if Subsets is a list of all subsets of List
subsets([],[]).
subsets([H|T],[H|NT]) :- subsets(T,NT).
subsets([_|T],NT) :- subsets(T,NT).

% sum(A single reservation of a list of reservations, Sum): Sum is the sum of seats filled in the reservation or reservations
% this first part handles the case of single reservation
sum_seats_filled([_, _, _, _, _, NumberOfPeople, _], NumberOfPeople).
sum_seats_filled([[_, _, _, _, _, NumberOfPeople, _]|T], Sum) :-
      sum_seats_filled(T, Sum1),
      Sum #= Sum1 + NumberOfPeople.
      sum_seats_filled([[_, _, _, _, _, NumberOfPeople, _]], NumberOfPeople).
% this second part handles the case of multiple reservations
sum_seats_filled([[[_, _, _, _, _, NumberOfPeople, _]]|T], Sum) :-
      sum_seats_filled(T, Sum1),
    Sum #= Sum1 + NumberOfPeople.

% getOptimal(Reservations, MaxList): true if MaxList is the list of reservations that maximizes the number of seats filled
getOptimal(Reservations,MaxList):-
      subsets(Reservations, Subsets),
      find_max_sum(Subsets, MaxList).

% Find the maximum sum in a list of lists
find_max_sum(Lists, MaxList) :-
    %create a list of sums of seats filled in each list (possible subset of reservations)
    maplist(sum_seats_filled, Lists, Sums),
    %find the maximum sum in the list of sums
    max_list(Sums, MaxSum),
    %find the index of the maximum sum in the list of sums
    nth1(Index, Sums, MaxSum),
    %find the list of reservations that corresponds to the maximum sum
    nth1(Index, Lists, MaxList).

