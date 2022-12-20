:- use_module(library(clpfd)). % for the #= operator


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