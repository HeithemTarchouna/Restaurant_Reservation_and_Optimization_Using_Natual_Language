/*bmsort( X, Y ) :- perm( X, Y ),
    format('Try ~w\n',[Y]),
    ord( Y ).


ord( [] ).
ord( [_] ).

ord( [H1,H2|T] ) :-
    H1 =< H2,
    ord( [H2|T] ).


perm( [], [] ).
perm( [[X]|Y], [U|V] ) :- 
    del( U, [X|Y], W ),
    perm( W, V ).

del( X, [X|Y], Y ).
del( X, [Y|U], [Y|V] ) :- 
    del( X, U, V ).*/



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sudoku with Constraint Logic Programming
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(library(clpfd)).
% Represent a sudoku grid as a matrix, i.e. list of lists.
problem(1, [[1,_,_,8,_,4,_,_,_],
            [_,2,_,_,_,_,4,5,6],
            [_,_,3,2,_,5,_,_,_],
            [_,_,_,4,_,_,8,_,5],
            [7,8,9,_,5,_,_,_,_],
            [_,_,_,_,_,6,2,_,3],
            [8,_,1,_,_,_,7,_,_],
            [_,_,_,1,2,3,_,8,_],
            [2,_,5,_,_,_,_,_,9]]).
problem(2, [[_,_,2,_,3,_,1,_,_],
            [_,4,_,_,_,_,_,3,_],
            [1,_,5,_,_,_,_,8,2],
            [_,_,_,2,_,_,6,5,_],
            [9,_,_,_,8,7,_,_,3],
            [_,_,_,_,4,_,_,_,_],
            [8,_,_,_,7,_,_,_,4],
            [_,9,3,1,_,_,_,6,_],
            [_,_,7,_,6,_,5,_,_]]).
problem(3, [[1,_,_,_,_,_,_,_,_],
            [_,_,2,7,4,_,_,_,_],
            [_,_,_,5,_,_,_,_,4],
            [_,3,_,_,_,_,_,_,_],
            [7,5,_,_,_,_,_,_,_],
            [_,_,_,_,_,9,6,_,_],
            [_,4,_,_,_,6,_,_,_],
            [_,_,_,_,_,_,_,7,1],
            [_,_,_,_,_,1,_,3,_]]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Constraining the sudoku
% Apply sudoku constraints to Grid.
sudoku(Grid) :-
  valid_grid(Grid),
  constrain_cells(Grid),
  constrain_rows(Grid),
  constrain_columns(Grid),
  constrain_blocks(Grid).
% Assume that Grid is 9x9
valid_grid(Grid) :-
  length(Grid, 9),                    % There are 9 rows
  maplist(same_length(Grid), Grid).   % and 9 columns
% All cells of Grid must be any of 1 through 9
constrain_cells(Grid) :-
  append(Grid, AllCells),             % Flatten the matrix to a list
  AllCells ins 1..9.                  
% All cells in a row of Grid must be different
constrain_rows(Grid) :-
  maplist(all_distinct, Grid).        
% All cells in a column of Grid must be different
constrain_columns(Grid) :-
  transpose(Grid, Columns),           % Get the columns
  maplist(all_distinct, Columns).
% All cells in a block of Grid must be different
constrain_blocks([As, Bs, Cs, Ds, Es, Fs, Gs, Hs, Is]) :-
  constrain_block(As, Bs, Cs),        
  constrain_block(Ds, Es, Fs),        % Split rows into blocks
  constrain_block(Gs, Hs, Is).
constrain_block([], [], []).
constrain_block([N1, N2, N3|Ns1],     
                [N4, N5, N6|Ns2],     % Get the first block
                [N7, N8, N9|Ns3]) :-           
  all_distinct([N1, N2, N3,           
                N4, N5, N6,
                N7, N8, N9]),
  constrain_block(Ns1, Ns2, Ns3).     % Do the rest
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Run and display the sudoku
do_sudoku(Problem) :-
  problem(Problem, Grid),
  display_grid(before, Grid),
  sudoku(Grid),                       % Apply the constraints
  maplist(labeling([ff]), Grid),      % Labeling removes all residues
  display_grid(after, Grid).
% Print grid
display_grid(Title, Grid) :-
  format('~k', Title),
  append(Grid, AllCells),
  format('
          \r\t~k ~k ~k ~k ~k ~k ~k ~k ~k
          \r\t~k ~k ~k ~k ~k ~k ~k ~k ~k
          \r\t~k ~k ~k ~k ~k ~k ~k ~k ~k
          \r\t~k ~k ~k ~k ~k ~k ~k ~k ~k
          \r\t~k ~k ~k ~k ~k ~k ~k ~k ~k
          \r\t~k ~k ~k ~k ~k ~k ~k ~k ~k
          \r\t~k ~k ~k ~k ~k ~k ~k ~k ~k
          \r\t~k ~k ~k ~k ~k ~k ~k ~k ~k
          \r\t~k ~k ~k ~k ~k ~k ~k ~k ~k~n',
    AllCells).


%test(X) :- X in 1 .. 10 ,indomain(X).




:- use_module(library(clpfd)).

% we need to allow a carrying slot
add( A, B, C ) :-
    add( A, B, C, 0 ).
    


% add numbers rep'd as lists of digits
add( [], [], [], 0 ).
add( [A|As], [B|Bs], [C|Cs], Carry ) :-
    A in 0..9,
    B in 0..9,
    C #= ( A + B + NextCarry ) mod 10,
    Carry #= div( A + B + NextCarry, 10 ),
    add( As, Bs, Cs, NextCarry ).


test(S,E,N,D,M,O,R,Y) :-
    add( [0,S,E,N,D], [0,M,O,R,E], Sum ),
    Sum = [M,O,N,E,Y],
    all_different( [S,E,N,D,M,O,R,Y] ),
    labeling( [], [S,E,N,D,M,O,R,Y] ).