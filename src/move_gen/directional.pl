:- module(directional, [horizontal_moves/4, vertical_moves/4, diagonal_moves/4]).

:- use_module('../utils/board_utils').

% horizontal_moves(+Board, +Coord, +Color, -HMoves)
% get all the horizontal moves for the piece at the given coordinates.
horizontal_moves(Board, Coord, Color, HMoves):-
    % traverse the columns
    both_way_moves(inc_col, Board, Coord, Color, HMoves).

% vertical_moves(+Board, +Coord, +Color, -VMoves)
% get all the vertical moves for the piece at the given coordinates.
vertical_moves(Board, Coord, Color, VMoves):-
    % traverse the rows
    both_way_moves(inc_row, Board, Coord, Color, VMoves).

% diagonal_moves(+Board, +Coord, +Color, -DMoves)
% get all the diagonal moves for the piece at the given coordinates.
diagonal_moves(Board, Coord, Color, LU-X):-
    both_way_moves([Old, New]>>(inc_row(Old, Temp), inc_col(Temp, New)), Board, Coord, Color, LU-RU),
    both_way_moves([Old, New]>>(inc_row(Old, Temp), inc_col(New, Temp)), Board, Coord, Color, RU-X).

% both_way_moves(+Next, +Board, +Coord, +Color, -Moves)
% get all the moves in both directions of OneWayNext for the given coordinates.
both_way_moves(OneWayNext, Board, Coord, Color, OneWay-X):-
    % goes in one way
    propagate(OneWayNext, Board, Color, Coord, Coord, OneWay-OtherWay),
    % goes in the other way (the opposite of the first way)
    propagate([Old, New]>>(call(OneWayNext, New, Old)), Board, Color, Coord, Coord, OtherWay-X).


% propagate(+Next, +Board, +Color, +Origin, +CurCoor, -Moves)
% propagates the move in the direction given by Next/2, until it can't no more.
% returns all moves along this axis.

% stop if move is illegal
propagate(_, _, _, _, CurCoor, X-X):- is_illegal_coord(CurCoor), !.
% stop if we hit a piece of the same color
propagate(_, Board, Color, _, CurCoor, X-X):- get_piece_at(CurCoor, Board, p(Color, _)), !.
% capture piece of the opposite color and stop (this doesn't capture empty pieces, because we ask for p/2 not a p/1)
propagate(Next, Board, Color, Origin, CurCoor, [move(Origin, CurCoor) | X]-X):- get_piece_at(CurCoor, Board, p(_, _)), !.
% the tile is empty, continue propagating
propagate(Next, Board, Color, Origin, CurCoor, [move(Origin, CurCoor)| Next]-X):-
    call(Next, CurCoor, NextCoord),
    propagate(Next, Board, Color, Origin, NextCoord, Next-X), !.
