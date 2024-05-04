:- module(directional, [horizontal_moves/4, vertical_moves/4, diagonal_moves/4]).

:- use_module('../utils/board_utils').

% horizontal_moves(+Board, +Coord, +Color, -HMoves)
% get all the horizontal moves for the piece at the given coordinates.
horizontal_moves(Board, Coord, Piece, HMoves):-
    % traverse the columns
    both_way_moves(inc_col, dec_col, Board, Coord, Piece, HMoves).

% vertical_moves(+Board, +Coord, +Color, -VMoves)
% get all the vertical moves for the piece at the given coordinates.
vertical_moves(Board, Coord, Piece, VMoves):-
    % traverse the rows
    both_way_moves(inc_row, dec_row, Board, Coord, Piece, VMoves).

% diagonal_moves(+Board, +Coord, +Color, -DMoves)
% get all the diagonal moves for the piece at the given coordinates.
diagonal_moves(Board, Coord, Piece, LU-X):-
    both_way_moves(
        [Old, New]>>(inc_row(Old, Temp), inc_col(Temp, New)),
        [Old, New]>>(dec_row(Old, Temp), dec_col(Temp, New)),
        Board, Coord, Piece, LU-RU),
    both_way_moves(
        [Old, New]>>(inc_row(Old, Temp), dec_col(Temp, New)),
        [Old, New]>>(dec_row(Old, Temp), inc_col(Temp, New)),
        Board, Coord, Piece, RU-X).

% both_way_moves(+Next, +Board, +Coord, +Color, -Moves)
% get all the moves in both directions of OneWayNext for the given coordinates.
both_way_moves(OneWayNext, OtherWayNext, Board, Coord, Piece, OneWay-X):-
    % goes in one way
    propagate(OneWayNext, Board, Piece, Coord, Coord, OneWay-OtherWay),
    % goes in the other way (the opposite of the first way)
    propagate(OtherWayNext, Board, Piece, Coord, Coord, OtherWay-X).


% propagate(+Next, +Board, +Color, +Origin, +CurCoor, -Moves)
% propagates the move in the direction given by Next/2, until it can't no more.
% returns all moves along this axis.

% stop if move is illegal
propagate(_, _, _, _, CurCoor, X-X):- is_illegal_coord(CurCoor), !.
% stop if we hit a piece of the same color
propagate(_, Board, p(Color, _), _, CurCoor, X-X):- get_piece_at(CurCoor, Board, p(Color, _)), !.
% capture piece of the opposite color and stop (this doesn't capture empty pieces, because we ask for p/2 not a p/1)
propagate(_, Board, p(Color, Type), Origin, CurCoor, [move(Type, Origin, CurCoor) | X]-X):-
    other_color(Color, OppositeColor),
    get_piece_at(CurCoor, Board, p(OppositeColor, _)), !.
% the tile is empty, continue propagating
propagate(Next, Board, p(Color, Type), Origin, CurCoor, [move(Type, Origin, CurCoor)| NextMoves]-X):-
    call(Next, CurCoor, NextCoord),
    propagate(Next, Board, p(Color, Type), Origin, NextCoord, NextMoves-X), !.
