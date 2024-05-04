:- module(pawn, [pawn_move/3]).

:- use_module('../utils/board_utils').
:- use_module('../utils/utils').

% pawn_move(+Board, +Color, move(+Coord, -NewCoord))
% get a plausible move for the given pawn at the given coordinates.
% doesn't check if the move is legal. or even existent.

% WHITE PAWN
% pawn progression
pawn_move(_, white, move(Coord, NewCoord)):-
    inc_row(Coord, NewCoord).
% white pawn can move 2 squares if it's still on the 7th rank (hasn't moved yet)
pawn_move(_, white, move(2/C, 4/C)).
% capture left
pawn_move(Board, white, move(Coord, NewCoord)):-
    inc_row(Coord, Temp),
    dec_col(Temp, NewCoord),
    get_piece_at(NewCoord, Board, p(black, _)).
% capture right
pawn_move(Board, white, move(Coord, NewCoord)):-
    inc_row(Coord, Temp),
    inc_col(Temp, NewCoord),
    get_piece_at(NewCoord, Board, p(black, _)).

% BLACK PAWN
% pawn progression
pawn_move(_, black, move(Coord, NewCoord)):-
    dec_row(Coord, NewCoord).
% black pawn can move 2 squares if it's still on the 2nd rank (hasn't moved yet)
pawn_move(_, black, move(7/C, 5/C)).
% capture left
pawn_move(Board, black, move(Coord, NewCoord)):-
    dec_row(Coord, Temp),
    dec_col(Temp, NewCoord),
    get_piece_at(NewCoord, Board, p(white, _)).
% capture right
pawn_move(Board, black, move(Coord, NewCoord)):-
    dec_row(Coord, Temp),
    inc_col(Temp, NewCoord),
    get_piece_at(Coord, Board, p(white, _)).