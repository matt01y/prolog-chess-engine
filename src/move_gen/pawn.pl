:- module(pawn, [pawn_move_wrapper/4]).

:- use_module('../board/board_utils').
:- use_module('../utils').

% pawn_move(+Board, +Color, move(+Coord, -NewCoord))
% get a plausible move for the given pawn at the given coordinates.
% doesn't check if the move is legal. or even existent.

pawn_move_wrapper(Board, Color, move(Coord, NewCoord), MoveType):-
    write("pawn_move_wrapper\n"),
    pawn_move(Board, Color, move(Coord, NewCoord)),
    pawn_move_type(Color, NewCoord, MoveType).
% TODO: en passent
pawn_move_wrapper(Board, Color, move(Coord, NewCoord), en_passent):-
    en_passent(move(Coord, NewCoord)).

% differentiates between a normal pawn move and a promotion move.
pawn_move_type(black, _/1, promotion(Piece)):- promotion(Piece).
pawn_move_type(white, _/8, promotion(Piece)):- promotion(Piece).
pawn_move_type(_, _, pawn).

promotion(knight).
promotion(bishop).
promotion(rook).
promotion(queen).

% WHITE PAWN
% pawn progression
pawn_move(_, white, move(Coord, NewCoord)):-
    inc_row(Coord, NewCoord).
% white pawn can move 2 squares if it's still on the 2th rank (hasn't moved yet)
pawn_move(Board, white, move(2/C, 4/C)):- get_piece_at(3/C, Board, p(empty)).
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
% black pawn can move 2 squares if it's still on the 7nd rank (hasn't moved yet)
pawn_move(Board, black, move(7/C, 5/C)):- get_piece_at(6/C, Board, p(empty)).
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

% en_passent(move(+Coord, -NewCoord))
% checks if the move is an en passent move.
en_passent(move(R/C, Nr/PrevC)):-
    % inexplicitly check if the last move was a pawn move
    % a double pawn move is never attacking
    get_last_move(m(pawn, PrevR/PrevC, R/PrevC, none)),
    % check if the last move was a double pawn move
    abs(PrevR - R) =:= 2,
    abs(C - PrevC) =:= 1, % pawn moves 1 rank (up/down regarding color)
    Nr is (PrevR + R) / 2. % Where the pawn would be if it moved only one square
