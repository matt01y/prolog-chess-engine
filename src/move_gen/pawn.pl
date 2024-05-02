:- module(pawn, [pawn_moves/4]).

:- use_module('../utils/board_utils').
:- use_module('../utils/utils').

% pawn_moves(+Board, +Coord, +Color, -Pawn_moves)
% get all the moves for the given pawn at the given coordinates.
pawn_moves(Board, Coord, Color, Pawn_moves):-
    findall(
        Move,
        ( 
            pawn_move(Board, Coord, Color, move(Coord, NewCoord)),
            % stop if move is illegal
            not(is_illegal_coord(NewCoord)),
            % can't move to a square with a piece of the same color
            not(get_piece_at(NR/C, Board, p(Color, _))),
            % Move needs to be a difference list so that we can merge em into 1
            Move is [move(Coord, NewCoord)| X]-X 
        ),
        Moves
    ),
    flatten_DLs_to_one_DL(Moves, Pawn_moves).

% pawn_move(+Board, +Coord, +Color, -Move)
% get a plausible move for the given pawn at the given coordinates.
% doesn't check if the move is legal. or even existent.

% WHITE PAWN
% pawn progression
pawn_move(_, Coord, white, move(Coord, NewCoord)):-
    inc_row(NewCoord, Coord).
% white pawn can move 2 squares if it's still on the 7th rank (hasn't moved yet)
pawn_move(_, 7/C, white, move(7/C, 5/C)).
% capture left
pawn_move(Board, Coord, white, move(Coord, NewCoord)):-
    inc_row(Temp, Coord),
    inc_col(NewCoord, Temp),
    get_piece_at(NewCoord, Board, p(black, _)).
% capture right
pawn_move(Board, Coord, white, move(Coord, NewCoord)):-
    inc_row(Temp, Coord),
    inc_col(Temp, NewCoord),
    get_piece_at(NR/NC, Board, p(black, _)).

% BLACK PAWN
% pawn progression
pawn_move(_, Coord, black, move(Coord, NewCoord)):-
    inc_row(Coord, NewCoord).
% black pawn can move 2 squares if it's still on the 2nd rank (hasn't moved yet)
pawn_move(_, 2/C, black, move(2/C, 4/C)).
% capture left
pawn_move(Board, Coord, black, move(Coord, NewCoord)):-
    inc_row(Coord, Temp),
    inc_col(NewCoord, Temp),
    get_piece_at(NewCoord, Board, p(white, _)).
% capture right
pawn_move(Board, Coord, black, move(Coord, NewCoord)):-
    inc_row(Coord, Temp),
    inc_col(Temp, NewCoord),
    get_piece_at(Coord, Board, p(white, _)).