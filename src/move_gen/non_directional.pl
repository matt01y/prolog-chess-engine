:- module(non_directional, [
    king_moves/4,
    knight_moves/4,
    pawn_moves/4
    ]).

:- use_module('../utils/board_utils').
:- use_module('../utils/utils').
:- use_module('../move_gen/pawn').

% king_moves(+Board, +Coord, +Color, -King_moves)
% get all the moves for the king at the given coordinates.
king_moves(Board, Coord, Color, King_moves):-
    all_moves_from(Board, Coord, Color, King_moves, king_options).

% king_options(_, _, move(+Coord, -NewCoord))
% get a plausible move for the given king at the given coordinates.
king_options(_, _, move(R/C, NR/NC)):-
    member(DeltaX, [-1, 0, 1]),
    member(DeltaY, [-1, 0, 1]),
    NR is R + DeltaX,
    NC is C + DeltaY.

% knight_moves(+Board, +Coord, +Color, -Knight_moves)
% get all the moves for the knight at the given coordinates.
knight_moves(Board, Coord, Color, Knight_moves):-
    all_moves_from(Board, Coord, Color, Knight_moves, knight_options).

% knight_options(_, _, move(+Coord, -NewCoord))
% get a plausible move for the given knight at the given coordinates.
knight_options(_, _, move(R/C, NR/NC)):-
    member(DeltaX, [-2, -1, 1, 2]),
    member(DeltaY, [-2, -1, 1, 2]),
    abs(DeltaX, AbsDeltaX),
    abs(DeltaY, AbsDeltaY),
    % this check makes sure a knight move is always in the L shape
    AbsDeltaX + AbsDeltaY =:= 3,
    NR is R + DeltaX,
    NC is C + DeltaY.

% pawn_moves(+Board, +Coord, +Color, -Pawn_moves)
% get all the moves for the given pawn at the given coordinates.
pawn_moves(Board, Coord, Color, Pawn_moves):-
    all_moves_from(Board, Coord, Color, Pawn_moves, pawn_move).

% all_moves_from(+Board, +Coord, +Color, -Moves, +Func)
% get all the moves for the piece at the given coordinates, that is generated by Func.
all_moves_from(Board, Coord, Color, Moves, Func):-
    findall(
        Move,
        ( 
            call(Func, Board, Color, move(Coord, NewCoord)),
            % stop if move is illegal
            not(is_illegal_coord(NewCoord)),
            % can't move to a square with a piece of the same color
            not(get_piece_at(NewCoord, Board, p(Color, _))),
            % Move needs to be a difference list so that we can merge em into 1
            Move = [move(Coord, NewCoord)| X]-X 
        ),
        K_Moves
    ),
    flatten_DLs_to_one_DL(K_Moves, Moves).