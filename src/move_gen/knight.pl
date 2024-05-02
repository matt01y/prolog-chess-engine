:- module(knight, [knight_moves/4]).

:- use_module('../utils/board_utils').
:- use_module('../utils/utils').

knight_moves(Board, R/C, Color, Knight_moves):-
    findall(
        Move,
        (
            member(DeltaX, [-2, -1, 1, 2]),
            member(DeltaY, [-2, -1, 1, 2]),
            abs(DeltaX, AbsDeltaX),
            abs(DeltaY, AbsDeltaY),
            AbsDeltaX + AbsDeltaY =:= 3,
            NR is R + DeltaX,
            NC is C + DeltaY,
            not(is_illegal_coord(NR/NC)),
            % can't move to a square with a piece of the same color
            not(get_piece_at(NR/NC, Board, p(Color, _))),
            % Move needs to be a difference list so that we can merge em into 1
            Move is [move(R/C, NR/NC)| X]-X 
        ),
        Moves
    ),
    flatten_DLs_to_one_DL(Moves, Knight_moves).