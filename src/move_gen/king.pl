:- module(king, [
    king_moves/4
]).

:- use_module('../utils/board_utils').
:- use_module('../utils/utils').

king_moves(Board, R/C, Color, King_moves):-
    findall(
        Move,
        (
            member(DeltaX, [-1, 0, 1]),
            member(DeltaY, [-1, 0, 1]),
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
    flatten_DLs_to_one_DL(Moves, King_moves).
