:- module(check_check,[
    filter_moves_checked/4,
    in_check/2
]).

:- use_module('../board/board_utils').
:- use_module('../move_gen/all_moves').

filter_moves_checked(Board, Color, Moves, FilteredMoves):-
    findall(
        Move,
        (
            member(Move, Moves),
            move_piece_wrapper(Board, Move, NewBoard),
            not(in_check(NewBoard, Color))
        ),
        FilteredMoves
    ).

in_check(Board, Color):-
    get_piece_at(KingCoord, Board, p(Color, king)),
    other_color(Color, OppositeColor),
    all_moves(Board, OppositeColor, Moves-[]),
    include(=(m(_, _, KingCoord, attacking)), Moves, KingMoves),
    length(KingMoves, Len),
    Len > 0.