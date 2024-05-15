:- module(check_check,[
    filter_moves_checked/4,
    current_state/3,
    checkmate/2,
    in_check/2
]).

:- use_module('../board/board_utils').
:- use_module('../move_gen/all_moves').

% filter_moves_checked(+Board, +Color, +Moves, -FilteredMoves)
% filters out the moves that would put the king in check.
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

% current_state(+Board, +Color, -State)
% returns the current state of the game.
current_state(Board, Color, check):- in_check(Board, Color).
current_state(Board, Color, checkmate):- checkmate(Board, Color).
current_state(Board, Color, remise):- remise(Board, Color).
current_state(_, _, normal).

% in_check(+Board, +Color)
% checks if the king of the given color is in check.
in_check(Board, Color):-
    get_piece_at(KingCoord, Board, p(Color, king)),
    other_color(Color, OppositeColor),
    all_moves(Board, OppositeColor, Moves-[]),
    include(=(m(_, _, KingCoord, attacking)), Moves, KingMoves),
    length(KingMoves, Len),
    Len > 0.

% checkmate(+Board, +Color)
% checks if the given color is in checkmate.
checkmate(Board, Color):-
    in_check(Board, Color),
    all_moves(Board, Color, Moves-[]),
    filter_moves_checked(Board, Color, Moves, FilteredMoves),
    length(FilteredMoves, 0).

% remise(+Board, +Color)
% checks if the given color is in remise.
remise(Board, Color):-
    not(in_check(Board, Color)),
    all_moves(Board, Color, Moves-[]),
    filter_moves_checked(Board, Color, Moves, FilteredMoves),
    length(FilteredMoves, 0).