:- module(setup_board,[
    setup_board/2,
    make_move/3
]).

:- use_module(board_gen).
:- use_module(board_utils).
:- use_module('../move_gen/all_moves').
:- use_module('../move_gen/check_check').

% setup_board(+Moves, -Board)
% sets up the board so that it represents the state after the moves.
setup_board(Moves, Board):-
    get_starting_board(B),
    setup_board_helper(Moves, B, Board).

% setup_board_helper(+Moves, +Board, -NewBoard)
% sets 1 move at a time.
setup_board_helper([], Board, Board).
setup_board_helper([Move|Rest], Board, NewBoard):-
    make_move(Move, Board, NextBoard),
    setup_board_helper(Rest, NextBoard, NewBoard).

% make_move(+Move, +Board, -NewBoard)
% makes 1 move in the setup_board process.

% castle case seperatly cause we can do this faster than generating all moves.
make_move(m(castle, Lenght), Board, NewBoard):-
    move_piece_wrapper(Board, m(castle, Lenght), NewBoard).
% en passent case
make_move(m(pawn, From, To, attacking), Board, NewBoard):-
    % check if it is en passent
    get_piece_at(To, Board, p(empty)),
    get_global_color(Color),
    all_moves(Board, Color, All_Moves-[]),
    include(
        =(m(en_passent, From, To, attacking)),
        All_Moves,
        Moves
    ),
    Moves = [Move],
    move_piece_wrapper(Board, Move, NewBoard).
% Promotion, regular
make_move(In_move, Board, NewBoard):-
    get_global_color(Color),
    all_moves(Board, Color, All_Moves-[]), !,
    include(
        =(In_move),
        All_Moves,
        Moves
    ), !,
    singular_move(Board, Color, Moves, M), !,
    move_piece_wrapper(Board, M, NewBoard).

% singular_move(+Board, +Color, +Moves, -Move)
% get the only correct move from a list of moves.
singular_move(_, _, [Move], Move).
singular_move(Board, Color, Moves, Move):-
    filter_moves_checked(Board, Color, Moves, [Move]).