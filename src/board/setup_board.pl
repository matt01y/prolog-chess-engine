:- module(setup_board,[
    setup_board/2
]).

:- use_module(board_gen).

setup_board(Moves, Board):-
    get_starting_board(B),
    setup_board_helper(Moves, B, Board).


setup_board_helper([], Board, Board).
setup_board_helper([Move|Rest], Board, NewBoard):-
    make_move(Move, Board, NextBoard),
    setup_board_helper(Rest, NextBoard, NewBoard).

make_move(m(castle, _, _, _, _, short), Board, NewBoard).
