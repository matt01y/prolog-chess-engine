:- module(main, [main/1]).
:- use_module('parser/parser').
:- use_module('board/board_gen').
:- use_module('move_gen/all_moves').
:- use_module('board/setup_board').
:- use_module('board/board_utils').
:- use_module('next_move').
:- use_module('utils').
:- use_module('move_gen/check_check').

:- initialization(main, main).

file_contains(File, GameMode, Moves, Movetext, End) :-
    phrase_from_file(parse_pgn(GameMode, Moves, Movetext, End), File).

% init(+GameMode)
% initialize the global states.
init(GameMode):-
    set_global_color(white),
    % left is always white
    b_setval(king_moved, (false, false)),
    b_setval(long_rook_moved, (false, false)),
    b_setval(short_rook_moved, (false, false)),
    b_setval(gamemode, GameMode),
    set_last_move(none).

% startup (+File, -Movetext, -Movetext_concat, -Board)
% Parse the file and setup the board.
% this function exists to execute the overlapping parts of main and main test.
startup(File, Movetext, Movetext_concat, Board, End):-
    file_contains(File, GameMode, Moves, Movetext, End), !,
    init(GameMode), !,
    setup_board(Moves, Board), !,
    atomic_list_concat(Movetext, " ", Movetext_concat), !.

% main (+File)
% Main function to run the program.
main([File]) :- !,
    startup(File, Movetext, Movetext_concat, Board, End), !,
    write(Movetext_concat), !,
    print_move_nr(Movetext), !,
    write(End), !,
    next_move(Board, End), !,
    nl.
main([File, B]):-
    string_codes("TEST", B), !,
    startup(File, Movetext, Movetext_concat, Board, End), !,
    filterd_moves_current_color(Board, FMoves), !,
    forall(
        member(Move, FMoves),
        (
            write(Movetext_concat),
            print_move_nr(Movetext),
            write(End),
            print_next_move(Move, FMoves), nl
        )
    ), !.