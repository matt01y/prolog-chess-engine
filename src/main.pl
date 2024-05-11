:- module(main, [main/1]).
:- use_module('parser/parser').
:- use_module('board/board_gen').
:- use_module('move_gen/all_moves').
:- use_module('board/setup_board').
:- use_module('board/board_utils').
:- use_module('next_move').
:- use_module('utils').

:- initialization(main, main).

file_contains(File, GameMode, Moves, Movetext) :-
    phrase_from_file(parse_pgn(GameMode, Moves, Movetext), File).

% init
% initialize the global states.
init(GameMode):-
    set_global_color(white),
    % left is always white
    b_setval(king_moved, (false, false)),
    b_setval(long_rook_moved, (false, false)),
    b_setval(short_rook_moved, (false, false)),
    b_setval(gamemode, GameMode),
    set_last_move(none).

main([File]) :- !,
    file_contains(File, GameMode, Moves, Movetext), !,
    init(GameMode), !,
    setup_board(Moves, Board),
    atomic_list_concat(Movetext, " ", Movetext_concat),
    write(Movetext_concat),
    print_move_nr(Movetext),
    next_move(Board),
    nl.

main([File, B]):-
    % TODO
    string_codes("TEST", B), !,
    write(File),
    write("\n"),
    write(B),
    write("\n").