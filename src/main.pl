:- module(main, [main/1]).
:- use_module('parser/parser').
:- use_module('board/board_gen').
:- use_module('move_gen/all_moves').
:- use_module('board/setup_board').
:- use_module('board/board_utils').

:- initialization(main, main).

file_contains(File, GameMode, Movetext) :-
    phrase_from_file(parse_pgn(GameMode, Movetext), File).

% init
% initialize the global states.
init:-
    set_global_color(white),
    % left is always white
    b_setval(king_moved, (false, false)),
    b_setval(long_rook_moved, (false, false)),
    b_setval(short_rook_moved, (false, false)),
    set_last_move(none).

main([File]) :- !,
    file_contains(File, GameMode, Movetext), !,
    init, !,
    get_global_color(Color),
    write(Color),
    
    % setup_board(Movetext, Board),

    write(GameMode),
    write(Movetext).

main([File, B]):-
    % TODO
    string_codes("TEST", B), !,
    write(File),
    write("\n"),
    write(B),
    write("\n").
