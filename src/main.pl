:- module(main, [main/1]).
:- use_module('parser/parser').
:- use_module('board/board_gen').
:- use_module('move_gen/all_moves').
:- use_module('board/setup_board').

% :- initialization(main, main).

file_contains(File, GameMode, Movetext) :-
    phrase_from_file(parse_pgn(GameMode, Movetext), File).

main([File]) :- !,
    file_contains(File, GameMode, Movetext),
    setup_board(Movetext, Board),

    write(GameMode),
    write(Movetext).

main([File, B]):-
    % TODO
    string_codes("TEST", B), !,
    write(File),
    write("\n"),
    write(B),
    write("\n").
