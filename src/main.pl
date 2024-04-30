:- module(main, [main/1]).
:- use_module('parser/parser').

:- initialization(main, main).

file_contains(File, GameMode, Movetext) :-
    phrase_from_file(parse_pgn(GameMode, Movetext), File).

main([File]) :-
    % trace,
    file_contains(File, GameMode, Movetext),
    write(GameMode),
    write(Movetext).
