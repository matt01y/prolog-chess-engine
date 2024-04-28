:- module(main, [main/1]).
:- use_module('parser/parser').

:- initialization(main, main).


main([File]) :-
    open(File, read, IStream),
    read_string(IStream, _, Input),
    close(IStream),
    write(Input),
    trace,
    parse_state(GameMode, Input, []),
    write(GameMode).