:- module(parser, [parse_pgn/4]).

:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

% parse_state(+Input, -Movetext)
% Parses the input string and returns the game mode, the board, the current player's color, and the meta information for the white and black players.
parse_pgn(GameMode, Movetext)-->
    pgn_rules(GameMode), !,
    maybe(" "),
    pgn_moves(Movetext),
    end.

end --> spaces, maybe("*"), spaces, maybe("\n").

spaces --> "" | (" ", spaces).

% string_without(+Split, -HalveMove)
% Parses the string until the Split symbol and returns the string before Split and converts it from codes.
string_without_c(Split, HalveMove) --> string_without(Split, A), {string_codes(HalveMove, A)}.

% string_without_with(+Split, -HalveMove)
% Parses the string until the Split symbol and returns the string before Split, while also parsing the Split symbol.
string_without_c_with(Split, HalveMove) --> string_without_c(Split, HalveMove), Split.

% in_between(+Start, -Between, +End)
% Parses the string between Start and End
in_between(Start, Between, End) --> Start, string_without_c_with(End, Between).

% maybe(+A)
% Parses A or not equivalent with A? in regex
maybe(A) --> "" | A.

% pgn_rules(-GameMode)
% Parses all the rules and gives the game mode. either 'classic' or 'koth'
pgn_rules(GameMode) --> pgn_rule(GameMode), eol, pgn_rules(GameMode).
pgn_rules(classic) --> [].
pgn_rules(_) --> eol.

% pgn_rule(-GameMode)
% Parses a single rule and gives the game mode. either 'classic' or 'koth'
pgn_rule(GameMode) --> in_between("[Rules \"", GameModeAA, "\"]"), {string_lower(GameModeAA, GameMode)}.
pgn_rule(_) --> in_between("[", _, "]").

% pgn_moves(-Movetext)
% Parses all the moves and returns them in a list.
pgn_moves([]) --> [].
pgn_moves([WM,BM|R]) -->
    pgn_move_number(),
    pgn_halve_move_double(WM),
    pgn_halve_move_double(BM), !,
    pgn_moves(R).
pgn_moves([HalveMove]) -->
    pgn_move_number(),
    pgn_halve_move(HalveMove).

% pgn_move_number()
% parses the move number regex form "[0-9]+. ?""
pgn_move_number() --> string_without_c_with(".", _), !, maybe(" ").

% pgn_halve_move(-HalveMove)
% Parses a halve move and returns it in string notation.
pgn_halve_move_double(HalveMove) --> string_without_c_with(" ", HalveMove), {halve_check(HalveMove, HalveMove)}.
pgn_halve_move(Out) --> string_without_c(" ", HalveMove),{halve_check(HalveMove, Out)}, !.

halve_check(HalveMove, HalveMove):-
    HalveMove \= "",
    not(atom_concat(_, "\n", HalveMove)).
halve_check(HalveMove, Out):-
    HalveMove \= "",
    atom_concat(Out, "\n", HalveMove).

% halve_move(Type, EndCoord, Rest).