:- module(parser, [parse_pgn/6]).

:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(translate).

% parse_state(+Input, -Movetext)
% Parses the input string and returns the game mode, the board, the current player's color, and the meta information for the white and black players.
parse_pgn(GameMode, Moves, Movetext, End)-->
    pgn_rules(GameMode), !,
    maybe(" "),
    pgn_moves(Moves, Movetext),
    end(End).

% end
% Parses the end of the file
end("") --> spaces, maybe("*"), spaces, maybe("\n").
end(B) --> string_without_c("\n", B), maybe("\n").



% spaces
% Parses any number of spaces (it's in the name)
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
pgn_moves([WM,BM|R], [C,WT,BT|RT]) -->
    pgn_move_number(C),
    parse_known_data(WM, WT)," ",
    parse_known_data(BM, BT), !, maybe(" "),
    pgn_moves(R, RT).
pgn_moves([HalveMove], [C,MoveString]) -->
    pgn_move_number(C),
    parse_known_data(HalveMove, MoveString).
pgn_moves([], []) --> [].

% pgn_move_number()
% parses the move number regex form "[0-9]+. ?""
pgn_move_number(C) --> (" "|""), string_without_c_with(".", A), {string_codes(B, A), string_concat(B, ".", C)
}, !, maybe(" ").