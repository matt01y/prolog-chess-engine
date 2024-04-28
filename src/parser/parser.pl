:- module(parser, [parse_state/3]).


maybe_nl --> ("" |"\n").
extra_option --> "[",_,"]", maybe_nl.

% parse_state(+Input, -GameMode, -Board, -Color, -Mwhite, -Mblack)
% Parses the input string and returns the game mode, the board, the current player's color, and the meta information for the white and black players.
parse_state(GameMode) -->
    parse_game_mode(GameMode),
    parse_rest_of_options.
    % parse_moves(Board, Color, Mwhite, Mblack).

% parse_game_mode(-GameMode)
% Parses the game mode. either 'classic' or 'koth'
parse_game_mode(koth) --> "[Rules \"koth\"]", maybe_nl.
parse_game_mode(classic) --> "[Rules \"classic\"]", maybe_nl.
parse_game_mode(GameMode) --> extra_option, parse_game_mode(GameMode).
parse_game_mode(classic) --> [].

% parse_rest_of_options
% parse the rest of the options we wont be needing, but still need to parse.
parse_rest_of_options --> extra_option, parse_rest_of_options.
parse_rest_of_options --> maybe_nl.
