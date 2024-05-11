:- module(next_move, [next_move/1]).

:- use_module('move_gen/all_moves').
:- use_module('move_gen/check_check').
:- use_module('board/board_utils').

next_move(Board):-
    get_global_color(Color),
    all_moves(Board, Color, All_Moves-[]),
    filter_moves_checked(Board, Color, All_Moves, [Move|Rest]),
    print_next_move(Move, [Move|Rest]).

print_next_move(m(Type, _, To, Attack), Moves):-
    include(=(m(Type, _, To, Attack)), Moves, FMoves),
    length(FMoves, 1),
    print_type(Type),
    print_defaults(To, Attack).
print_next_move(m(Type, _/Fc, To, Attack), Moves):-
    include(=(m(Type, _/Fc, To, Attack)), Moves, FMoves),
    length(FMoves, 1),
    print_type(Type),
    print_col(Fc),
    print_defaults(To, Attack).
print_next_move(m(Type, Fr/_, To, Attack), Moves):-
    include(=(m(Type, Fr/_, To, Attack)), Moves, FMoves),
    length(FMoves, 1),
    print_type(Type),
    print_row(Fr),
    print_defaults(To, Attack).
print_next_move(m(Type, Fr/Fc, To, Attack), _):-
    print_type(Type),
    print_col(Fc),
    print_row(Fr),
    print_defaults(To, Attack).

% print_defaults(+Tr/Tc, +Attack)
% print the default values for the move.
print_defaults(Tr/Tc, Attack):-
    print_attack(Attack),
    print_col(Tc),
    print_row(Tr).

% print_type(+Type)
% print the type of the piece.
print_type(queen):- write("Q").
print_type(king):- write("K").
print_type(rook):- write("R").
print_type(bishop):- write("B").
print_type(knight):- write("N").
print_type(_).

% print_attack(+Attack)
% print the attack symbol.
print_attack(attacking):- write("x").
print_attack(none).

% print_col(+Col)
% print the column.
print_col(1):- write("a").
print_col(2):- write("b").
print_col(3):- write("c").
print_col(4):- write("d").
print_col(5):- write("e").
print_col(6):- write("f").
print_col(7):- write("g").
print_col(8):- write("h").

% print_row(+Row)
% print the row.
print_row(1):- write("1").
print_row(2):- write("2").
print_row(3):- write("3").
print_row(4):- write("4").
print_row(5):- write("5").
print_row(6):- write("6").
print_row(7):- write("7").
print_row(8):- write("8").