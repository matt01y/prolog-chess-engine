% Code inspired by GAUTHIER PICARD his minimax implementation

:- module(minimax, [minimax/3]).

:- use_module(heuristic).
:- use_module('../board/board_utils').
:- use_module('../move_gen/all_moves').

% minimax(Pos, BestNextPos, Val)
% Pos is a position, Val is its minimax value.
% Best move from Pos leads to position BestNextPos.
minimax(Board, Color, BestNextMove, Val, Depth) :-                     % Pos has successors
    all_moves(Board, Color, Moves-[]),
    best(Moves, Color, BestNextMove, Val, NewDepth), !.

minimax_jumper(Board, _, _, Val, 0):- !,
    heuristic(Board, Val).

minimax_jumper(Board, Color, BestNextMove, Val, Depth):-
    NewDepth is Depth - 1,
    other_color(Color, OtherColor),
    set_global_color(OtherColor),
    minimax(Board, OtherColor, BestNextMove, Val, NewDepth), !.

best([Move], Color, Move, Val, Depth) :-
    move_piece_wrapper(Board, Move, NewBoard),
    minimax_jumper(NewBoard, Color, _, Val, Depth), !.

best([Move | Rest], Color, BestNextMove, BestVal, Depth) :-
    move_piece_wrapper(Board, Move, NewBoard),
    minimax_jumper(NewBoard, Color, _, Val1, Depth),
    best(Rest, Color, Move2, Val2, Depth),
    betterOf(Color, Pos1, Val1, Pos2, Val2, BestPos, BestVal).

betterOf(black, Pos0, Val0, _, Val1, Pos0, Val0) :-   % Pos0 better than Pos1
    get_global_color(black),
    Val0 > Val1, !.                             % MAX prefers the greater value
betterOf(white, Pos0, Val0, _, Val1, Pos0, Val0) :-
    get_global_color(white),
    Val0 < Val1, !.                            % MIN prefers the lesser value

betterOf(_, _, _, Pos1, Val1, Pos1, Val1).            % Otherwise Pos1 better than Pos0