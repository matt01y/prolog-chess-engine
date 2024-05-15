% Code inspired by GAUTHIER PICARD his minimax implementation

:- module(minimax, [minimax/5]).

:- use_module(heuristic).
:- use_module('../board/board_utils').
:- use_module('../move_gen/all_moves').
:- use_module('../move_gen/check_check').

% minimax(Pos, BestNextPos, Val)
% Pos is a position, Val is its minimax value.
% Best move from Pos leads to position BestNextPos.
minimax(Board, Color, BestNextMove, Val, Depth) :-                     % Pos has successors
    all_moves(Board, Color, Moves-[]),
    filter_moves_checked(Board, Color, Moves, FilteredMoves),
    include(is_attacking, FilteredMoves, AttackingMoves), !,
    perf_switcher(Board, AttackingMoves, FilteredMoves, Color, BestNextMove, Val, Depth).

% perf_switcher(+Board, +AttackingMoves, +FilteredMoves, +Color, -BestNextMove, -Val, +Depth)
% first cut in tree, prefer attacking moves as random won't always retaliate.
perf_switcher(Board, AttackingMoves, FilteredMoves, Color, BestNextMove, Val, Depth):-
    length(AttackingMoves, 0), !,
    best(Board, FilteredMoves, Color, BestNextMove, Val, Depth).
perf_switcher(Board, AttackingMoves, _, Color, BestNextMove, Val, Depth):-
    best(Board, AttackingMoves, Color, BestNextMove, Val, Depth).

% is_attacking(+Move)
% true if the move is an attacking move.
is_attacking(m(_, _, _, attacking)).

% minimax_jumper(+Board, +Color, -BestNextMove, -Val, +Depth)
% this is meant as a jump point for when we want to do recursive calls to minimax.

% stop if depth 0 is reached
minimax_jumper(Board, _, _, Val, 0):- !,
    heuristic(Board, Val).
% go forth and search!
minimax_jumper(Board, Color, BestNextMove, Val, Depth):-
    NewDepth is Depth - 1,
    other_color(Color, OtherColor),
    set_global_color(OtherColor),
    minimax(Board, OtherColor, BestNextMove, Val, NewDepth), !.

% best(+Board, +Moves, +Color, -BestNextMove, -Val, +Depth)
% gets the best move and its value from a list of moves.
best(Board, [Move], Color, Move, Val, Depth) :-
    move_piece_wrapper(Board, Move, NewBoard),
    minimax_jumper(NewBoard, Color, _, Val, Depth), !.
best(Board, [Move1 | Rest], Color, BestNextMove, BestVal, Depth) :-
    move_piece_wrapper(Board, Move1, NewBoard),
    minimax_jumper(NewBoard, Color, _, Val1, Depth),
    best(Board, Rest, Color, Move2, Val2, Depth),
    betterOf(Color, Move1, Val1, Move2, Val2, BestNextMove, BestVal).

% betterOf(+Color, +Pos0, +Val0, +Pos1, +Val1, -BestPos, -BestVal)
% gives the better of the 2 moves and its value
betterOf(black, Pos0, Val0, _, Val1, Pos0, Val0) :-   % Pos0 better than Pos1
    get_global_color(black),
    Val0 > Val1, !.                             % MAX prefers the greater value
betterOf(white, Pos0, Val0, _, Val1, Pos0, Val0) :-
    get_global_color(white),
    Val0 < Val1, !.                            % MIN prefers the lesser value
betterOf(_, _, _, Pos1, Val1, Pos1, Val1).            % Otherwise Pos1 better than Pos0