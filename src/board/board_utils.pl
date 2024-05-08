:- module(board_utils, [
    get_piece_at/3,
    is_illegal_coord/1,
    inc_row/2,
    inc_col/2,
    dec_col/2,
    dec_row/2,
    other_color/2,
    move_piece/3,
    set_global_color/1,
    get_global_color/1,
    change_global_color/0,
    castle_row/2
]).

:- use_module('../utils').

% get_piece_at(+Coord, +Board, -Piece)
% gives the piece at the given coordinate.
get_piece_at(R/C, Board, Piece):-
    arg(R, Board, Row),
    arg(C, Row, Piece).

% is_illegal_coord(+Coord)
% checks if the given coordinate is illegal.
is_illegal_coord(R/_):- R < 1, !.
is_illegal_coord(R/_):- R > 8, !.
is_illegal_coord(_/C):- C < 1, !.
is_illegal_coord(_/C):- C > 8, !.

% other_color(+Color1, -Color2)
% gives the other color.
other_color(white, black).
other_color(black, white).

% increment(+A, -B)
% is B = A + 1?
increment(A, B) :- B is A + 1.
% inc_row(+R, -NR)
% increment the row
inc_row(R/C, NR/C):- increment(R, NR).
% inc_col(+C, -NC)
% increment the column
inc_col(R/C, R/NC):- increment(C, NC).

% same as the increment but for decrement
decrement(A, B) :- B is A - 1.
dec_row(R/C, NR/C):- decrement(R, NR).
dec_col(R/C, R/NC):- decrement(C, NC).

% castle_row(+Color, -Row)
% gives the row of the king for the given color in case of castling.
castle_row(white, 1).
castle_row(black, 8).

% move_piece(+Board, +Move, -NewBoard)
% move the piece from the given move.

% short castling case
move_piece(Board, m(castle, short), NewBoard):-
    get_global_color(Color),
    castle_row(Color, R),
    set_meta(short_rook_moved, Color, true),
    move_piece(Board, m(king, R/5, R/7, none), TempBoard),
    move_piece(TempBoard, m(rook, R/8, R/6, none), NewBoard).
% long castling case
move_piece(Board, m(castle, long), NewBoard):-
    get_global_color(Color),
    castle_row(Color, R),
    set_meta(long_rook_moved, Color, true),
    move_piece(Board, m(king, R/5, R/3, none), TempBoard),
    move_piece(TempBoard, m(rook, R/1, R/4, none), NewBoard).
% promotion case
move_piece(Board, m(promotion(Type), From, To, _), NewBoard):-
    get_piece_at(From, Board, p(Color, pawn)),
    set_empty_piece_at(From, Board, TempBoard),
    set_piece_at(To, p(Color, Type), TempBoard, NewBoard).
% general case if all else fails. This is for just a plane and simple piece move.
move_piece(Board, m(Type, From, To, _), NewBoard):-
    get_piece_at(From, Board, p(Color, Type)),
    set_empty_piece_at(From, Board, TempBoard),
    set_piece_at(To, p(Color, Type), TempBoard, NewBoard).

% set_piece_at(+Coord, +Piece, +Board, -NewBoard)
% set the piece at the given coordinate. (duplicates the board, and breaks the logical paradigm by using setarg/3)
set_piece_at(R/C, Piece, Board, NewBoard):-
    duplicate_term(Board, NewBoard),
    arg(R, NewBoard, Row),
    setarg(C, Row, Piece).

% set_empty_piece_at(+Coord, +Board, -NewBoard)
% set the empty piece at the given coordinate.
set_empty_piece_at(R/C, Board, NewBoard):-
    set_piece_at(R/C, p(empty), Board, NewBoard).

% change_global_color
% changes the global color.
change_global_color:-
    b_getval(color, Color),
    other_color(Color, OtherColor),
    b_setval(color, OtherColor).

% get_global_color(-Color)
% get the global color.
get_global_color(Color):-
    b_getval(color, Color).

% set_global_color(+Color)
% set the global color.
set_global_color(Color):-
    b_setval(color, Color).


    