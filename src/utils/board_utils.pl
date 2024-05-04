:- module(board_utils, [
    get_piece_at/3,
    is_illegal_coord/1,
    inc_row/2,
    inc_col/2,
    dec_col/2,
    dec_row/2,
    other_color/2
]).

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