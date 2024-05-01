:- module(board_utils, [get_starting_board/1, get_piece_at/3, is_illegal_coord/1]).


% get_starting_board(-Board)
% gives an initial board state as per the chess rules.
get_starting_board(b(R1, R2, R3, R4, R5, R6, R7, R8)) :-
    backrow(white, R1),
    frontrow(white, R2),
    emptyrow(R3),
    emptyrow(R4),
    emptyrow(R5),
    emptyrow(R6),
    frontrow(black, R7),
    backrow(black, R8).

% backrow(+Color, -Row)
% gives a back row for the given color.
backrow(Color, r(p(Color, rook), p(Color, knight), p(Color, bishop), p(Color, queen), p(Color, king), p(Color, bishop), p(Color, knight), p(Color, rook))).

% frontrow(+Color, -Row)
% gives a front row for the given color.
frontrow(Color, r(p(Color, pawn), p(Color, pawn), p(Color, pawn), p(Color, pawn), p(Color, pawn), p(Color, pawn), p(Color, pawn), p(Color, pawn))).

% emptyrow(?Row)
% gives an empty row. or checks if a row is empty.
emptyrow(r(p(empty), p(empty), p(empty), p(empty), p(empty), p(empty), p(empty), p(empty))).

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
