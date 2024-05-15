:- module(heuristic, [
    heuristic/2
]).

:- use_module('../board/board_utils').


heuristic(Board, Value):-
    findall(p(C,T), get_piece_at(_, Board, p(C,T)), Pieces),
    partition(is_white, Pieces, WhitePieces, BlackPieces),
    write("white: "), 
    score_pieces(WhitePieces, WhiteScore),
    length(WhitePieces, L),
    write(L), write(" "),
    write(WhiteScore), nl,
    score_pieces(BlackPieces, BlackScore),
    length(BlackPieces, L2),
    write("black: "), write(L2), write(" "), write(BlackScore), nl,
    Value is WhiteScore - BlackScore.

is_white(p(white, _)).

score_pieces([], 0).
score_pieces([p(_, T)|Pieces], Score):-
    score_pieces(Pieces, RestScore),
    piece_value(T, PieceValue),
    Score is RestScore + PieceValue.

% piece_value(Piece, Value)
% Value is the value of the piece Piece
% scores where chosen by ai.
piece_value(king, 1000).
piece_value(queen, 9).
piece_value(rook, 5).
piece_value(bishop, 3).
piece_value(knight, 3).
piece_value(pawn, 1).
    
    