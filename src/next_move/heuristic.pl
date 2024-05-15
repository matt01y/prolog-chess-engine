:- module(heuristic, [
    heuristic/2
]).

:- use_module('../board/board_utils').
:- use_module('../move_gen/check_check').

% heuristic(+Board, -Value)
% Value is the heuristic value of the board.

% the checkmate case should be rewarded/punished the most.
heuristic(Board, -10000):-
    checkmate(Board, white), !.
heuristic(Board, 10000):-
    checkmate(Board, black), !.
% base case
heuristic(Board, Value):-
    findall(p(C,T), get_piece_at(_, Board, p(C,T)), Pieces),
    partition(is_white, Pieces, WhitePieces, BlackPieces),
    score_pieces(WhitePieces, WhiteScore),
    score_pieces(BlackPieces, BlackScore),
    Value is WhiteScore - BlackScore.

% is_white(+Piece)
% true if the piece is white.
is_white(p(white, _)).

% score_pieces(+Pieces, -Score)
% Score is the sum of the values of the pieces in Pieces.
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
    
    