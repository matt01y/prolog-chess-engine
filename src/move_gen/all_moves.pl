:- module(all_moves).

:- use_module('../utils/utils').
:- use_module('../utils/board_utils').
:- use_module('../move_gen/directional').
:- use_module('../move_gen/pawn').
:- use_module('../move_gen/knight').
:- use_module('../move_gen/king').

% all_moves(+Board, +MetData, +Color, -Moves)
% get all the moves for the given color. MetaData is information about things that happend in the past.
% this is use to determine if certain special moves are possible (castling, en-passant).
all_moves(Board, Color, Moves):-
    findall(
        PieceMoves,
        (
            get_piece_at(Coord, Board, p(Color, Type)), % this doesn't select p(empty) pieces because we ask a p/2.
            all_moves_of_piece(Board, Coord, p(Color, Type), PieceMoves)
        ),
        PieceMovesList
    ),
    flatten_DLs_to_one_DL(PieceMovesList, Moves).

% all_moves_of_piece(+Board, +Coord, +Piece, -Moves)
% get all the moves for the given piece at the given coordinates.
all_moves_of_piece(Board, Coord, p(Color, rook), Hz-R):-
    horizontal_moves(Board, Coord, Color, Hz-Vt),
    vertical_moves(Board, Coord, Color, Vt-R), !.
all_moves_of_piece(Board, Coord, p(Color, bishop), Bishop_moves):-
    diagonal_moves(Board, Coord, Color, Bishop_moves), !.
all_moves_of_piece(Board, Coord, p(Color, knight), Knight_moves):-
    knight_moves(Board, Coord, Color, Knight_moves), !.
all_moves_of_piece(Board, Coord, p(Color, queen), Hz-R):-
    horizontal_moves(Board, Coord, Color, Hz-Vt),
    vertical_moves(Board, Coord, Color, Vt-Diag),
    diagonal_moves(Board, Coord, Color, Diag-R), !.
all_moves_of_piece(Board, Coord, p(white, king), King_moves):-
    king_moves(Board, Coord, Color, King_moves), !.
all_moves_of_piece(Board, Coord, p(black, king), King_moves):-
    king_moves(Board, Coord, Color, King_moves), !.
all_moves_of_piece(Board, Coord, p(Color, pawn), Pawn_moves):-
    pawn_moves(Board, Coord, Color, Pawn_moves), !.
