:- module(all_moves, [all_moves/3]).

:- use_module('../utils/utils').
:- use_module('../utils/board_utils').
:- use_module('../move_gen/directional').
:- use_module('../move_gen/non_directional').

% all_moves(+Board, +Color, -Moves)
% get all the moves for the given color.
all_moves(Board, Color, Moves):-
    findall(
        PieceMoves,
        (
            get_piece_at(Coord, Board, p(Color, Type)), % this doesn't select p(empty) pieces because we ask a p/2.
            all_moves_of_piece(Board, Coord, p(Color, Type), PieceMoves)
        ),
        PieceMovesList
    ),
    flatten_DLs_to_one_DL(PieceMovesList, Moves),
    write(Moves).

% all_moves_of_piece(+Board, +Coord, +Piece, -Moves)
% get all the moves for the given piece at the given coordinates.
all_moves_of_piece(Board, Coord, p(Color, rook), Hz-R):-
    horizontal_moves(Board, Coord, p(Color, rook), Hz-Vt),
    vertical_moves(Board, Coord, p(Color, rook), Vt-R), !.
all_moves_of_piece(Board, Coord, p(Color, bishop), Bishop_moves):-
    diagonal_moves(Board, Coord, p(Color, bishop), Bishop_moves), !.
all_moves_of_piece(Board, Coord, p(Color, knight), Knight_moves):-
    knight_moves(Board, Coord, p(Color, knight), Knight_moves), !.
all_moves_of_piece(Board, Coord, p(Color, queen), Hz-R):-
    horizontal_moves(Board, Coord, p(Color, queen), Hz-Vt),
    vertical_moves(Board, Coord, p(Color, queen), Vt-Diag),
    diagonal_moves(Board, Coord, p(Color, queen), Diag-R), !.
all_moves_of_piece(Board, Coord, p(Color, king), King_moves):-
    king_moves(Board, Coord, p(Color, king), King_moves), !.
all_moves_of_piece(Board, Coord, p(Color, pawn), Pawn_moves):-
    pawn_moves(Board, Coord, p(Color, pawn), Pawn_moves), !.
