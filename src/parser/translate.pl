:- module(translate, [
    parse_known_data/4
]).


parse_known_data(m(castle, short), "O-O") --> "O-O".
parse_known_data(m(castle, long), "O-O-O") --> "O-O-O".
parse_known_data(m(Type, R/C, Nr/Nc, Attack), MoveString) -->
    parse_type(Typ, TypString), !,
    parse_col_maybe(C, OC),
    parse_row_maybe(R, OR),
    attack(Attack, AttackString),
    parse_col(Nc, NCS),
    parse_row(Nr, NRS), !,
    parse_promotion(Typ, Type, PromoString), !,
    parse_extra(ExtraString), !,
    {atomic_list_concat(
        [TypString, OC, OR, AttackString, NCS, NRS, PromoString, ExtraString],
        MoveString
    )}.

parse_extra("+") --> "+", {b_setval(game_state, check)}.
parse_extra("#") --> "#", {b_setval(game_state, checkmate)}.
parse_extra("") --> "", {b_setval(game_state, normal)}.

parse_type(queen, "Q") --> "Q".
parse_type(king, "K") --> "K".
parse_type(rook, "R") --> "R".
parse_type(bishop, "B") --> "B".
parse_type(knight, "N") --> "N".
parse_type(pawn, "") --> "".

parse_col_maybe(_, "") --> "".
parse_col_maybe(C, String) --> parse_col(C, String).
parse_col(1, "a") --> "a".
parse_col(2, "b") --> "b".
parse_col(3, "c") --> "c".
parse_col(4, "d") --> "d".
parse_col(5, "e") --> "e".
parse_col(6, "f") --> "f".
parse_col(7, "g") --> "g".
parse_col(8, "h") --> "h".

parse_row_maybe(_, "") --> "".
parse_row_maybe(R, String) --> parse_row(R, String).
parse_row(1, "1") --> "1".
parse_row(2, "2") --> "2".
parse_row(3, "3") --> "3".
parse_row(4, "4") --> "4".
parse_row(5, "5") --> "5".
parse_row(6, "6") --> "6".
parse_row(7, "7") --> "7".
parse_row(8, "8") --> "8".

attack(attacking, "x") --> "x".
attack(none, "") --> "".

parse_promotion(pawn, promotion(Promo), String) --> "=" , !, parse_promotion_piece(Promo, String).
parse_promotion(Typ, Typ, "") --> "".
parse_promotion_piece(queen, "=Q") --> "Q".
parse_promotion_piece(knight, "=N") --> "N".
parse_promotion_piece(rook, "=R") --> "R".
parse_promotion_piece(bishop, "=B") --> "B".
