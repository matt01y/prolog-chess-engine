:- module(translate, [
    parse_known_data/3
]).


parse_known_data(m(castle, _/5, _/7, none, none, short)) --> "O-O".
parse_known_data(m(castle, _/5, _/3, none, none, long)) --> "O-O-O".
parse_known_data(m(Type, R/C, Nr/Nc, Attack, Promotion, none)) -->
    parse_type(Type), !,
    parse_col_maybe(C),
    parse_row_maybe(R),
    attack(Attack),
    parse_col(Nc),
    parse_row(Nr), !,
    parse_promotion(Promotion).

parse_type(queen) --> "Q".
parse_type(king) --> "K".
parse_type(rook) --> "R".
parse_type(bishop) --> "B".
parse_type(knight) --> "N".
parse_type(pawn) --> "".

parse_col_maybe(R) --> "" | parse_col(R).
parse_col(1) --> "a".
parse_col(2) --> "b".
parse_col(3) --> "c".
parse_col(4) --> "d".
parse_col(5) --> "e".
parse_col(6) --> "f".
parse_col(7) --> "g".
parse_col(8) --> "h".

parse_row_maybe(C) --> "" | parse_row(C).
parse_row(1) --> "1".
parse_row(2) --> "2".
parse_row(3) --> "3".
parse_row(4) --> "4".
parse_row(5) --> "5".
parse_row(6) --> "6".
parse_row(7) --> "7".
parse_row(8) --> "8".

attack(attacking) --> "x".
attack(none) --> "".

parse_promotion(Promo) --> "=" | parse_promotion_piece(Promo).
parse_promotion(none) --> "".
parse_promotion_piece(queen) --> "Q".
parse_promotion_piece(knight) --> "N".
parse_promotion_piece(rook) --> "R".
parse_promotion_piece(bishop) --> "B".
