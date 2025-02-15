:- module(utils, [
    flatten_DLs_to_one_DL/2,
    meta/3,
    set_meta/3,
    print_move_nr/1,
    two_dl_to_one_dl/3
]).

% flatten_DLs_to_one_DL(+LofDLs, -L)
% flattens a list of difference lists into one difference list.
flatten_DLs_to_one_DL(LofDLs, DL):-
    foldl(two_dl_to_one_dl, LofDLs, A-A, DL).
    
% two_dl_to_one_dl(+DL1, +DL2, -DL)
% merges two difference lists into one.
two_dl_to_one_dl(DL1-DL2, DL2-End, DL1-End).

% meta(+Name, +Color, -Data)
% get the metadata for the given feature and color.
meta(Name, Color, Data):-
    b_getval(Name, WholeData),
    extract(Color, WholeData, Data).

% set_meta(+Name, +Color, +Data)
% set the metadata for the given feature and color.
set_meta(Name, Color, Data):-
    b_getval(Name, WholeData),
    insert(WholeData, Color, Data, NewWholeData),
    b_setval(Name, NewWholeData).

% insert(+Data, +Color, +Data, -NewData)
% update the correct field of the metadata.
insert((_, BlackData), white, Data, (Data, BlackData)).
insert((WhiteData, _), black, Data, (WhiteData, Data)).

% extract(+Color, +Data, -Data)
% get the correct field of the metadata.
extract(white, (Data, _), Data).
extract(black, (_, Data), Data).

% print_move_nr(+Movetext)
% print the move number.
print_move_nr(Movetext):-
    length(Movetext, L),
    ModL is L mod 3,
    print_move_nr_helper(L, ModL).

% print_move_nr_helper(+L, +ModL)
% print the move number if it is the correct value after modulus 3.
print_move_nr_helper(L, 0):-
    Number is L / 3 + 1,
    write(" "), write(Number), write(". ").
% otherwise just print a space.
print_move_nr_helper(_, _):- write(" ").
