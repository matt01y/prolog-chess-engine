:- module(utils, [
    flatten_DLs_to_one_DL/2,
    meta/3,
    set_meta/3,
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