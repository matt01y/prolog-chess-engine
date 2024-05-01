:- module(utils, [flatten_DLs_to_one_DL/2]).

% flatten_DLs_to_one_DL(+LofDLs, -L)
% flattens a list of difference lists into one difference list.
flatten_DLs_to_one_DL(LofDLs, DL):-
    foldl(two_dl_to_one_dl, LofDLs, A-A, DL).
    
% two_dl_to_one_dl(+DL1, +DL2, -DL)
% merges two difference lists into one.
two_dl_to_one_dl(DL1-DL2, DL2-End, DL1-End).