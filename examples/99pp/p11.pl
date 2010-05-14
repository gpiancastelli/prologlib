% P11 (*):  Modified run-length encoding

% encode_modified(L1, L2) :- the list L2 is obtained from the list L1 by 
%    run-length encoding. Consecutive duplicates of elements are encoded 
%    as terms [N,E], where N is the number of duplicates of the element E.
%    However, if N equals 1 then the element is simply copied into the 
%    output list.
% template: encode_modified(+list, ?list)

:- ensure_loaded(p10).

encode_modified(L1, L2) :- encode(L1, L), strip(L, L2).

strip([], []).
strip([[1,X]|Ys], [X|Zs]) :- strip(Ys, Zs).
strip([[N,X]|Ys], [[N,X]|Zs]) :- N > 1, strip(Ys, Zs).

% Example:
% ?- encode_modified([a,a,a,a,b,c,c,a,a,d,e,e,e,e], X).
% X = [[4,a],b,[2,c],[2,a],d,[4,e]]
