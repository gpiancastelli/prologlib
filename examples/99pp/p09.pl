% P09 (**):  Pack consecutive duplicates of list elements into sublists.

% pack(L1, L2) :- the list L2 is obtained from the list L1 by packing
%    repeated occurrences of elements into separate sublists.
% template: pack(+list, ?list)

pack([], []).
pack([X|Xs], [Z|Zs]) :- transfer(X, Xs, Ys, Z), pack(Ys, Zs).

% transfer(X, Xs, Ys, Z) :- Ys is the list that remains from the list
%    Xs when all leading copies of X are removed and transferred to Z

transfer(X, [], [], [X]).
transfer(X, [Y|Ys], [Y|Ys], [X]) :- X \= Y.
transfer(X, [X|Xs], Ys, [X|Zs]) :- transfer(X, Xs, Ys, Zs).

% Example:
% ?- pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e], X).
% X = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]
