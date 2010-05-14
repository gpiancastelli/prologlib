% P08 (**): Eliminate consecutive duplicates of list elements.

% compress(L1, L2) :- the list L2 is obtained from the list L1 by
%    compressing repeated occurrences of elements into a single copy
%    of the element.
% template: compress(+list, ?list)

compress([], []).
compress([X], [X]).
compress([X,X|Xs], Zs) :- compress([X|Xs], Zs).
compress([X,Y|Ys], [X|Zs]) :- X \= Y, compress([Y|Ys], Zs).

% Example:
% ?- compress([a,a,a,a,b,c,c,a,a,d,e,e,e,e], X).
% X = [a,b,c,a,d,e]
