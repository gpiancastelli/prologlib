member(E, [E|_]).
member(E, [_|L]) :- member(E, L).

append([], L2, L2).
append([E|T1], L2, [E|T2]) :- append(T1, L2, T2).

reverse(L1, L2) :- reverse(L1, [], L2).
reverse([], Acc, Acc).
reverse([H|T], Acc, Y) :- reverse(T, [H|Acc], Y).

delete(E, [], []).
delete(E, [E|T], L) :- !, delete(E, T, L).
delete(E, [H|T], [H|L]) :- delete(E, T, L).

% Find the last element of a list.
last([X], X).
last([_|L], X) :- last(L, X).

% Find the last but one element of a list.
last_but_one([X,_], X).
last_but_one([_,Y|Ys], X) :- last_but_one([Y|Ys], X).

% nth(+index, ?element, ?list)
% Find the Nth element of a list (starting by 1). This is very
% limited w.r.t. the standard nth/3, where index is specified
% as ? rather than +. So, in particular, here the generative
% queries (e.g. nth(N, [1,2,3], E) & nth(N, T, E)) do not work.
nth(1, [X|_], X).
nth(N, [_|L], X) :- N > 1, N1 is N - 1, nth(N1, L, X).

length(L, S) :- number(S), !, '$lengthN'(L, S), !.
length(L, S) :- var(S), '$lengthX'(L, S).
'$lengthN'([], 0).
'$lengthN'(_, N) :- N < 0, !, fail.
'$lengthN'([_|L], N) :- '$lengthN'(L, M), N is M + 1.
'$lengthX'([], 0).
'$lengthX'([_|L], N) :- '$lengthX'(L, M), N is M + 1.

palindrome(L) :- reverse(L, L).

list(X) :- var(X), !, fail.
list([]).
list([_|T]) :- list(T).

% flatten(+list, ?list)
% Flatten a nested list structure. (I am told that the template
% for the "standard" flatten/2 is flatten(+list, -list).)
flatten(X, [X]) :- not(list(X)).
flatten([], []).
flatten([X|Xs], Zs) :- flatten(X, Y), flatten(Xs, Ys), append(Y, Ys, Zs).

% duplicate(?list, ?list)
% Duplicate all elements in a list.
duplicate([], []).
duplicate([X|Xs], [X,X|Ys]) :- duplicate(Xs, Ys).

% duplicate(?list, +integer, ?list)
% Duplicate the elements of a list a given number of times.
duplicate(L1, N, L2) :- '$duplicate'(L1, N, L2, N).
'$duplicate'([], _, [], _).
'$duplicate'([_|Xs], N, Ys, 0) :- '$duplicate'(Xs, N, Ys, N).
'$duplicate'([X|Xs], N, [X|Ys], K) :- K > 0, K1 is K - 1, '$duplicate'([X|Xs], N, Ys, K1).

% drop(?list, +integer, ?list)
% Drop every Nth element from a list.
drop(L1, N, L2) :- '$drop'(L1, N, L2, N).
'$drop'([], _, [], _).
'$drop'([_|Xs], N, Ys, 1) :- '$drop'(Xs, N, Ys, N).
'$drop'([X|Xs], N, [X|Ys], K) :- K > 1, K1 is K - 1, '$drop'(Xs, N, Ys, K1).

% split(?list, +integer, ?list, ?list)
% Split a list into two parts.
split(L, 0, [], L).
split([X|Xs], N, [X|Ys], Zs) :- N > 0, N1 is N - 1, split(Xs, N1, Ys, Zs).

% slice(?list, +integer, +integer, ?list)
% Extract a slice from a list (boundaries included), where indexes start from 1.
slice([X|_], 1, 1, [X]).
slice([X|Xs], 1, K, [X|Ys]) :- K > 1, K1 is K - 1, slice(Xs, 1, K1, Ys).
slice([_|Xs], I, K, Ys) :- I > 1, I1 is I - 1, K1 is K - 1, slice(Xs, I1, K1, Ys).

% remove(?list, +integer, ?element, ?list)
% Remove the Nth element from a list, starting from 1.
remove([X|Xs], 1, X, Xs).
remove([Y|Xs], K, X, [Y|Ys]) :- K > 1, K1 is K - 1, remove(Xs, K1, X, Ys).

% insert(?list, +integer, ?element, ?list)
% Insert an element at a given position into a list, starting from 1.
insert(L, K, X, R) :- remove(R, K, X, L).

% range(+integer, +integer, ?list)
% Create a list containing all integers within a given range.
range(I, I, [I]).
range(I, K, [I|L]) :- I < K, I1 is I + 1, range(I1, K, L).


