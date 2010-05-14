% P12 (**): Decode a run-length compressed list.

% decode(L1, L2) :- L2 is the uncompressed version of the run-length
%    encoded list L1.
% template: decode(+list, ?list)

decode([], []).
decode([X|Ys], [X|Zs]) :- not(list(X)), decode(Ys, Zs).
decode([[1,X]|Ys], [X|Zs]) :- decode(Ys, Zs).
decode([[N,X]|Ys], [X|Zs]) :- N > 1, N1 is N - 1, decode([[N1,X]|Ys], Zs).

% Example:
% ?- decode([[4,a],b,[2,c],[2,a],d,[4,e]], X).
% X = [a,a,a,a,b,c,c,a,a,d,e,e,e,e]
