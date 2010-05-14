% partition(L, X) :- samesize(X, Y), part(L, Y), X = Y.
partition(L, X) :- samesize(X, Y), part(L, Y), Y = X.

part([], [[]]).
part([], [[] | T]) :- part([], T).
part([H | L], O) :- copy_term(O, O2), part(L, O2), update(O2, H, O).

update([X | T], A, [[A | X] | T]).
update([X | T], A, [X | T2]) :- not(member(A, X)), update(T, A, T2).
% update([X | T], A, [X | T2]) :- \+(member(A, X)), update(T, A, T2).

samesize([], []).
samesize([_ | T1], [_ | T2]) :- samesize(T1, T2).

member(E, [E | _]).
member(E, [_ | L]) :- member(E, L).

% ?- partition([a, b], [X, Y]).
