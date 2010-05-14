% tree(T)
% Check whether T is a term representing a binary tree
tree(nil).
tree(t(_,L,R)) :- tree(L), tree(R).

% symmetric(T)
% Check whether the binary tree T is symmetric.
symmetric(nil).
symmetric(t(_,L,R)) :- '$mirror'(L, R).
'$mirror'(nil, nil).
'$mirror'(t(_,L1,R1), t(_,L2,R2)) :- '$mirror'(L1, R2), '$mirror'(R1, L2).

% count_leaves(T, N)
% Count the leaves in a binary tree
count_leaves(nil, 0).
count_leaves(t(_,nil,nil), 1) :- !.
count_leaves(t(_,L,R), N) :- count_leaves(L, NL), count_leaves(R, NR), N is NL + NR.

% leaves(T, L)
% Collect the leaves of a binary tree in a list
leaves(nil, []).
leaves(t(X,nil,nil), [X]) :- !.
leaves(t(_,L,R), S) :- leaves(L, SL), leaves(R, SR), append(SL, SR, S).

% internals(T, L)
% Collect the internal nodes of a binary tree in a list
internals(nil, []).
internals(t(_,nil,nil), []) :- !.
internals(t(X,L,R), [X|S]) :- 
    internals(L, SL), internals(R, SR), append(SL, SR, S).

% atlevel(T, L)
% Collect the nodes of a binary tree at a given level in a list
atlevel(nil, _, []).
atlevel(t(X,_,_), 1, [X]).
atlevel(t(_,L,R), D, S) :- D > 1, D1 is D-1,
    atlevel(L, D1, SL), atlevel(R, D1, SR), append(SL, SR, S).
