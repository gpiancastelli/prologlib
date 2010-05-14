% P57 (**) Binary search trees (dictionaries)

% Use the predicate add/3, developed in chapter 4 of the course,
% to write a predicate to construct a binary search tree 
% from a list of integer numbers. Then use this predicate to test 
% the solution of the problem P56

% :- ensure_loaded(p56).
symmetric(nil).
symmetric(t(_,L,R)) :- '$mirror'(L, R).
'$mirror'(nil, nil).
'$mirror'(t(_,L1,R1), t(_,L2,R2)) :- '$mirror'(L1, R2), '$mirror'(R1, L2).

% add(X, T1, T2) :- the binary dictionary T2 is obtained by 
% adding the item X to the binary dictionary T1
% (element,binary-dictionary,binary-dictionary) (i,i,o)

add(X, nil, t(X,nil,nil)).
add(X, t(Root,L,R), t(Root,L1,R)) :- X @< Root, add(X, L, L1).
add(X, t(Root,L,R), t(Root,L,R1)) :- X @> Root, add(X, R, R1).

construct(L, T) :- construct(L, T, nil).

construct([], T, T).
construct([N|Ns], T, T0) :- add(N, T0, T1), construct(Ns, T, T1).
 	
test_symmetric(L) :- construct(L, T), symmetric(T).

% Examples:
% ?- test_symmetric([5,3,18,1,4,12,21]).
% yes
% ?- test_symmetric([3,2,5,7,4]).
% no
