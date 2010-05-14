% The family program
% From figure 1.8 in
%     Ivan Bratko
%     "Prolog Programming for Artificial Intelligence", 3rd edition
%     Addison-Wesley, 2001

parent(pam, bob).  % Pam is a parent of Bob
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

female(pam).  % Pam is female
male(tom).  % Tom is male
male(bob).
female(liz).
female(ann).
female(pat).
male(jim).

% Y is an offspring of X if X is a parent of Y
offspring(Y, X) :- parent(X, Y).

mother(X, Y) :-      % X is the mother of Y if...
    parent(X, Y),    % ...X is a parent of Y and...
    female(X).       % ...X is female

grandparent(X, Z) :-    % X is a grandparent of Z if...
    parent(X, Y),       % ...X is a parent of Y and...
    parent(Y, Z).       % ...Y is a parent of Z

sister(X, Y) :-
    parent(Z, X), parent(Z, Y),
    female(X),
    different(X, Y).

% The predecessor/2 predicate
predecessor(X, Z) :- parent(X, Z).
predecessor(X, Z) :-
    parent(X, Y),
    predecessor(Y, Z).

% Dummy different/2 predicate
different(X, Y) :- X \== Y.
