% P68 (**) Preorder and inorder sequences of binary trees

% We consider binary trees with nodes that are identified by
% single lower-case letters.

% a1) Given a binary tree, construct its preorder sequence

preorder(T, S) :- '$preorder'(T, L), atom_chars(S, L).

'$preorder'(nil, []).
'$preorder'(t(X,Left,Right), [X|List]) :-
    '$preorder'(Left, ListLeft),
    '$preorder'(Right, ListRight),
    append(ListLeft, ListRight, List).

% a2) Given a binary tree, construct its preorder sequence

inorder(T, S) :- '$inorder'(T, L), atom_chars(S, L).

'$inorder'(nil, []).
'$inorder'(t(X,Left,Right), List) :-
    '$inorder'(Left, ListLeft),
    '$inorder'(Right, ListRight),
    append(ListLeft, [X|ListRight], List).
