% P63 (**) Construct a complete binary tree
%
% A complete binary tree with height H is defined as follows: 
% The levels 1,2,3,...,H-1 contain the maximum number of nodes 
% (i.e 2**(i-1) at the level i, note that we start counting the 
% levels from 1 at the root). In level H, which may contain less 
% than the maximum number possible of nodes, all the nodes are 
% "left-adjusted". This means that in a levelorder tree traversal 
% all internal nodes come first, the leaves come second, and
% empty successors (the nils which are not really nodes!) 
% come last. Complete binary trees are used for heaps.

% :- ensure_loaded(p57).

% complete_binary_tree(N, T) :- T is a complete binary tree with
% N nodes. (+,?)

complete_binary_tree(N, T) :- complete_binary_tree(N, T, 1).

complete_binary_tree(N, nil, A) :- A > N, !.
complete_binary_tree(N, t(_,L,R), A) :- A =< N,
    AL is 2 * A, AR is AL + 1,
    complete_binary_tree(N, L, AL),
    complete_binary_tree(N, R, AR).
