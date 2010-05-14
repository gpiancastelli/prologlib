% mtree(T)
% Check if T is a term representing a multiway tree.
mtree(t(_,F)) :- forest(F).
forest([]).
forest([T|Ts]) :- mtree(T), forest(Ts).

% count_nodes(T, N)
% Count the nodes in a multiway tree T.
count_nodes(t(_,F), N) :- count_nodes(F, NF), N is NF + 1.
count_nodes([], 0).
count_nodes([T|Ts], N) :- count_nodes(T, NT), count_nodes(Ts, NTs), N is NT + NTs.

% build_tree(S, T)
% Build a multiway tree from a string.
% We suppose that the nodes of a multiway tree contain single characters.
% In the depth-first order sequence of its nodes, a special character ^
% has been inserted whenever, during the tree traversal, the move is a
% backtrack to the previous level.
build_tree(TS, T) :- atom(TS), !, atom_chars(TS, TL), '$build_tree'(TL-[], T).
build_tree(TS, T) :- nonvar(T), '$build_tree'(TL-[], T), atom_chars(TS, TL).
'$build_tree'([X|F1]-T, t(X,F)) :- '$build_forest'(F1-['^'|T], F).
'$build_forest'(F-F, []).
'$build_forest'(F1-F3, [T|F]) :- '$build_tree'(F1-F2, T), '$build_forest'(F2-F3, F).

% ipl(T, L)
% Determine the internal path length of a tree.
% We define the internal path length of a multiway tree as the total
% sum of the path lengths from the root to all nodes of the tree.
ipl(T, L) :- ipl(T, 0, L).
ipl(t(_,F), D, L) :- D1 is D + 1, ipl(F, D1, LF), L is LF + D.
ipl([], _, 0).
ipl([T1|Ts], D, L) :- ipl(T1, D, L1), ipl(Ts, D, Ls), L is L1 + Ls.

% bus(T, L).
% Construct the bottom-up order sequence L of the nodes in the tree T
bus(T, Seq) :- nonvar(T), !, '$bus_forest'(T, Seq).
bus(T, Seq) :- nonvar(Seq), '$bus_tree'(T, Seq).

'$bus_forest'(t(X,F), Seq) :-
    '$bus_forest'(F, SeqF), append(SeqF, [X], Seq).
'$bus_forest'([], []).
'$bus_forest'([T|Ts], Seq) :-
    '$bus_forest'(T, SeqT), '$bus_forest'(Ts, SeqTs), append(SeqT, SeqTs, Seq).

'$bus_tree'(t(X,F) ,Seq) :-
    append(SeqF, [X], Seq), '$bus_tree_forest'(F, SeqF).

'$bus_tree_forest'([], []).
'$bus_tree_forest'([T|Ts], Seq) :-
    append(SeqT, SeqTs, Seq),
    '$bus_tree'(T, SeqT), '$bus_tree_forest'(Ts, SeqTs).

% mtree_ltl(T, L)
% Lisp-like representation of a multiway tree T
mtree_ltl(T, L) :- '$mtree_ltl'(T, L-[]).
'$mtree_ltl'(t(X,[]), [X|L]-L) :- X \= '('.
'$mtree_ltl'(t(X,[T|Ts]), ['(',X|L]-R) :- '$forest_ltl'([T|Ts], L-[')'|R]).
'$forest_ltl'([], L-L).
'$forest_ltl'([T|Ts], L-R) :- '$mtree_ltl'(T, L-M), '$forest_ltl'(Ts, M-R).
