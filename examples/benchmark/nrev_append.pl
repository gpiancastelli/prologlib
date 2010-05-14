% Test with: nrev([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30], R).

nrev([],[]).
nrev([X|Rest],Ans) :- nrev(Rest,L), append(L,[X],Ans).

append([],L2,L2).
append([E|T1],L2,[E|T2]) :- append(T1,L2,T2).