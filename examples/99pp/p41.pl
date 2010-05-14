% P41 (*) A list of Goldbach compositions. 
% Given a range of integers by its lower and upper limit, 
% print a list of all even numbers and their Goldbach composition.

% :- ensure_loaded(p40).
% P40 (**) Goldbach's conjecture. 
% Goldbach's conjecture says that every positive even number greater 
% than 2 is the sum of two prime numbers. Example: 28 = 5 + 23.

% :- ensure_loaded(p31).
is_prime(2).
is_prime(3).
is_prime(P) :- integer(P), P > 3, P mod 2 =\= 0, \+ has_factor(P,3).  

% has_factor(N,L) :- N has an odd factor F >= L.
%    (integer, integer) (+,+)

has_factor(N,L) :- N mod L =:= 0.
has_factor(N,L) :- L * L < N, L2 is L + 2, has_factor(N,L2).

% goldbach(N,L) :- L is the list of the two prime numbers that
%    sum up to the given N (which must be even).
%    (integer,integer) (+,-)

goldbach(4,[2,2]) :- !.
goldbach(N,L) :- N mod 2 =:= 0, N > 4, goldbach(N,L,3).

goldbach(N,[P,Q],P) :- Q is N - P, is_prime(Q), !.
goldbach(N,L,P) :- P < N, next_prime(P,P1), goldbach(N,L,P1).

next_prime(P,P1) :- P1 is P + 2, is_prime(P1), !.
next_prime(P,P1) :- P2 is P + 2, next_prime(P2,P1).

% goldbach_list(A,B) :- print a list of the Goldbach composition
%    of all even numbers N in the range A <= N <= B
%    (integer,integer) (+,+)

goldbach_list(A,B) :- goldbach_list(A,B,2).

% goldbach_list(A,B,L) :- perform goldbach_list(A,B), but suppress
% all output when the first prime number is less than the limit L.

goldbach_list(A,B,L) :- A =< 4, !, g_list(4,B,L).
goldbach_list(A,B,L) :- A1 is ((A+1) // 2) * 2, g_list(A1,B,L).

g_list(A,B,_) :- A > B, !.
g_list(A,B,L) :- 
   goldbach(A,[P,Q]),
   print_goldbach(A,P,Q,L),
   A2 is A + 2,
   g_list(A2,B,L).

print_goldbach(A,P,Q,L) :- P >= L, !,
%   writef('%t = %t + %t',[A,P,Q]), nl.
    write(A), write(' = '), write(P), write(' + '), write(Q), nl.
print_goldbach(_,_,_,_).

