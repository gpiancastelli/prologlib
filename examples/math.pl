% prime(+integer)
% Determine whether a given integer number is prime. 
prime(2).
prime(3).
prime(P) :- integer(P), P > 3, P mod 2 =\= 0, not('$has_odd_factor'(P,3)).
'$has_odd_factor'(N, L) :- N mod L =:= 0.
'$has_odd_factor'(N, L) :- L * L < N, L2 is L + 2, '$has_odd_factor'(N, L2).

% gcd(+integer, +integer, ?integer)
% Determine the greatest common divisor of two positive integers.
gcd(X, 0, X) :- X > 0.
gcd(X, Y, G) :- Y > 0, Z is X mod Y, gcd(Y, Z, G).

% coprime(+integer, +integer)
% Determine whether two positive integer numbers are coprime. 
% Two numbers are coprime if their GCD equals 1.
coprime(X, Y) :- gcd(X, Y, 1).

% prime_factors(+integer, ?list)
% Determine the prime factors of a given positive integer. 
prime_factors(N, L) :- N > 0, '$prime_factors'(N, L, 2).
'$prime_factors'(1, [], _) :- !.
'$prime_factors'(N, [F|L], F) :- % N is multiple of F
    R is N // F, N =:= R * F, !, '$prime_factors'(R, L, F).
'$prime_factors'(N, L, F) :- % N is not multiple of F
    '$next_factor'(N, F, NF), '$prime_factors'(N, L, NF).
'$next_factor'(_, 2, 3) :- !.
'$next_factor'(N, F, NF) :- F * F < N, !, NF is F + 2.
'$next_factor'(N, _, N). % F > sqrt(N)

% prime_factors_with_multiplicity(+integer, ?list)
% Determine the prime factors of a given positive integer, producing 
% a list containing the prime factors and their multiplicity.
prime_factors_with_multiplicity(N, L) :- N > 0, '$prime_factors_with_multiplicity'(N, L, 2).
'$prime_factors_with_multiplicity'(1, [], _) :- !.
'$prime_factors_with_multiplicity'(N, [[F,M]|L], F) :- '$divide'(N, F, M, R), !, % F divides N
    '$next_factor'(R, F, NF), '$prime_factors_with_multiplicity'(R, L, NF).
'$prime_factors_with_multiplicity'(N, L, F) :- !, % F does not divide N
    '$next_factor'(N, F, NF), '$prime_factors_with_multiplicity'(N, L, NF).
'$divide'(N, F, M, R) :- '$divide'(N, F, M, R, 0), M > 0.
'$divide'(N, F, M, R, K) :- S is N // F, N =:= S * F, !, % F divides N
    K1 is K + 1, '$divide'(S, F, M, R, K1).
'$divide'(N, _, M, N, M).

% phi(+integer, -integer)
% Calculate Euler's totient function phi(m), defined as the number
% of positive integers r (1 <= r < m) that are coprime to m. %'
phi(N, Phi) :- prime_factors_with_multiplicity(N, L), '$phi'(L, Phi).
'$phi'([], 1).
'$phi'([[F,1]|L], Phi) :- !, '$phi'(L, Phi1), Phi is Phi1 * (F - 1).
'$phi'([[F,M]|L], Phi) :- M > 1, M1 is M - 1, '$phi'([[F,M1]|L], Phi1), Phi is Phi1 * F.

% primes(+integer, +interger, ?list)
% Given a range of integers by its lower and upper limit,
% constructs a list of all prime numbers in that range.
primes(A, B, L) :- A =< 2, !, '$primes'(2, B, L).
primes(A, B, L) :- A1 is (A // 2) * 2 + 1, '$primes'(A1, B, L).
'$primes'(A, B, []) :- A > B, !.
'$primes'(A, B, [A|L]) :- prime(A), !, '$next'(A, A1), '$primes'(A1, B, L). 
'$primes'(A, B, L) :- '$next'(A, A1), '$primes'(A1, B, L).
'$next'(2, 3) :- !.
'$next'(A, A1) :- A1 is A + 2.

% gray(N,C)
% C is the N-bit Gray code
gray(1, ['0','1']).
gray(N, C) :- N > 1, N1 is N-1,
    gray(N1, C1), reverse(C1, C2),
    '$prepend'('0', C1, C1P),
    '$prepend'('1', C2, C2P),
    append(C1P, C2P, C).
'$prepend'(_, [], []) :- !.
'$prepend'(X, [C|Cs], [CP|CPs]) :- atom_concat(X, C, CP), '$prepend'(X, Cs, CPs).
