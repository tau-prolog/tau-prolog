:- module(peano, [nat/2, add/3, mul/3, pow/3]).

nat(0, z).
nat(N, s(X)) :-
    N > 0,
    M is N-1,
    nat(M, X).

add(z, X, X).
add(s(X), Y, s(Z)) :-
    add(X, Y, Z).

mul(z, _, z).
mul(s(X), Y, W) :-
    mul(X, Y, Z),
    add(Y, Z, W).

pow(_, z, s(z)).
pow(X, s(Y), W) :-
    pow(X, Y, Z),
    mul(X, Z, W).