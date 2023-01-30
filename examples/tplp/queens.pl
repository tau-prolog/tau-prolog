:- module(queens, [queens/2]).

queens(N, Qs) :-
    range(1, N, Ns),
    queens(Ns, [], Qs).
queens(UnplacedQs, SafeQs, Qs) :-
    sel(Q, UnplacedQs, UnplacedQs1),
    \+attack(Q, SafeQs),
    queens(UnplacedQs1, [Q|SafeQs], Qs).
queens([], Qs, Qs).

range(M, N, [M|Ns]) :-
    M < N,
    M1 is M+1,
    range(M1, N, Ns).
range(N, N, [N]).

sel(X, [X|Xs], Xs).
sel(X, [Y|Ys], [Y|Zs]) :-
    sel(X, Ys, Zs).

attack(X, Xs) :-
    attack(X, 1, Xs).
attack(X, N, [Y|_Ys]) :-
    X is Y+N ; X is Y-N.
attack(X, N, [_Y|Ys]) :-
    N1 is N+1,
    attack(X, N1, Ys).