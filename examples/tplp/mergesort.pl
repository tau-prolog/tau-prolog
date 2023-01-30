:- module(mergesort, [mergesort/2]).

mergesort([], []).
mergesort([X], [X]).
mergesort([X,Y|Xs], Sorted) :-
    split([X,Y|Xs], Left, Right),
    mergesort(Left, Ls),
    mergesort(Right, Rs),
    merge(Ls, Rs, Sorted).

split([], [], []).
split([X], [X], []).
split([X,Y|T], [X|Xs], [Y|Ys]) :-
    split(T, Xs, Ys).

merge([], Ys, Ys).
merge(Xs, [], Xs).
merge([X|Xs], [Y|Ys], [X|Zs]) :-
    X =< Y,
    merge(Xs, [Y|Ys], Zs).
merge([X|Xs], [Y|Ys], [Y|Zs]) :-
    X > Y,
    merge([X|Xs], Ys, Zs).