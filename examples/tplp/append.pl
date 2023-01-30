:- module(append, [append/3]).

append([], X, X).
append([H|T], X, [H|S]) :-
    append(T, X, S).