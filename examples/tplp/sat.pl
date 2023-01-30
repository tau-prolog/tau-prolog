% unsat([[not(2),not(3),not(4)],[not(1),not(3),not(4)],[not(1),not(2),not(4)],[not(1),not(2),not(3)],[not(6),not(7),not(8)],[not(5),not(7),not(8)],[not(5),not(6),not(8)],[not(5),not(6),not(7)],[not(10),not(11),not(12)],[not(9),not(11),not(12)],[not(9),not(10),not(12)],[not(9),not(10),not(11)],[not(13),not(14),not(15)],[not(13),not(14),not(16)],[not(13),not(14),not(17)],[not(13),not(15),not(16)],[not(13),not(15),not(17)],[not(13),not(16),not(17)],[not(14),not(15),not(16)],[not(14),not(15),not(17)],[not(14),not(16),not(17)],[not(15),not(16),not(17)],[14,10,5],[4,10,5],[4,14,5],[4,14,10],[12,1,16],[6,1,16],[6,12,16],[6,12,1],[11,7,15],[3,7,15],[3,11,15],[3,11,7],[8,2,13],[8,2,17],[8,2,9],[8,13,17],[8,13,9],[8,17,9],[2,13,17],[2,13,9],[2,17,9],[13,17,9]]).

:- module(sat, [sat/2]).

sat([], []).
sat([C|Cs], [(L,V)|Is]) :-
    C = [X|_],
    (X = not(L) -> true ; X = L),
    (V = true ; V = false),
    apply(L, V, [C|Cs], F),
    sat(F, Is).

literal(L, true, [L|Cs], Cs) :- !.
literal(L, false, [not(L)|Cs], Cs) :- !.
literal(L, V, [H|Cs], [H|Rest]) :- literal(L, V, Cs, Rest).

apply(_, _, [], []).
apply(L, V, [X|Xs], Zs) :-
    literal(L, S, X, Y), !,
    (S = V ->
        Zs = Ys;
        Y \= [],
        Zs = [Y|Ys]),
    apply(L, V, Xs, Ys).
apply(L, V, [X|Xs], [X|Ys]) :-
    apply(L, V, Xs, Ys).