:- module(lists, [append/3, append/2, member/2, permutation/2,
                  maplist/2, maplist/3, maplist/4, maplist/5,
                  maplist/6, maplist/7, maplist/8,
                  foldl/4, foldl/5, foldl/6, foldl/7, foldl/8,
                  foldr/4, foldr/5, foldr/6, foldr/7, foldr/8]).

:- meta_predicate
    maplist(1, ?),
    maplist(2, ?, ?),
    maplist(3, ?, ?, ?),
    maplist(4, ?, ?, ?, ?),
    maplist(5, ?, ?, ?, ?, ?),
    maplist(6, ?, ?, ?, ?, ?, ?),
    maplist(7, ?, ?, ?, ?, ?, ?, ?),
    foldl(3, +, +, -),
    foldl(4, +, +, +, -),
    foldl(5, +, +, +, +, -),
    foldl(6, +, +, +, +, +, -),
    foldl(7, +, +, +, +, +, +, -),
    foldr(3, +, +, -),
    foldr(4, +, +, +, -),
    foldr(5, +, +, +, +, -),
    foldr(6, +, +, +, +, +, -),
    foldr(7, +, +, +, +, +, +, -).

%!  append(?List1, ?List2, ?List1AndList2)
%
%   True if List1AndList2 is the concatenation of List1 and List2.

append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :-
    append(Xs, Ys, Zs).

%!  append(+ListOfLists, ?List)
%
%   True if List is the concatenation of ListOfLists.

append([], []).
append([X|Xs], Zs) :-
    append(X, Ys, Zs),
    append(Xs, Ys).

%!  member(?Element, ?List)
%
%   True if Element is a member of List.

member(X, [X|_]).
member(X, [_|Xs]) :- member(X, Xs).

%!  permutation(?List, ?Permutation)
%
%   True if Permutation is a permutation of List.

permutation([], []).
permutation([X|Xs], Ps) :-
    permutation(Xs, Qs),
    append(As, Bs, Qs),
    append(As, [X|Bs], Ps).

%!  maplist(+Goal, ?List)
%
%   True if Goal can be successfully applied to List.

maplist(_, []).
maplist(P, [A|As]) :-
    call(P, A),
    maplist(P, As).

maplist(_, [], []).
maplist(P, [A|As], [B|Bs]) :-
    call(P, A, B),
    maplist(P, As, Bs).

maplist(_, [], [], []).
maplist(P, [A|As], [B|Bs], [C|Cs]) :-
    call(P, A, B, C),
    maplist(P, As, Bs, Cs).

maplist(_, [], [], [], []).
maplist(P, [A|As], [B|Bs], [C|Cs], [D|Ds]) :-
    call(P, A, B, C, D),
    maplist(P, As, Bs, Cs, Ds).

maplist(_, [], [], [], [], []).
maplist(P, [A|As], [B|Bs], [C|Cs], [D|Ds], [E|Es]) :-
    call(P, A, B, C, D, E),
    maplist(P, As, Bs, Cs, Ds, Es).

maplist(_, [], [], [], [], [], []).
maplist(P, [A|As], [B|Bs], [C|Cs], [D|Ds], [E|Es], [F|Fs]) :-
    call(P, A, B, C, D, E, F),
    maplist(P, As, Bs, Cs, Ds, Es, Fs).

maplist(_, [], [], [], [], [], [], []).
maplist(P, [A|As], [B|Bs], [C|Cs], [D|Ds], [E|Es], [F|Fs], [G|Gs]) :-
    call(P, A, B, C, D, E, F, G),
    maplist(P, As, Bs, Cs, Ds, Es, Fs, Gs).

%!  foldl(+Goal, +List, +Start, ?Result)
%
%   True if Result is the result of applying Goal to the elements of List
%   from left to right, in a cumulative way, using Start as initial accumulator.

foldl(_, [], V0, V0).
foldl(G, [A|As], V0, V2) :-
    call(G, V0, A, V1),
    foldl(G, As, V1, V2).

foldl(_, [], [], V0, V0).
foldl(G, [A|As], [B|Bs], V0, V2) :-
    call(G, V0, A, B, V1),
    foldl(G, As, Bs, V1, V2).

foldl(_, [], [], [], V0, V0).
foldl(G, [A|As], [B|Bs], [C|Cs], V0, V2) :-
    call(G, V0, A, B, C, V1),
    foldl(G, As, Bs, Cs, V1, V2).

foldl(_, [], [], [], [], V0, V0).
foldl(G, [A|As], [B|Bs], [C|Cs], [D|Ds], V0, V2) :-
    call(G, V0, A, B, C, D, V1),
    foldl(G, As, Bs, Cs, Ds, V1, V2).

foldl(_, [], [], [], [], [], V0, V0).
foldl(G, [A|As], [B|Bs], [C|Cs], [D|Ds], [E|Es], V0, V2) :-
    call(G, V0, A, B, C, D, E, V1),
    foldl(G, As, Bs, Cs, Ds, Es, V1, V2).

%!  foldr(+Goal, +List, +Start, ?Result)
%
%   True if Result is the result of applying Goal to the elements of List
%   from right to left, in a cumulative way, using Start as initial accumulator.

foldr(_, [], V0, V0).
foldr(G, [A|As], V0, V2) :-
    foldr(G, As, V0, V1),
    call(G, A, V1, V2).

foldr(_, [], [], V0, V0).
foldr(G, [A|As], [B|Bs], V0, V2) :-
    foldr(G, As, Bs, V0, V1),
    call(G, A, B, V1, V2).

foldr(_, [], [], [], V0, V0).
foldr(G, [A|As], [B|Bs], [C|Cs], V0, V2) :-
    foldr(G, As, Bs, Cs, V0, V1),
    call(G, A, B, C, V1, V2).

foldr(_, [], [], [], [], V0, V0).
foldr(G, [A|As], [B|Bs], [C|Cs], [D|Ds], V0, V2) :-
    foldr(G, As, Bs, Cs, Ds, V0, V1),
    call(G, A, B, C, D, V1, V2).

foldr(_, [], [], [], [], [], V0, V0).
foldr(G, [A|As], [B|Bs], [C|Cs], [D|Ds], [E|Es], V0, V2) :-
    foldr(G, As, Bs, Cs, Ds, Es, V0, V1),
    call(G, A, B, C, D, E, V1, V2).