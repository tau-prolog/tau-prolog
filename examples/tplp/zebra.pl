:- module(zebra, [zebra/1]).

zebra(Houses) :-
    Houses = [_,_,_,_,_],
    member(house(red,english,_,_,_), Houses),
    member(house(_,spanish,dog,_,_), Houses),
    member(house(green,_,_,coffee,_), Houses),
    member(house(_,ukrainian,_,tea,_), Houses),
    right(house(green,_,_,_,_), house(ivory,_,_,_,_), Houses),
    member(house(_,_,snails,_,old_gold), Houses),
    member(house(yellow,_,_,_,kool), Houses),
    Houses = [_,_,house(_,_,_,milk,_),_,_],
    Houses = [house(_,norwegian,_,_,_)|_],
    next(house(_,_,_,_,chesterfield), house(_,_,fox,_,_), Houses),
    next(house(_,_,_,_,kool), house(_,_,horse,_,_), Houses),
    member(house(_,_,_,orange_juice,lucky_strike), Houses),
    member(house(_,japanese,_,_,parliament), Houses),
    next(house(_,norwegian,_,_,_), house(blue,_,_,_,_), Houses),
    member(house(_,_,zebra,_,_), Houses),
    member(house(_,_,_,water,_), Houses).

member(X, [X|_]).
member(X, [_|T]) :-
    member(X, T).

right(X, Y, [Y,X|_]).
right(X, Y, [_|T]) :-
    right(X, Y, T).

next(X, Y, [X,Y|_]).
next(X, Y, [Y,X|_]).
next(X, Y, [_|T]) :-
    next(X, Y, T).