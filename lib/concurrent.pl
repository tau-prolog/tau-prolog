:- module(concurrent, [future_all/2]).

%!  future_all(+Futures, -All)
%
%   Returns a single Future that resolves to a list of the results of the
%   input futures.
%   This is not the current implementation in the compiled version of the
%   concurrent library, since it cannot detect the premature failure of a
%   future.

/* 

:- use_module(library(os)).
:- use_module(library(concurrent)).

top(X) :-
	future(a, sleep(2000), F1),
    future(b, throw(b),    F2),
    future(c, sleep(1000), F3),
    future_all([F1,F2,F3], F),
    await(F, X),
    write(Time).

?- top(X).
uncaught exception: b

% The Prolog implementation reports the error after 2 seconds.
% The implementation in the compiled version reports the failure immediately.

*/

future_all([], All) :-
    future([], true, All).
future_all([F|Fs], All) :-
    future([X|Xs],
        ( await(F, X),
          future_all(Fs, Rest),
          await(Rest, Xs)
        ), All).