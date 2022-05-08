:- module(tau, ['$member'/2, '$findall'/4, '$bagof'/3, '$setof'/3, '$if'/3]).

:- meta_predicate
    '$findall'(?, 0, -, ?),
    '$bagof'(?, ^, -),
    '$setof'(?, ^, -),
    '$if'(0, 0, 0).

%!  '$member'(?Element, ?List)
%
%   True if Element is a member of List.

'$member'(X, [X|_]).
'$member'(X, [_|Xs]) :- '$member'(X, Xs).

%!  '$findall'(+Template, :Goal, -Bag, +Tail)
%
%   True if Bag is a list of values in the form Template that would make the
%   goal Goal succeed. Tail is the tail of the difference list Bag-Tail.

'$findall'(Template0, Goal0, Instances, Tail) :-
    copy_term(Template0-Goal0, Template1-Goal1),
    call(Goal1),
    '$push_global_stack'(Var, Template1),
    false ; '$flush_global_stack'(Var, Instances, Tail).

%!  '$bagof'(+Template, :Goal, -Bag)
%
%   Unify Bag with the alternatives of Template for the goal Goal.

'$bagof'(Template, Goal0, Answer) :-
    '$free_variable_set'(Template^Goal0, Goal1, FV),
    findall(FV-Template, Goal1, Answers, []),
    keygroup(Answers, KeyGroups),
    keysort(KeyGroups, KeySorted),
    '$member'(FV-Answer, KeySorted).

%!  '$setof'(+Template, :Goal, -Bag)
%
%   Equivalent to '$bagof'/3, but sorts the result using sort/2 to get a sorted
%   list of alternatives without duplicates.

'$setof'(Template, Goal0, Answer) :-
    '$free_variable_set'(Template^Goal0, Goal1, FV),
    findall(FV-Template, Goal1, Answers, []),
    keygroup(Answers, KeyGroups),
    keysort(KeyGroups, KeySorted),
    '$member'(FV-Unsorted, KeySorted),
    sort(Unsorted, Answer).

%!  '$if'(:Condition, :Action, :Else)
%
%   This construct implements the so-called soft-cut.

'$if'(If, Then, Else) :-
    ( call(If),
      '$push_global_stack'(Stack, _),
      call(Then)
    ; '$flush_global_stack'(Stack, [], []),
      call(Else)
    ).