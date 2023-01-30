:- module(inorder, [inoder/2]).

inorder(Tree, List) :-
    inorder(Tree, List, []).

inorder(empty, H, H).
inorder(tree(X, L, R), Xs, Zs) :-
    inorder(L, Xs, [X|Ys]),
    inorder(R, Ys, Zs).