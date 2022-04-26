:- module(charsio, [write_to_chars/2,
                    writeq_to_chars/2,
                    write_canonical_to_chars/2]).

%!  write_to_chars(+Term, ?Chars)
%
%   Write a term to a code list.

write_to_chars(Term, Chars) :-
    write_term_to_chars(
        Term,
        [quoted(false), ignore_ops(false), numbervars(true)],
        Chars
    ).

%!  writeq_to_chars(+Term, ?Chars)
%
%   Write a term to a code list, using brackets and operators where appropriate.
%   Atoms that need quotes are quoted.

writeq_to_chars(Term, Chars) :-
    write_term_to_chars(
        Term,
        [quoted(true), ignore_ops(false), numbervars(true)],
        Chars
    ).

%!  write_canonical_to_chars(+Term, ?Chars)
%
%   Write a term to a code list, using standard parenthesised prefix notation.
%   Atoms that need quotes are quoted.

write_canonical_to_chars(Term, Chars) :-
    write_term_to_chars(
        Term,
        [quoted(true), ignore_ops(true), numbervars(false)],
        Chars
    ).

%!  fabricate_var_name(+Type, ?Name, +Number)
%
%   From Scryer Prolog:
%   https://github.com/mthom/scryer-prolog/blob/2d19243b3bac3cbad6679d6254cd7b6a2e6d1d4e/src/lib/charsio.pl

fabricate_var_name(VarType, VarName, N) :-
    char_code('A', AC),
    LN is N mod 26 + AC,
    char_code(LC, LN),
    NN is N // 26,
    (  NN =:= 0 ->
       (  VarType == fabricated ->
          atom_chars(VarName, ['_', LC])
       ;  VarType == numbervars ->
          atom_chars(VarName, [LC])
       )
    ;  number_chars(NN, NNChars),
       (  VarType == fabricated ->
          atom_chars(VarName, ['_', LC | NNChars])
       ;  VarType == numbervars ->
          atom_chars(VarName, [LC | NNChars])
       )
    ).