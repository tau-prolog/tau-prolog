:- module(charsio, []).

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