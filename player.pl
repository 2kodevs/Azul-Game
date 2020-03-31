:- [utils, game].

column_of(Line, Color, Column):-
    tiles_colors(Colors),
    index_of(Color, Colors, Idx),
    Column is ((Idx + Line - 1) mod 5) + 1.
