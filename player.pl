:- [utils, game].

column_of(Line, Color, Column):-
    tiles_colors(Colors),
    index_of(Color, Colors, Idx),
    Column is ((Idx + Line - 1) mod 5) + 1.

valid_choices(Game, Player, C):-
    property_of(factories, Game, Fac),
    property_of(board, Player, Board),
    findall(Lid:Fid:Color, (
        property_of(Lid, Board, Line),
        property_of(stocks, Line, Stocks),
        member(empty, Stocks),
        property_of(valid, Line, ValidColors),
        property_of(Fid, Fac, CurFac),
        member(Color, ValidColors),
        member(Color, CurFac)
    ), C).
