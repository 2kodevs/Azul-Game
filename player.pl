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

clean_line(Player, L, NewPlayer):-
    property_of(board, Player, Board),
    property_of(L, Board, Line),
    property_of(all, Line, Colors),
    property_of(valid, Line, [C]),
    concat(A, [C | B], Colors),
    concat(A, B, List),
    set_prop_to(all, Line, List, TempLine0),
    set_prop_to(valid, TempLine0, List, TempLine1),
    add([], L, empty, Stocks),
    set_prop_to(stocks, TempLine1, Stocks, TempLine2),
    set_prop_to(L, Board, TempLine2, NewBoard),
    set_prop_to(board, Player, NewBoard, NewPlayer).
