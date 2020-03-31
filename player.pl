:- [utils].

penalization_list([-1, -1, -2, -2, -2, -3, -3]:penalties).

line_score(L, Tile, S):-
    make_intervals(L, I),
    findall(X, (
        member(X, I),
        member(Tile, X)    
    ), [B]),
    length(B, S).
    
tile_score(P, (X, Y), S):-
    property_of(table, P, T),
    concat(T, [(X, Y)], N),
    line_score(N, (X, Y), RS),
    invert_axis(N, RN),
    line_score(RN, (Y, X), CS),
    S is RS + CS.

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

update_score(Player, (L, C), NewPlayer):-
    property_of(board, Player, Board),
    property_of(L, Board, Line),
    property_of(stocks, Line, Stocks),
    count(Stocks, empty, 0), !,
    tile_score(Player, (L, C), Score),
    property_of(score, Player, PScore),
    Sum is Score + PScore,
    set_prop_to(score, Player, Sum, NewPlayer).
update_score(P, _, _, P).

update_line(Player, Game, L:F:Color, NewPlayer, Dif):-    
    property_of(factories, Game, Factories),
    property_of(F, Factories, Fac),
    property_of(board, Player, Board),
    property_of(L, Board, Line),
    property_of(stocks, Line, Stocks),
    count(Stocks, empty, Empty),
    count(Fac, Color, Amount), 
    Dif is Empty - Amount,
    replace(Stocks, Amount, empty, Color, NewStocks),
    set_prop_to(stocks, Line, NewStocks, NewLine),
    set_prop_to(valid, NewLine, [Color], ValidLine),
    set_prop_to(L, Board, ValidLine, NewBoard),
    set_prop_to(board, Player, NewBoard, NewPlayer).

update_table(Player, Tile, NewPlayer):-
    property_of(table, Player, Table),
    add(Table, 1, Tile, NewTable),
    set_prop_to(table, Player, NewTable, NewPlayer).

penalize(Player, Amount, NewPlayer):-
    Amount < 0,
    property_of(penalties, Player, Penalties),
    length(Penalties, Sz),
    Sz > 0, !,
    concat([P1], R, Penalties),
    set_prop_to(penalties, Player, R, TempPlayer1),
    property_of(score, Player, Score),
    Sum is Score + P1,
    max(Sum, 0, NewScore),
    set_prop_to(score, TempPlayer1, NewScore, TempPlayer2),
    Times is Amount + 1,
    penalize(TempPlayer2, Times, NewPlayer).
penalize(Player, _, Player).    

update_player(Player, Game, L:F:Color, NewPlayer):-
    update_line(Player, Game, L:F:Color, TempPlayer0, Diff),   
    column_of(L, Color, C),
    update_score(TempPlayer0, (L, C), TempPlayer1),
    update_table(TempPlayer1, (L, C), TempPlayer2),
    penalize(TempPlayer2, Diff, NewPlayer).

update_game(Game, _:F:C, NewGame):-
    property_of(factories, Game, GameFac),
    property_of(F, GameFac, Fac),
    replace(Fac, 4, C, empty, NewFac),
    set_prop_to(F, GameFac, NewFac, NewFacs),
    set_prop_to(factories, Game, NewFacs, NewGame).

basic(Game, Player, NewGame, NewPlayer):-
    valid_choices(Game, Player, [A | _]),
    update_player(Player, Game, A, NewPlayer),
    update_game(Game, A, NewGame).

empty_board(Data:board):-
    add([], 5, 1, List),
    enumerate(List, 1, Enum),
    tiles_colors(C),
    findall([New:stocks, C:valid, C:all]:Sz, (
        property_of(Sz, Enum, _),
        add([], Sz, empty, New)    
    ), Data).

new_players(Amount, Players:players):-
    empty_board(Board),
    penalization_list(Penalties),
    add([], Amount, 
        [Board, Penalties, []:table, []:score],
    Players).