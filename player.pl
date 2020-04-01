:- [utils].

penalization_list([-1, -1, -2, -2, -2, -3, -3]:penalties).

% Update the list inside the method
% to add more strategies.
random_strategy(S):-
    random_permutation(
        [basic],
    [S | _]).    

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
    property_of(stocks, Line, CurStocks),
    add([], L, C, CurStocks),
    concat(A, [C | B], Colors),
    concat(A, B, List),
    set_prop_to(all, Line, List, TempLine0),
    set_prop_to(valid, TempLine0, List, TempLine1),
    add([], L, empty, Stocks),
    set_prop_to(stocks, TempLine1, Stocks, TempLine2),
    set_prop_to(L, Board, TempLine2, NewBoard),
    set_prop_to(board, Player, NewBoard, NewPlayer).

update_score(Player, (L, C), NewPlayer, Return):-
    property_of(board, Player, Board),
    property_of(L, Board, Line),
    property_of(stocks, Line, Stocks),
    count(Stocks, empty, 0), !,
    Return is L - 1,
    tile_score(Player, (L, C), Score),
    property_of(score, Player, PScore),
    Sum is Score + PScore,
    update_table(Player, (L, C), CurPlayer),
    set_prop_to(score, CurPlayer, Sum, NewPlayer).
update_score(P, _, P, 0).

update_line(Player, Game, L:F:Color, NewPlayer, Diff):-    
    property_of(factories, Game, Factories),
    property_of(F, Factories, Fac),
    property_of(board, Player, Board),
    property_of(L, Board, Line),
    property_of(stocks, Line, Stocks),
    count(Stocks, empty, Empty),
    count(Fac, Color, Amount), 
    Diff is min(Empty - Amount, 0),
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
    NewScore is max(Score + P1, 0),
    set_prop_to(score, TempPlayer1, NewScore, TempPlayer2),
    Times is Amount + 1,
    penalize(TempPlayer2, Times, NewPlayer).
penalize(Player, _, Player).    

update_player(Player, Game, L:F:Color, NewPlayer, Return):-
    update_line(Player, Game, L:F:Color, TempPlayer0, Diff),   
    column_of(L, Color, C),
    update_score(TempPlayer0, (L, C), TempPlayer1, Amount),
    Return is Amount - Diff,
    penalize(TempPlayer1, Diff, NewPlayer).

update_game(Game, _:F:C, NewGame, ReturnedTiles):-
    property_of(factories, Game, GameFac),
    property_of(F, GameFac, Fac),
    replace(Fac, 4, C, empty, NewFac),
    set_prop_to(F, GameFac, NewFac, NewFacs),
    set_prop_to(factories, Game, NewFacs, Temp),
    property_of(outs, Game, Outs),
    property_of(C, Outs, Number),
    Sum is Number + ReturnedTiles,
    set_prop_to(C, Outs, Sum, NewOuts),
    set_prop_to(outs, Temp, NewOuts, NewGame).

basic(Game, Player, NewGame, NewPlayer):-
    valid_choices(Game, Player, [A | _]), !,
    update_player(Player, Game, A, NewPlayer, Return),
    update_game(Game, A, NewGame, Return).
basic(Game, Player, NewGame, NewPlayer):-
    property_of(factories, Game, Factories),
    property_of(Id, Factories, F),
    count(F, empty, C), C \= 4,
    member(Color, F), Color \= empty, !,
    count(F, Color, Amount),
    update_game(Game, 1:Id:Color, NewGame, Amount),
    Neg is Amount * -1,
    penalize(Player, Neg, NewPlayer).
basic(Game, Player, Game, Player).

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
        [Board, Penalties, []:table, 0:score],
    List),
    findall(P, (
        member(X, List),
        random_strategy(S),
        set_prop_to(strategy, X, S, P)    
    ), RawPlayers),
    enumerate(RawPlayers, 1, Players).

run_round(G, [], G).
run_round(Game, [P1:Id | Players], NewGame):-
    property_of(strategy, P1, St),
    Choice =.. [St, Game, P1, TempGame1, NewP1],
    Choice,
    property_of(players, TempGame1, OldPlayers),
    set_prop_to(Id, OldPlayers, NewP1, CurPlayers),
    set_prop_to(players, TempGame1, CurPlayers, TempGame2),    
    run_round(TempGame2, Players, NewGame).

clean_players(Game, NewGame):-
    property_of(players, Game, Players),
    findall(Player:Id, (
        member(X:Id, Players),
        property_of(board, X, Board),
        verify_lines(X, Board, Player)
    ), NewPlayers),
    set_prop_to(players, Game, NewPlayers, NewGame).

verify_lines(P, [], P).
verify_lines(Player, [_:Line | Lines], NewPlayer):-
    clean_line(Player, Line, CurPlayer), !,
    verify_lines(CurPlayer, Lines, NewPlayer).
verify_lines(Player, [_ | Lines], NewPlayer):-
    verify_lines(Player, Lines, NewPlayer).
