:- [utils, logs].

%% penalization_list(-Penalizations:list) is det
% 
% The penalization_list/1 fact return the order of penalization values 
%
% @param Penalizations Return the penalization list
% @copyright 2kodevs 2019-2020
penalization_list([-1, -1, -2, -2, -2, -3, -3]:penalties).

%% random_strategy(-Strategy:Functor) is det
% 
% The random_strategy/1 predicate return a random play strategy 
%
% @param Strategy Functor of one strategy
% @copyright 2kodevs 2019-2020
random_strategy(S) :-
    random_permutation([basic], [S|_]).    

%% line_score(+List:list, +Tile:point, -Score:int) is det
% 
% The line_score/3 predicate find the score of add Tile to a line
%
% @param List Line where Tile is being added
% @param Tile New tile
% @param Score Number of tiles adyacent to Tile
% @copyright 2kodevs 2019-2020
line_score(List, Tile, Score) :-
    make_intervals(List, Interval),
    findall(X,
            ( member(X, Interval),
              member(Tile, X)
            ),
            [Adyacents]),
    length(Adyacents, Score).
    
%% tile_score(+Player:Player, +Tile:point, -Score:int) is det
% 
% The tile_score/3 predicate calculate the score of add Tile to the
% player Wall
%
% @param Player Target
% @param Tile New tile
% @param Score Score related to Tile
% @copyright 2kodevs 2019-2020
tile_score(Player,  (Row, Column), Score) :-
    property_of(table, Player, Table),
    concat(Table, [(Row, Column)], NewTable),
    % row score
    line_score(NewTable,  (Row, Column), RowScore),
    invert_axis(NewTable, InvertedAxis),
    % column score
    line_score(InvertedAxis,  (Column, Row), ColumnScore),
    Score is RowScore+ColumnScore.

%% column_of(+Line:Line, +Color, -Column:int) is det
% 
% The column_of/3 predicate given a line and a color return the
% column on the player Wall
%
% @param Line Player line
% @param Color Tile color
% @param Column The column that represent this color on Line
% @copyright 2kodevs 2019-2020
column_of(Line, Color, Column) :-
    tiles_colors(Colors),
    index_of(Color, Colors, Idx),
    Column is (Idx+Line-1)mod 5+1.

%% valid_choices(+Game:Game, +Player:Player, -Choices:list) is det
% 
% The valid_choices/3 predicate given a Game and a Player find all
% choices that aument the tiles in the pattern lines of Player
%
% @param Game A running Game
% @param Player Target
% @param Choices All the player possible valid selections
% @copyright 2kodevs 2019-2020
valid_choices(Game, Player, Choices) :-
    property_of(factories, Game, Fac),
    property_of(board, Player, Board),
    findall(Lid:Fid:Color,
            ( property_of(Lid, Board, Line),
              property_of(stocks, Line, Stocks),
              member(empty, Stocks),
              property_of(valid, Line, ValidColors),
              property_of(Fid, Fac, CurFac),
              member(Color, ValidColors),
              member(Color, CurFac)
            ),
            Choices).

%% clean_line(+Player:Player, +LineId:int, -NewPlayer:Player) is semidet
% 
% The clean_line/3 predicate if the line numbered LineId of the player 
% pattern lines is full, it becomes an empty line.
%
% @param Player Target
% @param Line Patter line id
% @param NewPlayer Updated player
% @copyright 2kodevs 2019-2020
clean_line(Player, L, NewPlayer) :-
    property_of(board, Player, Board),
    property_of(L, Board, Line),
    property_of(all, Line, Colors),
    property_of(valid, Line, [C]),
    property_of(stocks, Line, CurStocks),
    % cheking that the line is full
    add([], L, C, CurStocks),
    concat(A, [C|B], Colors),
    concat(A, B, List),
    set_prop_to(all, Line, List, TempLine0),
    set_prop_to(valid, TempLine0, List, TempLine1),
    % cleaning the line
    add([], L, empty, Stocks),
    set_prop_to(stocks, TempLine1, Stocks, TempLine2),
    set_prop_to(L, Board, TempLine2, NewBoard),
    set_prop_to(board, Player, NewBoard, NewPlayer).

%% update_score(+Player:Player, +Tile:point, -NewPlayer:Player, -ReturnedTiles:int) is det
% 
% The update_score/4 predicate update the player score after try to add Tile
% to his Wall.
%
% @param Player Target
% @param Tile New tile attempt
% @param NewPlayer Updated player
% @param ReturnedTiles Number of tiles that get out of the game
% @copyright 2kodevs 2019-2020
update_score(Player,  (L, C), NewPlayer, Return) :-
    property_of(board, Player, Board),
    property_of(L, Board, Line),
    property_of(stocks, Line, Stocks),
    count(Stocks, empty, 0), !,
    info_log(["Player fulfilled the row ", L, " of his pattern lines"]),
    Return is L-1,
    tile_score(Player,  (L, C), Score),
    property_of(score, Player, PScore),
    Sum is Score+PScore,
    update_table(Player,  (L, C), CurPlayer),
    set_prop_to(score, CurPlayer, Sum, NewPlayer).
update_score(P, _, P, 0).

%% update_line(+Player:Player, +Game:Game, +Selection, -NewPlayer:Player, -ReturnedTiles:int) is det
% 
% The update_line/5 predicate update the pattern line of Player. The id of the line is 
% given on selection in the form <L:F:Color> where L is the line Id, F is a factory Id, and
% Color is the color selected from F. Update the line following the game rules.
%
% @param Player Target
% @param Game Current Game
% @param Selection Tuple Line:Factory:Color
% @param NewPlayer Updated player
% @param ReturnedTiles Number of tiles that get out of the game by overflow of the stock size
% @copyright 2kodevs 2019-2020
update_line(Player, Game, L:F:Color, NewPlayer, Diff) :-
    property_of(factories, Game, Factories),
    property_of(F, Factories, Fac),
    property_of(board, Player, Board),
    property_of(L, Board, Line),
    property_of(stocks, Line, Stocks),
    count(Stocks, empty, Empty),
    count(Fac, Color, Amount),
    Diff is min(Empty-Amount, 0),
    replace(Stocks, Amount, empty, Color, NewStocks),
    debug_log(["Player new pattern line ", L, " is -> ", NewStocks]),
    set_prop_to(stocks, Line, NewStocks, NewLine),
    set_prop_to(valid, NewLine, [Color], ValidLine),
    set_prop_to(L, Board, ValidLine, NewBoard),
    set_prop_to(board, Player, NewBoard, NewPlayer).

%% update_table(+Player:Player, +Tile:point, -NewPlayer:Player) is det
% 
% The update_table/3 predicate add a tile to the player board
%
% @param Player Target
% @param Tile New acquire tile
% @param NewPlayer Updated player
% @copyright 2kodevs 2019-2020
update_table(Player, Tile, NewPlayer) :-
    debug_log(["Adding <", Tile, "> to the player table"]),
    property_of(table, Player, Table),
    add(Table, 1, Tile, NewTable),
    set_prop_to(table, Player, NewTable, NewPlayer).

%% penalize(+Player:Player, +Amount:int, -NewPlayer:Player) is det
% 
% The penalize/3 predicate add an Amount number of penalizations to Player using
% its penalization property.
%
% @param Player Target
% @param Amount Number of penalizations
% @param NewPlayer Updated player
% @copyright 2kodevs 2019-2020
penalize(Player, Amount, NewPlayer) :-
    Amount<0,
    property_of(penalties, Player, Penalties),
    length(Penalties, Sz),
    Sz>0, !,
    concat([P1], R, Penalties),
    debug_log(["Player recive ", P1, " of penalization"]),
    set_prop_to(penalties, Player, R, TempPlayer1),
    property_of(score, Player, Score),
    NewScore is max(Score+P1, 0),
    set_prop_to(score, TempPlayer1, NewScore, TempPlayer2),
    Times is Amount+1,
    penalize(TempPlayer2, Times, NewPlayer).
penalize(Player, _, Player).    

%% update_player(+Player:Player, +Game:Game, +Selection, -NewPlayer:Player, -ReturnedTiles:int) is det
% 
% The update_player/5 predicate update all the player information after a new choice
%
% @param Player Target
% @param Game Current Game
% @param Selection Tuple Line:Factory:Color
% @param NewPlayer Updated player
% @param ReturnedTiles Number of tiles that get out of the game
% @copyright 2kodevs 2019-2020
update_player(Player, Game, L:F:Color, NewPlayer, Return) :-
    update_line(Player,
                Game,
                L:F:Color,
                TempPlayer0,
                Diff),
    column_of(L, Color, C),
    update_score(TempPlayer0,  (L, C), TempPlayer1, Amount),
    Return is Amount-Diff,
    penalize(TempPlayer1, Diff, NewPlayer).

%% update_game(+Game:Game, +Selection, -NewGame:Game, -ReturnedTiles:int) is det
% 
% The update_game/4 predicate update all the game information after a player turn
%
% @param Game Current Game
% @param Selection Tuple Line:Factory:Color
% @param NewGame Updated game
% @param ReturnedTiles Number of tiles that get out of the game
% @copyright 2kodevs 2019-2020
update_game(Game, _:F:C, NewGame, ReturnedTiles) :-
    property_of(factories, Game, GameFac),
    property_of(F, GameFac, Fac),
    findall(X,
            ( member(X, Fac),
              not(member(X, [empty, C]))
            ),
            ToCenter),
    add([], 4, empty, NewFac),
    set_prop_to(F, GameFac, NewFac, TempFacs),
    property_of(center, TempFacs, Center),
    concat(ToCenter, Center, NewCenter),
    set_prop_to(center, TempFacs, NewCenter, NewFacs),
    debug_log(["New factories center is -> ", NewCenter]),
    set_prop_to(factories, Game, NewFacs, Temp),
    property_of(outs, Game, Outs),
    property_of(C, Outs, Number),
    Sum is Number+ReturnedTiles,
    set_prop_to(C, Outs, Sum, NewOuts),
    set_prop_to(outs, Temp, NewOuts, NewGame).

%% basic(+Game:Game, +Player:Player, -NewGame:Game, -NewPlayer:Player, -Selection:int) is det
% 
% The basic/5 predicate is a player strategy. At the beginning find all the valid choices
% and take the first. If its not possible to choose any tile, and if there is no tile left in the
% factories, skip the turn. 
%
% @param Game Current Game
% @param Player Current Player
% @param NewGame Updated game
% @param NewPlayer Updated Player
% @param Selection Player choice Line:Factory:Color
% @copyright 2kodevs 2019-2020
basic(Game, Player, NewGame, NewPlayer, A) :-
    valid_choices(Game, Player, [A|_]), !,
    update_player(Player, Game, A, NewPlayer, Return),
    update_game(Game, A, NewGame, Return).
basic(Game, Player, NewGame, NewPlayer, none:Id:Color) :-
    property_of(factories, Game, Factories),
    property_of(Id, Factories, F),
    count(F, empty, C),
    not(length(F, C)),
    member(Color, F),
    Color\=empty, !,
    count(F, Color, Amount),
    update_game(Game, none:Id:Color, NewGame, Amount),
    Neg is Amount* -1,
    penalize(Player, Neg, NewPlayer).
basic(Game, Player, Game, Player, none:none:none).

%% empty_board(+Board:Board) is det
% 
% The empty_board/1 predicate create a new player board
%
% @param Board The new board
% @copyright 2kodevs 2019-2020
empty_board(Data:board) :-
    add([], 5, 1, List),
    enumerate(List, 1, Enum),
    tiles_colors(C),
    findall([New:stocks, C:valid, C:all]:Sz,
            ( property_of(Sz, Enum, _),
              add([], Sz, empty, New)
            ),
            Data).

%% new_players(+Amount:int, -Players:Indexed-list) is det
% 
% The new_players/2 predicate create an Amount of new players
%
% @param Amount Number of players to return
% @param Players Indexed list of players
% @copyright 2kodevs 2019-2020
new_players(Amount, Players:players) :-
    empty_board(Board),
    penalization_list(Penalties),
    add([], Amount, [Board, Penalties, []:table, 0:score], List),
    findall(P,
            ( member(X, List),
              random_strategy(S),
              set_prop_to(strategy, X, S, P)
            ),
            RawPlayers),
    enumerate(RawPlayers, 1, Players).

%% run_round(+Game:Game, +Players:Indexed-list, -NewGame:Game, -Events:list) is det
% 
% The run_round/4 predicate run a turn for each player and return the list of 
% events relative to each one of their choices.
%
% @param Game Current game
% @param Players Indexed list of players in the round order
% @copyright 2kodevs 2019-2020
run_round(G, [], G, []).
run_round(Game, [P1:Id|Players], NewGame, [Id:Fid|Events]) :-
    property_of(strategy, P1, St),
    info_log(["Player ", Id, " turn start --------------------"]),
    Choice=..[St, Game, P1, TempGame1, NewP1, Lid:Fid:Color],
    Choice,
    info_log([
        "Player chose all color ", 
        Color, " from factory ",
        Fid, " and add them to the ",
        Lid, " line"
    ]),
    info_log([NewP1:pattern, "\n----------------------------------------------"]),
    property_of(players, TempGame1, OldPlayers),
    set_prop_to(Id, OldPlayers, NewP1, CurPlayers),
    set_prop_to(players, TempGame1, CurPlayers, TempGame2),
    run_round(TempGame2, Players, NewGame, Events).

%% clean_players(+Game:Game, -NewGame:Game) is det
% 
% The clean_players/2 predicate clean the pattern lines fulfilled of each player.
%
% @param Game Current game
% @param NewGame Updated Game
% @copyright 2kodevs 2019-2020
clean_players(Game, NewGame) :-
    property_of(players, Game, Players),
    findall(Player:Id,
            ( member(X:Id, Players),
              property_of(board, X, Board),
              verify_lines(X, Board, CleanedPlayer),
              info_log([CleanedPlayer:player]),
              penalization_list(Penalizations),
              set_prop_to(penalization, CleanedPlayer, Penalizations, Player)
            ),
            NewPlayers),
    set_prop_to(players, Game, NewPlayers, NewGame).

%% verify_lines(+Player:Player, +Lines:Indexed-Lst, -NewPlayer:Player) is det
% 
% The verify_lines/3 predicate clean fulfilled lines in Lines belonging to Player.
%
% @param Player Current Player
% @param Lines Lines to clean
% @param NewPlayer Updated Player
% @copyright 2kodevs 2019-2020
verify_lines(P, [], P).
verify_lines(Player, [_:Line|Lines], NewPlayer) :-
    clean_line(Player, Line, CurPlayer), !,
    verify_lines(CurPlayer, Lines, NewPlayer).
verify_lines(Player, [_|Lines], NewPlayer) :-
    verify_lines(Player, Lines, NewPlayer).
