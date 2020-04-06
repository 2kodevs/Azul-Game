:- [player, logs].
:- (dynamic initial_player/1).

%% initial_player(-Id:int) is det
% 
% The initial_player/1 fact store the id 
% of the player who is the first to play in the current
% round.
%
% @param Id return the first player Id
% @copyright 2kodevs 2019-2020
initial_player(1).

%% use_fac(+EmptyFactories:list, +Tiles:list, -FullFactories) is det
% 
% The use_fac/3 predicate given a set of factories  
% fill each one following the order of the
% tiles which is also given.
%
% @param EmptyFactories A list compoused ideally with [[empty, ...], [empty, ...], ...], 
% representing the empty factories
% @param Colors A list of tiles to add to the factories
% @param FullFactories The result of put the Tiles into the factories
% @copyright 2kodevs 2019-2020
use_fac([], _, []).
use_fac(Factories, [], Factories).
use_fac([[]|Factories], Tiles, [[]|Result]) :-
    use_fac(Factories, Tiles, Result).
use_fac([[_|Fac1]|Factories], [Tile1|Tiles], [[Tile1|Res1]|Result]) :-
    use_fac([Fac1|Factories], Tiles, [Res1|Result]).

%% populate(+Game:Game, -NewGame:Game) is det
% 
% The populate/2 predicate try to ensure that all
% the factories in Game will be full at the beginning
% ot the next round.
%
% @param Game Current Game state
% @param NewGame The new Game with more or the same amount of tile in the bag
% @copyright 2kodevs 2019-2020
populate(Game, NewGame) :-
    property_of(amounts, Game, Amounts),
    property_of(factories, Game, Factories),
    length(Factories, FacSz),
    findall(X, member(X:_, Amounts), Quantities),
    sum_list(Quantities, Sum), 
    % check if the tiles could full the factories
    Sum<FacSz*4, !,
    property_of(outs, Game, Outs),
    debug_log(["Adding more tiles to the bag.\n\t", Amounts:amounts, "\n\t", Outs:outs]),
    % adding tiles to the bag
    findall(RealAmount:Color,
            ( property_of(Color, Amounts, QAmount),
              property_of(Color, Outs, QOut),
              RealAmount is QOut+QAmount
            ),
            NewAmounts),
    set_prop_to(amounts, Game, NewAmounts, TempGame),
    % Saving that 0 tiles are out
    findall(0:Color, member(_:Color, Outs), NewOuts),
    set_prop_to(outs, TempGame, NewOuts, NewGame).
populate(Game, Game).

%% new_round(+Game:Game, -NewGame:Game) is multi
% 
% The new_round/2 predicate prepare the Game before the start of a new round.
%
% @param Game Current Game state
% @param NewGame The new Game ready to run a new round
% @copyright 2kodevs 2019-2020
new_round(Game, NewGame) :-
    debug_log(["Prepairing a new round"]),
    % Check if more tiles are needed
    populate(Game, TempGame1),
    %Select the random tiles to add
    property_of(amounts, TempGame1, Amounts),
    findall(List,
            ( property_of(Color, Amounts, Quantity),
              add([], Quantity, Color, List)
            ),
            ColorGroups),
    concat_all(ColorGroups, ColorsList),
    random_permutation(ColorsList, ColorsOrder),
    % Saving the selected tiles
    property_of(factories, TempGame1, GameFac),
    remove_prop(center, GameFac, SimpleFac),
    findall(Fac, member(Fac:_, SimpleFac), RawFac),
    use_fac(RawFac, ColorsOrder, TempFac),
    concat_all(TempFac, UsedTiles),
    % Update the amount of tiles, and create the new game
    findall(NewQ:Color,
            ( property_of(Color, Amounts, QOld),
              count(UsedTiles, Color, Used),
              NewQ is QOld-Used
            ),
            NewAmounts),
    set_prop_to(amounts, TempGame1, NewAmounts, TempGame2),
    enumerate(TempFac, 1, EnumFac),
    set_prop_to(center, EnumFac, [ligth_green], AllFac),
    set_prop_to(factories, TempGame2, AllFac, NewGame),
    % TODO: [stdevAntiD2ta] See what is happen when pass AllFac:factories, instead of AllFac in the next line
    info_log(["Starting new round. Factories distribution:\n", AllFac]).

%% any_full_row(+Player:Player, -Rows:Game) is semidet
% 
% The any_full_row/2 predicate check if the player have any full row
% in his Wall.
%
% @param Player Player target
% @param Rows On succeeds, return the number of full rows 
% @copyright 2kodevs 2019-2020
any_full_row(Player, RowsQ) :-
    property_of(table, Player, Table),
    findall(true,
            ( bagof(Column,
                    member((_, Column), Table),
                    Columns),
              length(Columns, 5)
            ),
            Rows),
    length(Rows, RowsQ),
    any(Rows).

%% ending_condition(+Game:Game) is semidet
% 
% The ending_condition/1 predicate check if the Game ends.
%
% @param Game Game to verify. 
% @copyright 2kodevs 2019-2020
ending_condition(Game) :-
    property_of(players, Game, P),
    member(X:_, P),
    any_full_row(X, _).
    
%% full_rows(+Player:Player, -Rows:Game) is det
% 
% The full_rows/2 predicate is a wrapper for any_full_row/2
% that return 0 when that function fails
%
% @param Player Player target
% @param Rows Return the number of full rows 
% @copyright 2kodevs 2019-2020
full_rows(Player, RowsQ) :-
    any_full_row(Player, RowsQ), !.
full_rows(_, 0).

%% cascade(+Tile:Point, +Table:list) is det
% 
% The cascade/2 predicate check if all the tiles of the
% same color of Tile below it are in Table
%
% @param Tile A table point
% @param Table Table target 
% @copyright 2kodevs 2019-2020
cascade((5, Col), Table) :-
    member((5, Col), Table).
cascade((Row, Col), Table) :-
    member((Row, Col), Table),
    NewRow is Row+1,
    NewCol is max((Col+1)mod 6, 1),
    cascade((NewRow, NewCol), Table).

%% full_colors(+Player:Player, -Amount:int) is det
% 
% The full_colors/2 predicate count the number of full colors in
% the Player Wall
%
% @param Player Player target
% @param Amount Number of full colors
% @copyright 2kodevs 2019-2020
full_colors(Player, Amount) :-
    property_of(table, Player, Table),
    findall(true,
            ( member((1, Col), Table),
              cascade((1, Col), Table)
            ),
            List),
    length(List, Amount).    

%% table_score(+Player:Player, -Score:int) is det
% 
% The table_score/2 predicate calculate the score of a player Wall
%
% @param Player Player target
% @param Score Player Wall Score
% @copyright 2kodevs 2019-2020
table_score(P, S) :-
    full_rows(P, RS),
    property_of(table, P, T),
    invert_axis(T, RT),
    full_rows([RT:table], CS),
    full_colors(P, DS),
    S is RS*2+CS*7+10*DS.

%% new_game(-Game:Game) is det
% 
% The new_game/1 predicate prepare a standard 4 player game
%
% @param Game New Game
% @copyright 2kodevs 2019-2020
new_game(Players, Factories, [P, A:amounts, O:outs, F:factories]) :-
    tiles_colors(C),
    new_players(Players, P),
    findall(20:X, member(X, C), A),
    findall(0:X, member(X, C), O),
    add([], 4, empty, E),
    add([], Factories, E, EF),
    enumerate(EF, 1, NF),
    set_prop_to(center, NF, [], F).

%% order_players(+Game:Game, -Players:list) is det
% 
% The order_players/2 predicate return the players in the round order
%
% @param Game A runing Game
% @param Players Sorted Players
% @copyright 2kodevs 2019-2020
order_players(Game, NewPlayers) :-
    property_of(players, Game, Players),
    indexed_sort(Players, OriginalOrder),
    sort_players(OriginalOrder, NewPlayers).

%% sort_players(+Players:list, -NewPlayers:list) is det
% 
% The sort_players/2 predicate return the players ordered from 1 to N, 
% where N is the number of players.
%
% @param Players Players target
% @param Players Ordered Players
% @copyright 2kodevs 2019-2020
sort_players(Players, NewPlayers) :-
    initial_player(Pid),
    concat(A, [Player:Pid|B], Players),
    concat([Player:Pid|B], A, NewPlayers).

%% run(+Game:Game, +Events:list, -NewGame:Game) is det
% 
% The run/3 predicate run the Game until it ends, given a list of 
% previous occured selection that players did in the current round.
%
% @param Game Game target
% @param Events Players selections in the round
% @param NewGame The resulting Game
% @copyright 2kodevs 2019-2020
run(Game, Events, NewGame) :-
    order_players(Game, Players),
    run_round(Game, Players, TempGame, CurEvents),
    concat(Events, CurEvents, NewEvents),
    validate(TempGame, NewEvents, NewGame).

%% validate(+Game:Game, +Events:list, -NewGame:Game) is det
% 
% The validate/3 predicate check if the Game current round end, 
% start a new one if needed, and continue running the Game.
%
% @param Game Game target
% @param Events Players selections in the round
% @param NewGame The resulting Game
% @copyright 2kodevs 2019-2020
validate(Game, Events, NewGame) :-
    property_of(factories, Game, Factories),
    findall(Fac, member(Fac:_, Factories), FacList),
    concat_all(FacList, AllTiles),
    length(AllTiles, Sz),
    count(AllTiles, empty, Sz), !,
    clean_players(Game, TempGame),
    end_or_continue(TempGame, Events, NewGame).
validate(Game, Events, NewGame) :-
    run(Game, Events, NewGame).

%% end_or_continue(+Game:Game, +Events:list, -NewGame:Game) is det
% 
% The end_or_continue/3 predicate check if the Game ends and calculate the scores, 
% or continue running the game.
%
% @param Game Game target
% @param Events Players selections in the round
% @param NewGame The resulting Game
% @copyright 2kodevs 2019-2020
end_or_continue(Game, _, NewGame) :-
    ending_condition(Game), !,
    calculate_scores(Game, NewGame).
end_or_continue(Game, Events, NewGame) :-
    initial_player(Id),
    get_value_or_default(center, Events, NewId, Id),
    retract(initial_player(Id)),
    asserta(initial_player(NewId)),
    property_of(players, Game, Players),
    property_of(NewId, Players, FirstPlayer),
    penalize(FirstPlayer, -1, NewFirstP),
    info_log([
        "Player ", 
        NewId, 
        " will be the first at the next round. ",
        "Cause that recive a penalization\n"
    ]),
    set_prop_to(NewId, Players, NewFirstP, NewPlayers),
    set_prop_to(players, Game, NewPlayers, TempGame1),
    new_round(TempGame1, TempGame2),
    run(TempGame2, [], NewGame).

%% calculate_scores(+Game:Game, -NewGame:Game) is det
% 
% The calculate_scores/2 predicate calculate the final score of each Game player 
%
% @param Game Game target
% @param NewGame The Game with the scores calculated
% @copyright 2kodevs 2019-2020
calculate_scores(Game, NewGame) :-
    property_of(players, Game, Players),
    findall(NewPlayer:Id,
            ( property_of(Id, Players, Player),
              table_score(Player, TableScore),
              property_of(score, player, Score),
              NewScore is Score+TableScore,
              set_prop_to(score, Player, NewScore, NewPlayer)
            ),
            NewPlayers),
    set_prop_to(players, Game, NewPlayers, NewGame).


main(Level, File, Players, Factories) :-
    set_log_mode(Level),
    set_log_file(File),
    project_info,
    info_log(["Preparing a ", Players, " players Game"]),
    new_game(Players, Factories, Game),
    new_round(Game, NewGame),
    run(NewGame, [], EndedGame), !, 
    info_log(["The game ends. Who will be the winner??\n", EndedGame:scores]),
    writeln("true.").
main(_, _) :- 
    error_log(["An unexpected failure occur"]),
    writeln("fail.").