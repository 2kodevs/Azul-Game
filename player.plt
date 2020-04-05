:- begin_tests(player).

:- include("utils.pl").
:- include("player.pl").

test(line_score, [nondet]) :-
    line_score([(1, 2),  (2, 3),  (1, 1),  (2, 5),  (2, 4),  (3, 4),  (1, 4)],  (2, 4), Score),
    assertion(Score=:=3).

test(tile_score, [nondet]) :-
    tile_score(
               [ [(1, 2),  (2, 3),  (1, 1),  (2, 5),  (2, 4),  (3, 4),  (1, 4),  (3, 5),  (3, 6),  (3, 3)]:table
               ],
               (2, 4),
               Score),
    assertion(Score=:=6).

test(new_players, [nondet]) :-
    new_players(2, Players:players),
    penalization_list(P),
    strategies(S),
    findall(Y:Id,
            ( member(L:Id, Players),
              member(X:strategy, L),
              member(X, S),
              concat([X:strategy], Y, L)
            ),
            NewP),
    assertion(NewP==[[[[[empty]:stocks, [blue, red, yellow, black, white]:valid, [blue, red, yellow, black, white]:all]:1, [[empty, empty]:stocks, [blue, red, yellow, black, white]:valid, [blue, red, yellow, black, white]:all]:2, [[empty, empty, empty]:stocks, [blue, red, yellow, black, white]:valid, [blue, red, yellow, black, white]:all]:3, [[empty, empty, empty, empty]:stocks, [blue, red, yellow, black, white]:valid, [blue, red, yellow, black, white]:all]:4, [[empty, empty, empty, empty, empty]:stocks, [blue, red, yellow, black, white]:valid, [blue, red, yellow, black, white]:all]:5]:board, P, []:table, 0:score]:1, [[[[empty]:stocks, [blue, red, yellow, black, white]:valid, [blue, red, yellow, black, white]:all]:1, [[empty, empty]:stocks, [blue, red, yellow, black, white]:valid, [blue, red, yellow, black, white]:all]:2, [[empty, empty, empty]:stocks, [blue, red, yellow, black, white]:valid, [blue, red, yellow, black, white]:all]:3, [[empty, empty, empty, empty]:stocks, [blue, red, yellow, black, white]:valid, [blue, red, yellow, black, white]:all]:4, [[empty, empty, empty, empty, empty]:stocks, [blue, red, yellow, black, white]:valid, [blue, red, yellow, black, white]:all]:5]:board, P, []:table, 0:score]:2]).  

test(column_of, [nondet]) :-
    column_of(4, blue, R),
    assertion(R=:=4).

test(empty_board, [nondet]) :-
    empty_board(Data),
    assertion(Data==[[[empty]:stocks, [blue, red, yellow, black, white]:valid, [blue, red, yellow, black, white]:all]:1, [[empty, empty]:stocks, [blue, red, yellow, black, white]:valid, [blue, red, yellow, black, white]:all]:2, [[empty, empty, empty]:stocks, [blue, red, yellow, black, white]:valid, [blue, red, yellow, black, white]:all]:3, [[empty, empty, empty, empty]:stocks, [blue, red, yellow, black, white]:valid, [blue, red, yellow, black, white]:all]:4, [[empty, empty, empty, empty, empty]:stocks, [blue, red, yellow, black, white]:valid, [blue, red, yellow, black, white]:all]:5]:board).

:- end_tests(player).

