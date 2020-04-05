:- begin_tests(game).

:- include("game.pl").
:- include("utils.pl").
:- include("player.pl").

test(use_fac, [nondet]) :-
    use_fac([[empty, empty, empty, empty], [empty, empty, empty, empty]],
            [red, blue, yellow, black, white, black, red, red, blue, blue],
            R),
    assertion(R==[[red, blue, yellow, black], [white, black, red, red]]).


test(populate_fail, [nondet]) :-
    populate(
             [ [[empty], [empty]]:factories,
               [100:red, 100:blue]:amounts,
               [5:black]:outs
             ],
             _),
    populate(
             [ [[empty], [empty]]:factories,
               [3:red, 4:blue, 0:black]:amounts,
               [0:red, 0:blue, 5:black]:outs
             ],
             R),
    assertion(R==[[0:red, 0:blue, 0:black]:outs, [3:red, 4:blue, 5:black]:amounts, [[empty], [empty]]:factories]).

test(any_full_row, [nondet]) :-
    any_full_row(
                 [ [(0, 3),  (1, 1),  (2, 1),  (1, 2),  (1, 0),  (4, 4),  (1, 3),  (3, 2),  (1, 4),  (9, 9),  (3, 1),  (3, 0),  (3, 3),  (3, 4)]:table
                 ],
                 R),
    assertion(R=:=2).

test(any_full_row_fail, [fail]) :-
    any_full_row([[]:table], _).

test(full_rows, [nondet]) :-
    full_rows(
              [ [(0, 3),  (1, 1),  (2, 1),  (1, 2),  (1, 0),  (4, 4),  (1, 3),  (3, 2),  (1, 4),  (9, 9),  (3, 1),  (3, 0),  (3, 3),  (3, 4)]:table
              ],
              R1),
    assertion(R1=:=2),
    full_rows([[]:table], R2),
    assertion(R2=:=0).

test(cascade, [nondet]) :-
    cascade((2, 3), [(2, 3),  (3, 4),  (4, 5),  (5, 1)]).

test(cascade_fail, [fail]) :-
    cascade((2, 3), [(2, 3),  (3, 4),  (4, 5)]).

test(full_colors, [nondet]) :-
    full_colors(
                [ [(2, 3),  (4, 1),  (3, 4),  (5, 2),  (1, 1),  (4, 5),  (3, 2),  (1, 3),  (5, 1),  (2, 4),  (1, 2),  (3, 5)]:table
                ],
                R),
    assertion(R=:=2).

test(ending_condition, [nondet]) :-
    ending_condition(
                     [ [[[(0, 3),  (1, 1),  (2, 1),  (1, 2),  (1, 0),  (4, 4),  (1, 3),  (3, 2),  (1, 4),  (9, 9),  (3, 1),  (3, 0),  (3, 3),  (3, 4)]:table]:1, [[(0, 3),  (1, 1),  (2, 1),  (4, 4),  (1, 3),  (3, 2),  (1, 4),  (9, 9),  (3, 1),  (3, 0),  (3, 3),  (3, 4)]:table]:2]:players
                     ]).

test(table_score, [nondet]) :-
    table_score(
                [ [(2, 3),  (4, 1),  (3, 4),  (2, 1),  (5, 2),  (1, 1),  (3, 3),  (4, 4),  (4, 5),  (3, 2),  (1, 3),  (1, 4),  (5, 1),  (3, 1),  (2, 4),  (1, 2),  (3, 5)]:table
                ],
                R),
    assertion(R=:=29).

test(new_game, [nondet]) :-
    new_players(4, P),
    new_game(Game),
    assertion(Game==[P, [20:blue, 20:red, 20:yellow, 20:black, 20:white]:amounts, [0:blue, 0:red, 0:yellow, 0:black, 0:white]:outs, [[]:center, [empty, empty, empty, empty]:1, [empty, empty, empty, empty]:2, [empty, empty, empty, empty]:3, [empty, empty, empty, empty]:4, [empty, empty, empty, empty]:5, [empty, empty, empty, empty]:6, [empty, empty, empty, empty]:7, [empty, empty, empty, empty]:8, [empty, empty, empty, empty]:9]:factories]).

:- end_tests(game).

