:- begin_tests(game).

:- include("game.pl").
:- include("utils.pl").

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

:- end_tests(game).

