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

:- end_tests(game).

