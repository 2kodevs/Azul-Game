:- begin_tests(game).

:- include("game.pl").
:- include("utils.pl").

test(use_fac, [nondet]) :-
    use_fac([[empty, empty, empty, empty], [empty, empty, empty, empty]],
            [red, blue, yellow, black, white, black, red, red, blue, blue],
            R),
    assertion(R==[[red, blue, yellow, black], [white, black, red, red]]).


test(populate_fail, [fail]) :-
    populate(
             [ [[empty], [empty]]:factories,
               [100:red, 100:blue]:amounts,
               [5:black]:outs
             ],
             _).

:- end_tests(game).

