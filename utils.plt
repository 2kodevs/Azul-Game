:- begin_tests(utils).

:- include("utils.pl").


test(add) :-
    add([], 5, red, R),
    assertion(R==[red, red, red, red, red]),
    add(R, 3, blue, B),
    assertion(concat(R, [blue, blue, blue], B)).

:- end_tests(utils).
