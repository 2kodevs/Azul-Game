:- begin_tests(utils).

:- include("utils.pl").


test(add) :-
    add([], 5, red, [red, red, red, red, red]),
    add([red, red], 3, blue, [red, red, blue, blue, blue]).

:- end_tests(utils).
