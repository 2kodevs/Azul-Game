:- begin_tests(utils).

:- include("utils.pl").


test(add, [nondet]) :-
    add([], 5, red, R),
    assertion(R==[red, red, red, red, red]),
    add(R, 3, blue, B),
    assertion(concat(R, [blue, blue, blue], B)).

test(make_intervals, [blocked(1)]) :-
    make_intervals([1, 2, 3, 1, 2, 3], R),
    assertion(R==[1, 1, 2, 2, 3, 3]).

test(prop, [nondet]) :-
    set_prop_to(prop1, [[1, 2, 3]:prop1, v2:prop2], v1, N),
    remove_prop(prop1, N, R),
    assertion(R==[v2:prop2]),
    remove_prop(prop1, [v2:prop2], R),
    assertion(R==[v2:prop2]),
    set_prop_to(prop1, R, v1, X),
    property_of(prop1, X, V),
    assertion(V==v1).

test(invert_axis, [nondet]) :-
    invert_axis([(0, 0),  (0, 1),  (1, 0),  (1, 1)], R),
    assertion(R==[(0, 0),  (1, 0),  (0, 1),  (1, 1)]).

:- end_tests(utils).
