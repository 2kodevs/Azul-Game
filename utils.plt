:- begin_tests(utils).

:- include("utils.pl").


test(add, [nondet]) :-
    add([], 5, red, R),
    assertion(R==[red, red, red, red, red]),
    add(R, 3, blue, B),
    assertion(concat(R, [blue, blue, blue], B)).

test(concat_all, [nondet]) :-
    concat_all([[1, 2], [3, 4]], R),
    assertion(R==[1, 2, 3, 4]).

test(make_intervals, [nondet]) :-
    make_intervals([(0, 0),  (0, 1),  (1, 1),  (1, 3)], R),
    assertion(R==[[(0, 0),  (0, 1),  (1, 1)], [(1, 3)]]).

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

test(replace, [nondet]) :-
    replace([1, 6, 3, 4, 5, 6, 6], 2, 6, 2, R1),
    assertion(R1==[1, 2, 3, 4, 5, 2, 6]),
    replace([1, 2, 3], 5, 4, 6, R2),
    assertion(R2==[1, 2, 3]).

test(index_of_ok, [nondet]) :-
    index_of(2, [1, 2, 3], I),
    assertion(I=:=1).

test(index_of_fail, [fail]) :-
    index_of(2, [3, 4, 5], _).

test(count_ok, [nondet]) :-
    count([1, 2, 3, 2], 2, I1),
    assertion(I1=:=2),
    count([1, 2, 3], 4, I2),
    assertion(I2=:=0).

:- end_tests(utils).
