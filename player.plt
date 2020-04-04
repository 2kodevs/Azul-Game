:- begin_tests(player).

:- include("utils.pl").
:- include("player.pl").

test(line_score, [nondet]) :-
    line_score([(1, 2),  (2, 3),  (1, 1),  (2, 5),  (2, 4),  (3, 4),  (1, 4)],  (2, 4), Score),
    assertion(Score=:=3).

test(column_of, [nondet]) :-
    column_of(4, blue, R),
    assertion(R=:=4).

:- end_tests(player).