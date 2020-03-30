concat([], X, X).
concat([X | R], Y, [X | Z]) :- 
    concat(R, Y, Z).

add(L, 0, _, L).
add(L, K, X, R):-
    K > 0,
    P is K - 1,
    concat(L, [X], L1),
    add(L1, P, X, R).

list_print([]).
list_print([X | L]):-
    writeln(X),
    list_print(L).

isList([]).
isList([_|_]).

any(true).
any(L) :- 
    isList(L), member(true, L).

consecutive([(X, B) | L], (X, Y), C, R):-
    B is Y + 1, !,
    consecutive(L, (X, B), A, R),
    concat([(X, B)], A, C).
consecutive(L, _, [], L).

blocks([], []).
blocks([X | L], I):-
    consecutive(L, X, C, R),
    concat([X], C, B),
    blocks(R, K),  
    concat([B], K, I).

make_intervals(L, I):-
    isList(L),
    sort(L, S),
    blocks(S, I).

property_of(P, O, V):-
    member(V:P, O).

invert_axis(L, R):-
    bagof((Y, X), Y^member((X, Y), L), R).

max(X, Y, Y):-
    Y >= X, !.
max(X, Y, X):-
    X >= Y.