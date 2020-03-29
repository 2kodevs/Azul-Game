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

    
