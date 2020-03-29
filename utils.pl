concat([], X, X).
concat([X | R], Y, [X | Z]) :- 
    concat(R, Y, Z).

add(L, 0, _, L).
add(L, K, X, R):-
    K > 0,
    P is K - 1,
    concat(L, [X], L1),
    add(L1, P, X, R).

use_fac(_, 0, []).
use_fac([A, B, C, D | L], X, [[A, B, C, D] | R]):-
    X > 0,
    X1 is X - 1,
    use_fac(L, X1, R).

fac_print([]).
fac_print([X | L]):-
    writeln(X),
    fac_print(L).

new_round(F):-
    add([], 20, red, A),
    add(A, 20, blue, B),
    add(B, 20, yellow, Y),
    add(Y, 20, grey, G),
    add(G, 20, white, W),
    random_permutation(W, D),
    use_fac(D, 9, F).

    
