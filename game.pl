% import utils to run this file

use_fac(_, 0, []).
use_fac([A, B, C, D | L], X, [[A, B, C, D] | R]):-
    X > 0,
    X1 is X - 1,
    use_fac(L, X1, R).

new_round(F):-
    add([], 20, red, A),
    add(A, 20, blue, B),
    add(B, 20, yellow, Y),
    add(Y, 20, grey, G),
    add(G, 20, white, W),
    random_permutation(W, D),
    use_fac(D, 9, F).
