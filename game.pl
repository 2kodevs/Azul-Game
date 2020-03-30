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

any_full_row(P, S):-
    property_of(table, P, T),
    findall(true, (
        bagof(X, member((_, X), T), Col),
        length(Col, 5)
    ), Rows),
    length(Rows, S),
    any(Rows).

ending_condion([], []).
ending_condion(Data):-
    findall(true, (
        member(X, Data),
        any_full_row(X, _)    
    ), P),
    any(P).

line_score(L, Tile, S):-
    make_intervals(L, I),
    findall(X, (
        member(X, I),
        member(Tile, X)    
    ), [B]),
    length(B, S).
    
tile_score(P, (X, Y), S):-
    property_of(table, P, T),
    concat(T, [(X, Y)], N),
    line_score(N, (X, Y), RS),
    invert_axis(N, RN),
    line_score(RN, (Y, X), CS),
    S is RS + CS.
    
full_rows(P, S):- 
    any_full_row(P, S), !.
full_rows(_, 0).

cascade((5, Y), L):-
    member((5, Y), L).
cascade((X, Y), L):-
    member((X, Y), L),
    NX is X + 1,
    Y1 is (Y + 1) mod 6,
    max(Y1, 1, NY),
    cascade((NX, NY), L).

full_colors(P, S):-
    property_of(table, P, T),
    findall(true, (
        member((1, X), T),
        cascade((1, X), T)
    ), L),
    length(L, S).    

table_score(P, S):-
    full_rows(P, RS),
    property_of(table, P, T),
    invert_axis(T, RT),
    full_rows([RT:table], CS),
    full_colors(P, DS),
    S is RS * 2 + CS * 7 + 10 * DS.

tiles_colors([blue, red, yellow, black, white]).

new_game():-
    tiles_colors(C),
    findall(20:X, member(X, C), A),
    add([], 4, [[]:table, 0:score], P),
    findall(0:X, member(X, C), O),
    add([], 4, empty, E),
    add([], 9, E, F),
    Game = [P:players, A:amounts, O:outs, F:factories],