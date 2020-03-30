% import utils to run this file

use_fac(L, [], L).
use_fac([], _, []).
use_fac([[] | F], L, [[] | R]):-
    use_fac(F, L, R).
use_fac([[_ | A] | F], [X | L], [[X | B] | R]):-
    use_fac([A | F], L, [B | R]).

populate(A, F, G, NA):-
    length(F, V0),
    findall(X, member(X:_, A), N),
    sum_list(N, V1), 
    V1 < V0 * 4, !,
    property_of(outs, G, M),
    findall(K:X, (
        property_of(X, A, V2),
        property_of(X, M, V3),
        K is V2 + V3
    ), NA).
populate(A, _, _, A).

new_round(G0, NG):-
    property_of(amounts, G0, GA),
    property_of(factories, G0, GF),
    populate(GA, GF, G0, A),
    findall(L, (
        member(V:C, A),
        add([], V, C, L)    
    ), W),
    concat_all(W, R),
    random_permutation(R, D),
    use_fac(GF, D, F),
    set_prop_to(amounts, G0, A, G1),
    set_prop_to(factories, G1, F, NG).

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
    run(Game).

run(G0):-
    new_round(G0, G1),
    %TODO: run the round
    validate(G1).

validate(G0):-
    ending_condion(G0), !,
    calculate_scores(G0, _).
    %TODO: show winner and scores
validate(G):-
    run(G).

calculate_scores(G0, G1):-
    property_of(players, G0, GP),
    findall(NP, (
        member(X, GP),
        table_score(X, TS),
        property_of(score, X, PS),
        S is PS + TS,
        set_prop_to(score, X, S, NP)
    ), P),
    set_prop_to(players, G0, P, G1).
