:- [player].
:- dynamic initial_player/1.

initial_player(1).

use_fac(L, [], L).
use_fac([], _, []).
use_fac([[] | F], L, [[] | R]):-
    use_fac(F, L, R).
use_fac([[_ | A] | F], [X | L], [[X | B] | R]):-
    use_fac([A | F], L, [B | R]).

populate(G0, NG):-
    property_of(amounts, G0, A),
    property_of(factories, G0, F),
    length(F, V0),
    findall(X, member(X:_, A), N),
    sum_list(N, V1), 
    V1 < V0 * 4, !,
    property_of(outs, G0, M),
    findall(K:X, (
        property_of(X, A, V2),
        property_of(X, M, V3),
        K is V2 + V3
    ), NA),
    set_prop_to(amounts, G0, NA, G1),
    findall(0:X, member(_:X, M), NO),
    set_prop_to(outs, G1, NO, NG).
populate(G, G).

new_round(G0, NG):-
    populate(G0, G1),
    property_of(amounts, G1, A),
    findall(L, (
        member(V:C, A),
        add([], V, C, L)    
    ), W),
    concat_all(W, R),
    random_permutation(R, D),
    property_of(factories, G1, AF),
    remove_prop(center, AF, GF),
    findall(X, member(X:_, GF), Z),
    use_fac(Z, D, Q),
    concat_all(Q, S),
    findall(V:C, (
        member(Cur:C, A),
        count(S, C, K),
        V is Cur - K
    ), NA),
    set_prop_to(amounts, G1, NA, G2),
    enumerate(Q, 1, NF),
    set_prop_to(center, NF, [], F),
    set_prop_to(factories, G2, F, NG).

any_full_row(P, S):-
    property_of(table, P, T),
    findall(true, (
        bagof(X, member((_, X), T), Col),
        length(Col, 5)
    ), Rows),
    length(Rows, S),
    any(Rows).

ending_condion(Game):-
    property_of(players, Game, P),
    member(X:_, P),
    any_full_row(X, _).
    
full_rows(P, S):- 
    any_full_row(P, S), !.
full_rows(_, 0).

cascade((5, Y), L):-
    member((5, Y), L).
cascade((X, Y), L):-
    member((X, Y), L),
    NX is X + 1,
    NY is max((Y + 1) mod 6, 1),
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

new_game([P, A:amounts, O:outs, F:factories]):-
    tiles_colors(C),
    new_players(4, P),
    findall(20:X, member(X, C), A),
    findall(0:X, member(X, C), O),
    add([], 4, empty, E),
    add([], 9, E, EF),
    enumerate(EF, 1, NF),
    set_prop_to(center, NF, [], F).

order_players(G, NP):-
    property_of(players, G, P0),
    indexed_sort(P0, P1),
    sort_players(P1, NP).

sort_players(P0, NP):-
    initial_player(Pid),
    concat(A, [V:Pid | B], P0),
    concat([V:Pid | B], A, NP).

run(G0, NG):-
    order_players(G0, P),
    run_round(G0, P, G1, E),
    validate(G1, E, NG).

validate(G0, E, NG):-
    property_of(factories, G0, F),
    findall(X, member(X:_, F), L),
    concat_all(L, R),
    length(R, Sz),
    count(R, empty, Sz), !,
    clean_players(G0, G1),
    end_or_continue(G1, E, NG).
validate(G0, _, NG):-
    run(G0, NG).

end_or_continue(G0, _, NG):-
    ending_condion(G0), !,
    calculate_scores(G0, NG).
    %TODO: show winner and scores
end_or_continue(G0, E, NG):-
    initial_player(Id),
    get_value_or_default(center, E, Nid, Id),
    retract(initial_player(Id)),
    asserta(initial_player(Nid)),
    property_of(players, G0, GP),
    property_of(Nid, GP, F),
    penalize(F, -1, NF),
    set_prop_to(Nid, GP, NF, NP),
    set_prop_to(players, G0, NP, G1),
    new_round(G1, G2),
    run(G2, NG).

calculate_scores(G0, G1):-
    property_of(players, G0, GP),
    findall(NP:Id, (
        member(X:Id, GP),
        table_score(X, TS),
        property_of(score, X, PS),
        S is PS + TS,
        set_prop_to(score, X, S, NP)
    ), P),
    set_prop_to(players, G0, P, G1).

main :-
    new_game(G0), 
    new_round(G0, G1),
    run(G1, _), !. 
    % TODO: Print the winner