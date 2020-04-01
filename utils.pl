tiles_colors([blue, red, yellow, black, white]).

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

concat_all([], []).
concat_all([X | Y], R) :- 
    concat_all(Y, L),
    concat(X, L, R).

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

get_value_or_default(P, O, V, _):-
    property_of(P, O, V), !.
get_value_or_default(_, _, D, D).

remove_prop(_, [], []).
remove_prop(P, [(_:P) | R], L):-
    !, remove_prop(P, R, L). 
remove_prop(P, [(X:Y) | R], [(X:Y) | L]):-
    Y \= P,
    remove_prop(P, R, L).

set_prop_to(P, O, V, N):-
    remove_prop(P, O, C),
    concat([V:P], C, N).

invert_axis(L, R):-
    findall((Y, X), member((X, Y), L), R).

replace(L, 0, _, _, L):- !.
replace(L, _, V, _, L):-
    not(member(V, L)), !.
replace(L, T, V, N, R):-
    T > 0,
    Z is T - 1,
    concat(A, [V | B], L), !,
    replace(B, Z, V, N, K),
    concat(A, [N | K], R).

index_of(V, L, I):-
    concat(A, [V | _], L), !,
    length(A, I).

count(L, V, R):-
    findall(1, member(V, L), K),
    length(K, R).   

enumerate([], _, []).
enumerate([E1 | List], Number, [E1:Number | Enum]):-
    Next is Number + 1,
    enumerate(List, Next, Enum).