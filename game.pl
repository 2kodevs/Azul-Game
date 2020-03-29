any_full_rows(P):-
    member(T:table, P),
    findall(true, (
        bagof(X, member((_, X), T), Col),
        length(Col, 5)
    ), Rows),
    any(Rows).

ending_condion([], []).
ending_condion(Data):-
    findall(true, (
        member(X, Data),
        any_full_rows(X)    
    ), P),
    any(P).
    
