concat([], X, X).
concat([X | R], Y, [X | Z]) :- 
    concat(R, Y, Z).

isList([]).
isList([_|_]).

any(true).
any(L) :- isList(L), member(true, L).