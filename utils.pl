:- module(utils, [
    tiles_colors/1,
    concat/3,
    add/4,
    concat_all/2,
    any/1,
    make_intervals/2,
    property_of/3,
    get_value_or_default/4,
    remove_prop/3,
    set_prop_to/4,
    invert_axis/2,
    replace/5,
    index_of/3,
    column_of/3,
    count/3,
    enumerate/3,
    indexed_sort/2,
    format_fac/3,
    format_players/2,
    format_cell/2,
    fill_table/4
]).

http:location(pldoc, root('azul/help'), [priority(10)]).

%! tiles_colors(+Colors:list) is det
% 
% The tiles_colors/1 fact return the game tile colors 
%
% @param Colors Return the Tiles colors of the game
% @copyright 2kodevs 2019-2020
tiles_colors([blue, red, yellow, black, white]).

%! concat(+List1:list, +List2:list, -Result:list) is det
% 
% The concat/3 predicate return the concatenation of List1 and List2
% in Result 
%
% @param List1 First part of the result list
% @param List2 Second part of the result list
% @param Result The concatenation list
% @copyright 2kodevs 2019-2020
concat([], X, X).
concat([X|R], Y, [X|Z]) :-
    concat(R, Y, Z).

%! add(+List:list, +Amount:list, +Element, -Result:list) is det
% 
% The add/4 predicate return the concatenation of Amount occurences
% of Element at the end of List 
%
% @param List First part of the result list
% @param Amount Times that Element needs to be concatenated
% @param Element Value to concatenate
% @param Result The concatenation list
% @copyright 2kodevs 2019-2020
add(L, 0, _, L).
add(L, K, X, R) :-
    K>0,
    P is K-1,
    concat(L, [X], L1),
    add(L1, P, X, R).

%! list_print(+List:list) is det
% 
% The list_print/1 predicate print the List element in lines 
% separated by newline 
%
% @param List List target
% @copyright 2kodevs 2019-2020
list_print([]).
list_print([X|L]) :-
    writeln(X),
    list_print(L).

%! isList(+Object) is det
% 
% The isList/1 fact return if Object is a list 
% separated by newline 
%
% @param List List target
% @copyright 2kodevs 2019-2020
isList([]).
isList([_|_]).

%! concat_all(+List:list, -Result:list) is det
% 
% The concat_all/2 predicate return the concatenation of all list elements of List
%
% @param List1 List that contains the lists to concatenate
% @param Result The concatenation list
% @copyright 2kodevs 2019-2020
concat_all([], []).
concat_all([X|Y], R) :-
    concat_all(Y, L),
    concat(X, L, R).

%! any(+Object) is semidet
% 
% The any/1 predicate return true if Object is true, or is a list that contains any true
%
% @param Object Target
% @copyright 2kodevs 2019-2020
any(true).
any(L) :-
    isList(L),
    member(true, L).

%! consecutive(+List:list, +Point:point, -Consecutive:list, -Others:list) is det
% 
% The consecutive/4 predicate split the List in two parts. The elements consecutive
% in the two Point line, and the rest of them.
% 
% @param List Points list
% @param Point Target
% @param Consecutive Prefix of List containing the consecutive elements to Point
% @param Others Suffix of List with the rest of the elements
% @copyright 2kodevs 2019-2020
consecutive([(X, B)|L],  (X, Y), C, R) :-
    B is Y+1, !,
    consecutive(L,  (X, B), A, R),
    concat([(X, B)], A, C).
consecutive(L, _, [], L).

%! blocks(+List:list, -Blocks:list) is det
% 
% The blocks/2 predicate return a set of list that represent the 
% groups of elements that are consecutive in List. See consecutive/2
% 
% @param List Points list
% @param Block Consecutive groups of points
% @copyright 2kodevs 2019-2020
blocks([], []).
blocks([X|L], I) :-
    consecutive(L, X, C, R),
    concat([X], C, B),
    blocks(R, K),
    concat([B], K, I).

%! make_intervals(+List:list, -Intervals:list) is det
% 
% The make_intervals/2 predicate return the Blocks of List after a sort operation
% 
% @param List Points list
% @param Intervals Consecutive groups of points
% @copyright 2kodevs 2019-2020
make_intervals(L, I) :-
    isList(L),
    sort(L, S),
    blocks(S, I).

%! property_of(+Property, +Object:list, -Value) is nondet
% 
% The property_of/3 predicate return Value of an Object Property
% 
% @param Property Object property target
% @param Object Target
% @param Value The value associated with the object property
% @copyright 2kodevs 2019-2020
property_of(P, O, V) :-
    member(V:P, O).

%! get_value_or_default(+Property, +Object:list, -Value, +Default) is multi
% 
% The get_value_or_default/4 predicate is a wrapper for
% property_of/3 the same value when it succeeds and Default when it fails
% 
% @param Property Object property target
% @param Object Target
% @param Value The value associated with the object property
% @param Default The return value on failures
% @copyright 2kodevs 2019-2020
get_value_or_default(P, O, V, _) :-
    property_of(P, O, V).
get_value_or_default(_, _, D, D).

%! remove_prop(+Property, +Object:list, -NewObject:list) is det
% 
% The remove_prop/3 predicate remove a property of an object
% 
% @param Porperty Object property target
% @param Object Target
% @param NewObject The same as Object without property
% @copyright 2kodevs 2019-2020
remove_prop(_, [], []).
remove_prop(P, [_:P|R], L) :- !,
    remove_prop(P, R, L). 
remove_prop(P, [X:Y|R], [X:Y|L]) :-
    Y\=P,
    remove_prop(P, R, L).

%! set_prop_to(+Property, +Object:list, +Value, -NewObject:list) is det
% 
% The set_prop_to/4 predicate set the value of an Object property
% 
% @param Property Object property target
% @param Object Target
% @param Value new property value
% @param NewObject Modified object
% @copyright 2kodevs 2019-2020
set_prop_to(P, O, V, N) :-
    remove_prop(P, O, C),
    concat([V:P], C, N).

%! invert_axis(+List:list, -Result:list) is det
% 
% The invert_axis/2 predicate invert the coordinates of all points of List
% 
% @param List Points list
% @param Result Points inverted list
% @copyright 2kodevs 2019-2020
invert_axis(L, R) :-
    findall((Y, X), member((X, Y), L), R).

%! replace(+List:list, +Amount:int, +Value, +NewValue, -Result:list) is det
% 
% The replace/5 predicate change the first Amount occurences of Value in List
% to NewValue
% 
% @param List Objects list
% @param Amount The number of replacements
% @param Value The value to replace
% @param NewValue The new value in Result
% @param Result The list with the replacements
% @copyright 2kodevs 2019-2020
replace(L, 0, _, _, L) :- !.
replace(L, _, V, _, L) :-
    not(member(V, L)), !.
replace(L, T, V, N, R) :-
    T>0,
    Z is T-1,
    concat(A, [V|B], L), !,
    replace(B, Z, V, N, K),
    concat(A, [N|K], R).

%! index_of(+Value, +List:list, -Index:int) is multi
% 
% The index_of/3 predicate return the index of Value in List
% 
% @param Value Target
% @param List Elements container
% @param Index The index of Value in List
% @copyright 2kodevs 2019-2020
index_of(V, L, I) :-
    concat(A, [V|_], L),
    length(A, I).
index_of(_, _, -1).

%! column_of(+Line:Line, +Color, -Column:int) is det
% 
% The column_of/3 predicate given a line and a color return the
% column on the player Wall
%
% @param Line Player line
% @param Color Tile color
% @param Column The column that represent this color on Line
% @copyright 2kodevs 2019-2020
column_of(Line, Color, Column) :-
    tiles_colors(Colors),
    index_of(Color, Colors, Idx),
    Column is (Idx+Line-1)mod 5+1.

%! count(+List:list, +Value, -Amount:int) is det
% 
% The count/3 predicate return Amount of occurences of Value in List
% 
% @param List Elements container
% @param Value Target
% @param Amount Number of ocurences
% @copyright 2kodevs 2019-2020
count(L, V, R) :-
    findall(1, member(V, L), K),
    length(K, R).   

%! enumerate(+List:list, +Start:int, -Result:Index-List) is det
% 
% The enumerate/3 predicate return the List with its elements enumerated from Start
% to N, where N is Start + length of List
% 
% @param List Elements container
% @param Start The initial number to asign
% @param Result Enumerated List
% @copyright 2kodevs 2019-2020
enumerate([], _, []).
enumerate([E1|List], Number, [E1:Number|Enum]) :-
    Next is Number+1,
    enumerate(List, Next, Enum).

%! indexed_sort(+List:Index-List, -Result:Index-List) is det
% 
% The indexed_sort/2 predicate return List sorted by its indexes
% 
% @param List Elements container
% @param Result Elements Ordered by index
% @copyright 2kodevs 2019-2020
indexed_sort(L, R) :-
    findall(X:Y, property_of(X, L, Y), I),
    sort(I, O),
    findall(X:Y, property_of(X, O, Y), R).

%! split_fac(+RowLength:Int, +Current:Int, +List:List, +Top:List, +Bottom:List, -Result:List) is det
% 
% The split_fac/6 predicate return the elements of the factories sorted in two sides,
% an upper and bottom one, for displaying purposes
% 
% @param RowLength Length of the row of a single factory
% @param Current Starting index of the row
% @param List Elements of all factories concatenated
% @param Top List where the elements of the upper side of the print are going to be acumulated  
% @param Bottom List where the elements of the bottom side of the print are going to be acumulated  
% @param Result Two list, one will the upper side and another will the bottom side, ready to be raw printed
% @copyright 2kodevs 2019-2020
split_fac(_, _, [], Top, Buttom, [Top, Buttom]).
split_fac(Len, Cur, [X|Data], Acum, Buttom, R) :-
    NewCur is Cur+1,
    NewCur=<Len,
    concat(Acum, [X], NewAcum),
    split_fac(Len, NewCur, Data, NewAcum, Buttom, R), !.
split_fac(Len, _, Data, Top, Bottom, [T, B]) :-
    split_fac(Len, 0, Data, Bottom, Top, [B, T]).

%! format_fac(+Mode:Int, +List:List, +FD:File-Descriptor) is det
% 
% The format_fac/3 predicate prints the factories
% 
% @param Mode Mode of printing
% @param List Elements container
% @param FD File descriptor for where to write
% @copyright 2kodevs 2019-2020
format_fac(_, [], _) :- !.
format_fac(0, Data, FD) :-
    split_fac(2, 0, Data, [], [], [Top, Bottom]),
    length(Data, Len),
    make_space(7, '', S),
    Times is Len/4,
    nl(FD),
    write(FD, "Factories:\n"),
    print_symbol(Times, S, ++++++++++++++++++, FD),
    nl(FD),
    format_fac(1, Top, FD),
    nl(FD),
    format_fac(1, Bottom, FD),
    nl(FD),
    print_symbol(Times, S, ++++++++++++++++++, FD),
    nl(FD).
format_fac(1, [X|Line], FD) :-
    atom_string(X, SX),
    atom_length(X, Len),
    Y is 6-Len,
    make_space(Y, '  ', S),
    write(FD, '| '),
    write(FD, SX),
    write(FD, S),
    format_fac(2, Line, FD).
format_fac(2, [X], FD) :-
    atom_string(X, SX),
    write(FD, SX),
    atom_length(X, Len),
    Y is 6-Len,
    make_space(Y, ' ', S),
    write(FD, S),
    write(FD, '|'),
    format_fac(1, [], FD).
format_fac(2, [X|Line], FD) :-
    atom_string(X, SX),
    write(FD, SX),
    atom_length(X, Len),
    Y is 6-Len,
    make_space(Y, ' ', S),
    write(FD, S),
    write(FD, '|  ---  '),
    format_fac(1, Line, FD).
format_fac(3, [X|Line], FD) :-
    atom_string(X, SX),
    atom_length(X, Len),
    Y is 6-Len,
    make_space(Y, '  ', S),
    write(FD, SX),
    write(FD, S),
    format_fac(3, Line, FD).
format_fac(4, Center, FD) :-
    write(FD, "Center:\n"),
    length(Center, LenC),
    NewTimes is round(LenC*8+3),
    print_symbol(NewTimes, "", +, FD),
    nl(FD),
    write(FD, '| '),
    format_fac(3, Center, FD),
    write(FD, '|'),
    nl(FD),
    print_symbol(NewTimes, "", +, FD).

%! format_cell(+List:List, +FD:File-Descriptor) is det
% 
% The format_cell/2 predicate prints a list of elements in the 
% following format: | <item1>  <item2>  ... <itemN> |
% 
% @param List Elements container
% @param FD File descriptor for where to write
% @copyright 2kodevs 2019-2020
format_cell([], _).
format_cell([L|PL], FD) :-
    write(FD, '| '),
    format_fac(3, L, FD),
    write(FD, '|'),
    nl(FD),
    format_cell(PL, FD).

%! make_space(+Times:Int, +Initial_Separator:String, -Result:String) is det
% 
% The make_space/3 predicate return the result of concatenating the blank space ''
% Times times to Initial_Separator
% 
% @param Times Number of repetitions of the blank space
% @param Initial_Separator Initial string to concatenate the spaces
% @param Result Initial_Separator + Times * ' '
% @copyright 2kodevs 2019-2020
make_space(0, S, S) :- !.
make_space(Times, Acum, S) :-
    NewTimes is Times-1,
    string_concat(Acum, ' ', NewAcum),
    make_space(NewTimes, NewAcum, S).

%! print_symbol(+Times:Int, +Separator:String, +Symbol:String, +FD:File-Descriptor) is det
% 
% The print_symbol/4 predicate prints to FD Symbol Times times separated by Separator
% 
% @param Times Number of repetitions of Symbol + Space
% @param Separator Initial string to concatenate the spaces
% @param Symbol String to be repeated
% @param FD File descriptor for where to write
% @copyright 2kodevs 2019-2020
print_symbol(0, _, _, _) :- !.
print_symbol(Times, Space, Symb, FD) :-
    NewTimes is Times-1,
    write(FD, Symb),
    write(FD, Space),
    print_symbol(NewTimes, Space, Symb, FD).

%! format_players(+List:List, +FD:File-Descriptor) is det
% 
% The format_players/2 predicate prints the score and the strategy
% of players in List
% 
% @param List Elements container
% @param FD File descriptor for where to write
% @copyright 2kodevs 2019-2020
format_players([], _).
format_players([P:Score|Players], FD) :-
    property_of(id, P, Id),
    write(FD, 'Player '),
    write(FD, Id),
    write(FD, ' --- score: '),
    write(FD, Score),
    write(FD, ' --- strategy: '),
    property_of(strategy, P, St),
    write(FD, St),
    nl(FD),
    format_players(Players, FD).

%! fill_table(+Point:(X:Int, Y:Int), +Table:List, +Acum:List, +FD:File-Descriptor) is det
% 
% The fill_table/4 predicate prints the score and the strategy
% of players in List
% 
% @param Point Coordinate of the actual tile
% @param Table Wall with tiles
% @param Acum List where the elements of a row are going to be acumulated  
% @param FD File descriptor for where to write
% @copyright 2kodevs 2019-2020
fill_table((6, 1), _, R, R).
fill_table((X, 6), Table, Acum, [Acum|R]) :-
    NewX is X+1,
    fill_table((NewX, 1), Table, [], R), !.
fill_table((X, Y), Table, Acum, R) :-
    member((X, Y), Table), !,
    column_of(X, C, Y),
    concat(Acum, [C], NewAcum),
    NewY is Y+1,
    fill_table((X, NewY), Table, NewAcum, R).
fill_table((X, Y), Table, Acum, R) :-
    concat(Acum, ['  -'], NewAcum),
    NewY is Y+1,
    fill_table((X, NewY), Table, NewAcum, R).    