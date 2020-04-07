:- [utils].
:- (dynamic log_dir/1).
:- (dynamic log_mode/1).

%% log_dir(-Dir:string) is det
% 
% The log_dir/1 fact return the current log file 
%
% @param Dir Return the name of the current log file
% @copyright 2kodevs 2019-2020
log_dir("log.log").

%% log_mode(-ModeId:int) is det
% 
% The log_mode/1 fact return the current log mode 
% identifier. Default to 3, that means info
%
% @param Dir Return the name of the current log file
% @copyright 2kodevs 2019-2020
log_mode(3).

%% log_id(+ModeName, -ModeId:int) is det
% 
% The log_id/2 fact return given a mode name
% return the identifier associated to it.
%
% @param Dir Return the name of the current log file
% @copyright 2kodevs 2019-2020
log_id(error, 1).
log_id(warning, 2).
log_id(info, 3).
log_id(debug, 4).

%% print_log(+Data, +FileDescriptor) is det
% 
% The print_log/2 predicate write Data to FileDescriptor in the
% better possible way.
%
% @param Data Output Target
% @param FileDescriptor File Target
% @copyright 2kodevs 2019-2020
print_log(error, FD) :-
    write(FD, "ERROR: "), !.
print_log(warning, FD) :-
    write(FD, "WARNING: "), !.
print_log(info, FD) :-
    write(FD, "INFO: "), !.
print_log(debug, FD) :-
    write(FD, "DEBUG: "), !.
print_log(Facs:factories, FD) :-
    property_of(center, Facs, Center),
    remove_prop(center, Facs, Data),
    findall(V, member(V:_, Data), L),
    concat_all(L, NewData),
    format_fac(0, NewData, FD),
    format_fac(4, Center, FD), !.
print_log(Game:center, FD) :-
    property_of(factories, Game, Facs),
    property_of(center, Facs, Center),
    format_fac(4, Center, FD).
print_log(Player:pattern, FD) :-
    nl(FD),
    property_of(board, Player, B),
    findall(PL:Id, ( member(X:Id, B),
        property_of(stocks, X, Stocks),
        Times is 5-Id,
        add(Stocks, Times, '  -', PL)
    ), RawLines),
    indexed_sort(RawLines, LinesSorted),
    findall(Line, member(Line:_, LinesSorted), Lines),
    write(FD, "               Pattern Line                \n"),
    write(FD, "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"),
    format_cell(Lines, FD),
    write(FD, "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"), !.
print_log(Game:scores, FD) :-
    property_of(players, Game, Players),
    nl(FD),
    findall([Id:id, Strategy:strategy]:Score, ( 
        member(X:Id, Players),
        property_of(score, X, Score),
        property_of(strategy, X, Strategy)
    ), PlayersInverted),
    indexed_sort(PlayersInverted, PlayersSorted),
    reverse(PlayersSorted, P),
    format_players(P, FD), !.
print_log(Data:player, FD) :-
    property_of(player, Data, Player),
    property_of(table, Player, RawTable),
    sort(RawTable, Table),
    fill_table((1, 1), Table, [], FTable),
    nl(FD),
    property_of(id, Data, Id),
    string_concat("~~~~~~~~~~~~", "Player ", S1),
    string_concat(Id, " --- Wall~~~~~~~~~~~~~~", S2),
    string_concat(S1, S2, S3),
    write(FD, S3),
    nl(FD),
    format_cell(FTable, FD),
    write(FD, "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"),
    property_of(score, Player, Score),
    property_of(strategy, Player, St),
    format_players([[Id:id, St:strategy]:Score], FD), !.
print_log(Data, FD) :-
    write(FD, Data).

%% show_logs(+List:list) is det
% 
% The show_logs/1 predicate Display all the 
% data contained in List
%
% @param List Outputs list
% @copyright 2kodevs 2019-2020
show_logs(List) :-
    file_descriptor(append, FD),
    findall(1, ( 
        member(Data, List),
        print_log(Data, FD)
    ), _),
    nl(FD),
    close(FD).

%% valid_log(+ModeName, +List:list) is det
% 
% The valid_log/2 predicate Display the data contained in 
% List if the ModeName is valid.
%
% @param ModeName Target mode
% @param List Outputs list
% @copyright 2kodevs 2019-2020
valid_log(Mode, List) :-
    log_id(Mode, Id),
    log_mode(Current),
    Current>=Id, !,
    show_logs([Mode|List]).
valid_log(_, _).

%% error_log(+List:list) is det
% 
% The error_log/1 predicate Show a log in mode error 
%
% @param List Outputs list
% @copyright 2kodevs 2019-2020
error_log(List) :-
    valid_log(error, List).

%% warning_log(+List:list) is det
% 
% The warning_log/1 predicate Show a log in mode warning 
%
% @param List Outputs list
% @copyright 2kodevs 2019-2020
warning_log(List) :-
    valid_log(warning, List).

%% info_log(+List:list) is det
% 
% The info_log/1 predicate Show a log in mode info 
%
% @param List Outputs list
% @copyright 2kodevs 2019-2020
info_log(List) :-
    valid_log(info, List).

%% debug_log(+List:list) is det
% 
% The debug_log/1 predicate Show a log in mode debug 
%
% @param List Outputs list
% @copyright 2kodevs 2019-2020
debug_log(List) :-
    valid_log(debug, List).

%% set_log_mode(+ModeName) is det
% 
% The set_log_mode/1 predicate set the logger  
% system mode to ModeName.
%
% @param ModeName Mode target
% @copyright 2kodevs 2019-2020
set_log_mode(Mode) :-
    log_id(Mode, NewId),
    log_mode(Id),
    retract(log_mode(Id)),
    asserta(log_mode(NewId)), !.
set_log_mode(Mode) :-
    log_mode(Id),
    log_id(Current, Id),
    warning_log([ 
        "Unkonwn mode <",
        Mode,
        ">. Previous mode <",
        Current,
        "> is still in use."
    ]).

%% set_log_file(+FileDir) is det
% 
% The set_log_file/1 predicate set the path where the logger  
% write to FileDir.
%
% @param FileDir File to use for store the logs
% @copyright 2kodevs 2019-2020
set_log_file(Dir) :-
    log_dir(OldDir),
    retract(log_dir(OldDir)),
    asserta(log_dir(Dir)), !.

%% file_descriptor(+Mode, -FileDescriptor) is det
% 
% The file_descriptor/2 predicate return a file descriptor 
% associated to the current log file open in Mode
%
% @param Mode File descriptor required mode
% @param FileDescriptor An open file descriptor
% @copyright 2kodevs 2019-2020
file_descriptor(Mode, FD) :-
    log_dir(Dir),
    open(Dir, Mode, FD).

%% project_info(+Mode, -FileDescriptor) is det
% 
% The project_info/0 predicate write the project information 
% to the log file
%
% @copyright 2kodevs 2019-2020
project_info :-
    file_descriptor(write, FD),
    writeln(FD, "Azul-Game Developed by 2kodevs"),
    writeln(FD, "See us at https://github.com/2kodevs/\n"),
    close(FD).

%% set_log_mode_by_id(+ModeId) is det
% 
% The set_log_mode_by_id/1 predicate set the logger  
% system mode to the mode with id ModeId.
%
% @param ModeId Id of Mode target
% @copyright 2kodevs 2019-2020
set_log_mode_by_id(Id):-
    log_id(Mode, Id),
    set_log_mode(Mode).