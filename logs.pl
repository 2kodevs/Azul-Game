:- [utils].
:- dynamic log_dir/1.
:- dynamic log_mode/1.

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

%% print_log(+Data) is det
% 
% The print_log/1 predicate try to display Data in the
% better possible way.
%
% @param Data Output Target
% @copyright 2kodevs 2019-2020
print_log(Data):-
    write(Data).

%% show_logs(+List:list) is det
% 
% The show_logs/1 predicate Display all the 
% data contained in List
%
% @param List Outputs list
% @copyright 2kodevs 2019-2020
show_logs(List):-
    findall(1, (
        member(Data, List),
        print_log(Data)
    ), _).

%% show_mode(+ModeName) is det
% 
% The show_mode/1 predicate Display the log 
% mode name
%
% @param ModeName Target
% @copyright 2kodevs 2019-2020
show_mode(error):- write("EROR: ").
show_mode(warning):- write("WARNING: ").
show_mode(info):- write("INFO: ").
show_mode(debug):- write("DEBUG: ").

%% valid_log(+ModeName, +List:list) is det
% 
% The valid_log/2 predicate Display the data contained in 
% List if the ModeNme is valid.
%
% @param ModeName Target mode
% @param List Outputs list
% @copyright 2kodevs 2019-2020
valid_log(Mode, List):-
    log_id(Mode, Id),
    log_mode(Current),
    Current >= Id, !,
    show_mode(Mode),
    show_logs(List), nl.
valid_log(_, _).

%% error_log(+List:list) is det
% 
% The error_log/1 predicate Show a log in mode error 
%
% @param List Outputs list
% @copyright 2kodevs 2019-2020
error_log(List):- valid_log(error, List).

%% warning_log(+List:list) is det
% 
% The warning_log/1 predicate Show a log in mode warning 
%
% @param List Outputs list
% @copyright 2kodevs 2019-2020
warning_log(List):- valid_log(warning, List).

%% info_log(+List:list) is det
% 
% The info_log/1 predicate Show a log in mode info 
%
% @param List Outputs list
% @copyright 2kodevs 2019-2020
info_log(List):- valid_log(info, List).

%% debug_log(+List:list) is det
% 
% The debug_log/1 predicate Show a log in mode debug 
%
% @param List Outputs list
% @copyright 2kodevs 2019-2020
debug_log(List):- valid_log(debug, List).

%% set_log_mode(+ModeName) is det
% 
% The set_log_mode/1 predicate set the logger  
% system mode to ModeName.
%
% @param Dir Return the name of the current log file
% @copyright 2kodevs 2019-2020
set_log_mode(Mode):-
    log_id(Mode, NewId),
    log_mode(Id),
    retract(log_mode(Id)),
    asserta(log_mode(NewId)), !.
set_log_mode(Mode):- 
    log_mode(Id),
    log_id(Current, Id),
    warning_log(["Unkonwn mode <", Mode, ">. Previous mode <", Current, "> is still in use."]).

%% file_descriptor(+Mode, -FileDescriptor) is det
% 
% The file_descriptor/1 predicate return a file descriptor 
% associated to the current log file open in Mode
%
% @param Mode File descriptor required mode
% @param FileDescriptor An open file descriptor
% @copyright 2kodevs 2019-2020
file_descriptor(Mode, FD):-
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
    writeln(FD, "See us at https://github.com/2kodevs/"),
    nl(FD), close(FD).
