%%%-------------------------------------------------------------------
%%% @doc 系统相关
%%%-------------------------------------------------------------------
-module(wlib_sys).

%% API
-export([
    get_stacktrace/0
]).


%% @doc 查看当前进程当前的stacktrace
-spec get_stacktrace() -> tuple() | undefined.
get_stacktrace()->
    get_stacktrace(erlang:self()).

%% @doc 查看指定进程当前的stacktrace
-spec get_stacktrace(pid() | atom()) -> tuple() | undefined .
get_stacktrace(PID) when erlang:is_pid(PID) ->
    case erlang:process_info(PID, current_stacktrace) of
        {current_stacktrace, [_|Stacktrace]} ->
            {current_stacktrace, Stacktrace};
        Err -> Err
    end;
get_stacktrace(PName) when erlang:is_atom(PName) ->
    case erlang:whereis(PName) of
        PID when erlang:is_pid(PID) ->
            get_stacktrace(PID);
        _ -> undefined
    end;
get_stacktrace(_Other)->
    undefined.
