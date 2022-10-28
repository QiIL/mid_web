%% -------------------------------------------------------------------
%%
%% Copyright (c) 2011-2017 Basho Technologies, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc File backend for lager, with multiple file support.
%% Multiple files are supported, each with the path and the loglevel being
%% configurable. The configuration paramter for this backend is a list of
%% key-value 2-tuples. See the init() function for the available options.
%% This backend supports external and internal log
%% rotation and will re-open handles to files if the inode changes. It will
%% also rotate the files itself if the size of the file exceeds the
%% `size' and keep `count' rotated files. `date' is
%% an alternate rotation trigger, based on time. See the README for
%% documentation.
%% For performance, the file backend does delayed writes, although it will
%% sync at specific log levels, configured via the `sync_on' option. By default
%% the error level or above will trigger a sync.

-module(wlog_backend).

-include_lib("lager/include/lager.hrl").
-include_lib("kernel/include/file.hrl").

-behaviour(gen_event).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([{parse_transform, lager_transform}]).
-endif.

-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2,
    code_change/3]).

-export([config_to_id/1]).

-define(DEFAULT_LOG_LEVEL, info).
-define(DEFAULT_ROTATION_DATE, "$D0"). %% midnight
-define(DEFAULT_ROTATION_MOD, lager_rotator_default).
-define(DEFAULT_SYNC_LEVEL, error).
-define(DEFAULT_SYNC_INTERVAL, 1000).
-define(DEFAULT_SYNC_SIZE, 1024*64). %% 64kb
-define(DEFAULT_CHECK_INTERVAL, 1000).

-record(state, {
    name :: string(),
    level :: {'mask', integer()},
    fd :: file:io_device() | undefined,
    inode :: integer() | undefined,
    ctime :: file:date_time() | undefined,
    flap = false :: boolean(),
    date :: undefined | string(),
    rotator = lager_util :: atom(),
    shaper :: lager_shaper(),
    formatter :: atom(),
    formatter_config :: any(),
    sync_on :: {'mask', integer()},
    check_interval = ?DEFAULT_CHECK_INTERVAL :: non_neg_integer(),
    sync_interval = ?DEFAULT_SYNC_INTERVAL :: non_neg_integer(),
    sync_size = ?DEFAULT_SYNC_SIZE :: non_neg_integer(),
    last_check = os:timestamp() :: erlang:timestamp(),
    os_type :: atom()
}).

-type option() :: {file, string()} | {level, lager:log_level()} |
{high_water_mark, non_neg_integer()} |
{flush_queue, boolean()} |
{flush_threshold, non_neg_integer()} |
{sync_interval, non_neg_integer()} |
{sync_size, non_neg_integer()} | {sync_on, lager:log_level()} |
{check_interval, non_neg_integer()} | {formatter, atom()} |
{formatter_config, term()}.

-spec init([option(),...]) -> {ok, #state{}} | {error, {fatal,bad_config}}.
init({FileName, LogLevel}) when is_list(FileName), is_atom(LogLevel) ->
    %% backwards compatibility hack
    init([{file, FileName}, {level, LogLevel}]);
init({FileName, LogLevel, Date, Count}) when is_list(FileName), is_atom(LogLevel) ->
    %% backwards compatibility hack
    init([{file, FileName}, {level, LogLevel}, {date, Date}, {count, Count}]);
init([{FileName, LogLevel, Date, Count}, {Formatter,FormatterConfig}]) when is_list(FileName), is_atom(LogLevel), is_atom(Formatter) ->
    %% backwards compatibility hack
    init([{file, FileName}, {level, LogLevel}, {date, Date}, {count, Count}, {formatter, Formatter}, {formatter_config, FormatterConfig}]);
init([LogFile,{Formatter}]) ->
    %% backwards compatibility hack
    init([LogFile,{Formatter,[]}]);
init([{FileName, LogLevel}, {Formatter,FormatterConfig}]) when is_list(FileName), is_atom(LogLevel), is_atom(Formatter) ->
    %% backwards compatibility hack
    init([{file, FileName}, {level, LogLevel}, {formatter, Formatter}, {formatter_config, FormatterConfig}]);
init(LogFileConfig) when is_list(LogFileConfig) ->
    case validate_logfile_proplist(LogFileConfig) of
        false ->
            %% falied to validate config
            {error, {fatal, bad_config}};
        Config ->
            %% probabably a better way to do this, but whatever
            [RelName, Level, Date, Rotator, HighWaterMark, Flush, SyncInterval, SyncSize, SyncOn, CheckInterval, Formatter, FormatterConfig] =
                [proplists:get_value(Key, Config) || Key <- [file, level, date, rotator, high_water_mark, flush_queue, sync_interval, sync_size, sync_on, check_interval, formatter, formatter_config]],
            FlushThr = proplists:get_value(flush_threshold, Config, 0),
            Name = lager_util:expand_path(RelName),
            schedule_rotation(Name, Date),
            Shaper = lager_util:maybe_flush(Flush, #lager_shaper{hwm=HighWaterMark, flush_threshold = FlushThr, id=Name}),
            State0 = #state{name=Name, level=Level,  date=Date, rotator=Rotator,
                shaper=Shaper, formatter=Formatter, formatter_config=FormatterConfig,
                sync_on=SyncOn, sync_interval=SyncInterval, sync_size=SyncSize, check_interval=CheckInterval},
            State = case Rotator:create_logfile(Name, {SyncSize, SyncInterval}) of
                {ok, {FD, Inode, Ctime}} ->
                    State0#state{fd=FD, inode=Inode, ctime=Ctime};
                {error, Reason} ->
                    ?INT_LOG(error, "Failed to open log file ~ts with error ~s", [Name, file:format_error(Reason)]),
                    State0#state{flap=true}
            end,
            {ok, State}
    end.

%% @private
handle_call({set_loglevel, Level}, #state{name=Ident} = State) ->
    case validate_loglevel(Level) of
        false ->
            {ok, {error, bad_loglevel}, State};
        Levels ->
            ?INT_LOG(notice, "Changed loglevel of ~s to ~p", [Ident, Level]),
            {ok, ok, State#state{level=Levels}}
    end;
handle_call(get_loglevel, #state{level=Level} = State) ->
    {ok, Level, State};
handle_call({set_loghwm, Hwm}, #state{shaper=Shaper, name=Name} = State) ->
    case validate_logfile_proplist([{file, Name}, {high_water_mark, Hwm}]) of
        false ->
            {ok, {error, bad_log_hwm}, State};
        _ ->
            NewShaper = Shaper#lager_shaper{hwm=Hwm},
            ?INT_LOG(notice, "Changed loghwm of ~ts to ~p", [Name, Hwm]),
            {ok, {last_loghwm, Shaper#lager_shaper.hwm}, State#state{shaper=NewShaper}}
    end;
handle_call(rotate, State = #state{name=File}) ->
    {ok, NewState} = handle_info({rotate, File}, State),
    {ok, ok, NewState};
handle_call(_Request, State) ->
    {ok, ok, State}.

%% @private
handle_event({log, Message},
    #state{name=Name, level=L, shaper=Shaper, formatter=Formatter,formatter_config=FormatConfig} = State) ->
    case lager_util:is_loggable(Message,L,{lager_file_backend, Name}) of
        true ->
            case lager_util:check_hwm(Shaper) of
                {true, Drop, #lager_shaper{hwm=Hwm} = NewShaper} ->
                    NewState = case Drop > 0 of
                        true ->
                            Report = io_lib:format(
                                "lager_file_backend dropped ~p messages in the last second that exceeded the limit of ~p messages/sec",
                                [Drop, Hwm]),
                            ReportMsg = lager_msg:new(Report, warning, [], []),
                            write(State, lager_msg:timestamp(ReportMsg),
                                lager_msg:severity_as_int(ReportMsg), Formatter:format(ReportMsg, FormatConfig));
                        false ->
                            State
                    end,
                    {ok,write(NewState#state{shaper=NewShaper},
                        lager_msg:timestamp(Message), lager_msg:severity_as_int(Message),
                        Formatter:format(Message,FormatConfig))};
                {false, _, #lager_shaper{dropped=D} = NewShaper} ->
                    {ok, State#state{shaper=NewShaper#lager_shaper{dropped=D+1}}}
            end;
        false ->
            {ok, State}
    end;
handle_event(_Event, State) ->
    {ok, State}.

%% @private
handle_info({rotate, File}, #state{name=File, date=Date, rotator=Rotator}=State0) ->
    ?lager_info("rotate file: ~p~n", [State0]),
    State1 = close_file(State0),
    _ = Rotator:rotate_logfile(File, Date),
    schedule_rotation(File, Date),
    {ok, State1};
handle_info({shaper_expired, Name}, #state{shaper=Shaper, name=Name, formatter=Formatter, formatter_config=FormatConfig} = State) ->
    _ = case Shaper#lager_shaper.dropped of
        0 ->
            ok;
        Dropped ->
            Report = io_lib:format(
                "lager_file_backend dropped ~p messages in the last second that exceeded the limit of ~p messages/sec",
                [Dropped, Shaper#lager_shaper.hwm]),
            ReportMsg = lager_msg:new(Report, warning, [], []),
            write(State, lager_msg:timestamp(ReportMsg),
                lager_msg:severity_as_int(ReportMsg), Formatter:format(ReportMsg, FormatConfig))
    end,
    {ok, State#state{shaper=Shaper#lager_shaper{dropped=0, mps=0, lasttime=os:timestamp()}}};
handle_info(_Info, State) ->
    {ok, State}.

%% @private
terminate(_Reason, State) ->
    %% leaving this function call unmatched makes dialyzer cranky
    _ = close_file(State),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Convert the config into a gen_event handler ID
config_to_id({Name,_Severity}) when is_list(Name) ->
    {?MODULE, Name};
config_to_id({Name,_Severity,_Rotation,_Count}) ->
    {?MODULE, Name};
config_to_id([{Name,_Severity,_Rotation,_Count}, _Format]) ->
    {?MODULE, Name};
config_to_id([{Name,_Severity}, _Format]) when is_list(Name) ->
    {?MODULE, Name};
config_to_id(Config) ->
    case proplists:get_value(file, Config) of
        undefined ->
            erlang:error(no_file);
        File ->
            {?MODULE, File}
    end.

write(#state{name=Name, fd=FD,
    inode=Inode, ctime=Ctime,
    flap=Flap, rotator=Rotator}=State0, Timestamp, Level, Msg) ->
    case write_should_check(State0, Timestamp) of
        true ->
            %% need to check for rotation
            Buffer = {State0#state.sync_size, State0#state.sync_interval},
            case Rotator:ensure_logfile(Name, FD, Inode, Ctime, Buffer) of
                {ok, {NewFD, NewInode, NewCtime}} ->
                    %% update our last check and try again
                    State1 = State0#state{last_check=Timestamp, fd=NewFD, inode=NewInode, ctime=NewCtime},
                    do_write(State1, Level, Msg);
                {error, Reason} ->
                    case Flap of
                        true ->
                            State0;
                        _ ->
                            ?INT_LOG(error, "Failed to reopen log file ~ts with error ~s", [Name, file:format_error(Reason)]),
                            State0#state{flap=true}
                    end
            end;
        false ->
            do_write(State0, Level, Msg)
    end.

write_should_check(#state{fd=undefined}, _Timestamp) ->
    true;
write_should_check(#state{last_check=LastCheck0, check_interval=CheckInterval,
    name=Name, inode=Inode0, ctime=Ctime0}, Timestamp) ->
    LastCheck1 = timer:now_diff(Timestamp, LastCheck0) div 1000,
    case LastCheck1 >= CheckInterval of
        true ->
            true;
        _ ->
            % We need to know if the file has changed "out from under lager" so we don't
            % write to an invalid FD
            {Result, _FInfo} = lager_util:has_file_changed(Name, Inode0, Ctime0),
            Result
    end.

do_write(#state{fd=FD, name=Name, flap=Flap} = State, Level, Msg) ->
    %% delayed_write doesn't report errors
    _ = file:write(FD, unicode:characters_to_binary(Msg)),
    {mask, SyncLevel} = State#state.sync_on,
    case (Level band SyncLevel) =/= 0 of
        true ->
            %% force a sync on any message that matches the 'sync_on' bitmask
            Flap2 = case file:datasync(FD) of
                {error, Reason2} when Flap == false ->
                    ?INT_LOG(error, "Failed to write log message to file ~ts: ~s",
                        [Name, file:format_error(Reason2)]),
                    true;
                ok ->
                    false;
                _ ->
                    Flap
            end,
            State#state{flap=Flap2};
        _ ->
            State
    end.

validate_loglevel(Level) ->
    try lager_util:config_to_mask(Level) of
        Levels ->
            Levels
    catch
        _:_ ->
            false
    end.

validate_logfile_proplist(List) ->
    try validate_logfile_proplist(List, []) of
        Res ->
            case proplists:get_value(file, Res) of
                undefined ->
                    ?INT_LOG(error, "Missing required file option", []),
                    false;
                _File ->
                    %% merge with the default options
                    {ok, DefaultRotationDate} = lager_util:parse_rotation_date_spec(?DEFAULT_ROTATION_DATE),
                    lists:keymerge(1, lists:sort(Res), lists:sort([
                        {level, validate_loglevel(?DEFAULT_LOG_LEVEL)}, {date, DefaultRotationDate},
%%                        {size, ?DEFAULT_ROTATION_SIZE}, {count, ?DEFAULT_ROTATION_COUNT},
                        {rotator, ?DEFAULT_ROTATION_MOD},
                        {sync_on, validate_loglevel(?DEFAULT_SYNC_LEVEL)}, {sync_interval, ?DEFAULT_SYNC_INTERVAL},
                        {sync_size, ?DEFAULT_SYNC_SIZE}, {check_interval, ?DEFAULT_CHECK_INTERVAL},
                        {formatter, lager_default_formatter}, {formatter_config, []}
                    ]))
            end
    catch
        {bad_config, Msg, Value} ->
            ?INT_LOG(error, "~s ~p for file ~tp",
                [Msg, Value, proplists:get_value(file, List)]),
            false
    end.

validate_logfile_proplist([], Acc) ->
    Acc;
validate_logfile_proplist([{file, File}|Tail], Acc) ->
    %% is there any reasonable validation we can do here?
    validate_logfile_proplist(Tail, [{file, File}|Acc]);
validate_logfile_proplist([{level, Level}|Tail], Acc) ->
    case validate_loglevel(Level) of
        false ->
            throw({bad_config, "Invalid loglevel", Level});
        Res ->
            validate_logfile_proplist(Tail, [{level, Res}|Acc])
    end;
validate_logfile_proplist([{rotator, Rotator}|Tail], Acc) ->
    case is_atom(Rotator) of
        true ->
            validate_logfile_proplist(Tail, [{rotator, Rotator}|Acc]);
        false ->
            throw({bad_config, "Invalid rotation module", Rotator})
    end;
validate_logfile_proplist([{high_water_mark, HighWaterMark}|Tail], Acc) ->
    case HighWaterMark of
        Hwm when is_integer(Hwm), Hwm >= 0 ->
            validate_logfile_proplist(Tail, [{high_water_mark, Hwm}|Acc]);
        _ ->
            throw({bad_config, "Invalid high water mark", HighWaterMark})
    end;
validate_logfile_proplist([{date, Date}|Tail], Acc) ->
    case lager_util:parse_rotation_date_spec(Date) of
        {ok, Spec} ->
            validate_logfile_proplist(Tail, [{date, Spec}|Acc]);
        {error, _} when Date == "" ->
            %% legacy config allowed blanks
            validate_logfile_proplist(Tail, [{date, undefined}|Acc]);
        {error, _} ->
            throw({bad_config, "Invalid rotation date", Date})
    end;
validate_logfile_proplist([{sync_interval, SyncInt}|Tail], Acc) ->
    case SyncInt of
        Val when is_integer(Val), Val >= 0 ->
            validate_logfile_proplist(Tail, [{sync_interval, Val}|Acc]);
        _ ->
            throw({bad_config, "Invalid sync interval", SyncInt})
    end;
validate_logfile_proplist([{sync_size, SyncSize}|Tail], Acc) ->
    case SyncSize of
        Val when is_integer(Val), Val >= 0 ->
            validate_logfile_proplist(Tail, [{sync_size, Val}|Acc]);
        _ ->
            throw({bad_config, "Invalid sync size", SyncSize})
    end;
validate_logfile_proplist([{check_interval, CheckInt}|Tail], Acc) ->
    case CheckInt of
        Val when is_integer(Val), Val >= 0 ->
            validate_logfile_proplist(Tail, [{check_interval, Val}|Acc]);
        always ->
            validate_logfile_proplist(Tail, [{check_interval, 0}|Acc]);
        _ ->
            throw({bad_config, "Invalid check interval", CheckInt})
    end;
validate_logfile_proplist([{sync_on, Level}|Tail], Acc) ->
    case validate_loglevel(Level) of
        false ->
            throw({bad_config, "Invalid sync on level", Level});
        Res ->
            validate_logfile_proplist(Tail, [{sync_on, Res}|Acc])
    end;
validate_logfile_proplist([{formatter, Fmt}|Tail], Acc) ->
    case is_atom(Fmt) of
        true ->
            validate_logfile_proplist(Tail, [{formatter, Fmt}|Acc]);
        false ->
            throw({bad_config, "Invalid formatter module", Fmt})
    end;
validate_logfile_proplist([{formatter_config, FmtCfg}|Tail], Acc) ->
    case is_list(FmtCfg) of
        true ->
            validate_logfile_proplist(Tail, [{formatter_config, FmtCfg}|Acc]);
        false ->
            throw({bad_config, "Invalid formatter config", FmtCfg})
    end;
validate_logfile_proplist([{flush_queue, FlushCfg}|Tail], Acc) ->
    case is_boolean(FlushCfg) of
        true ->
            validate_logfile_proplist(Tail, [{flush_queue, FlushCfg}|Acc]);
        false ->
            throw({bad_config, "Invalid queue flush flag", FlushCfg})
    end;
validate_logfile_proplist([{flush_threshold, Thr}|Tail], Acc) ->
    case Thr of
        _ when is_integer(Thr), Thr >= 0 ->
            validate_logfile_proplist(Tail, [{flush_threshold, Thr}|Acc]);
        _ ->
            throw({bad_config, "Invalid queue flush threshold", Thr})
    end;
validate_logfile_proplist([Other|_Tail], _Acc) ->
    throw({bad_config, "Invalid option", Other}).

schedule_rotation(_, undefined) ->
    ok;
schedule_rotation(Name, Date) ->
    erlang:send_after(lager_util:calculate_next_rotation(Date) * 1000, self(), {rotate, Name}),
    ok.

close_file(#state{fd=undefined} = State) ->
    State;
close_file(#state{fd=FD} = State) ->
    %% Flush and close any file handles.
    %% delayed write can cause file:close not to do a close
    _ = file:datasync(FD),
    _ = file:close(FD),
    _ = file:close(FD),
    State#state{fd=undefined}.
