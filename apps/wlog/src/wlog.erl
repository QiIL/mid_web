%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @doc 
%%%-------------------------------------------------------------------
-module(wlog).
-include("wlog.hrl").
%% API
-export([
    init/0,
    get_level/0,
    get_num/0,
    set_log_level/1
]).

-export([
    debug_msg/1,
    debug_msg/2,
    info_msg/1,
    info_msg/2,
    notice_msg/1,
    notice_msg/2,
    warning_msg/1,
    warning_msg/2,
    error_msg/1,
    error_msg/2,
    critical_msg/1,
    critical_msg/2,
    alert_msg/1,
    alert_msg/2,
    emergency_msg/1,
    emergency_msg/2
]).

-define(LAGER_BACKEND, wlog_backend).
-define(LAGER_LOGFILE, "info.log").

init() ->
    {ok, LogLevel} = application:get_env(wlog, log_level),
    set_log_level(LogLevel).

get_level() ->
    {ok, Level} = application:get_env(wlog, log_level),
    Level.

get_num() ->
    {ok, Num} = application:get_env(wlog, log_level_num),
    Num.

set_log_level(Level) -> 
    case lists:member(Level, lager_util:levels()) of
        true ->
            lager:set_loglevel(?LAGER_BACKEND, ?LAGER_LOGFILE, Level),
            application:set_env(wlog, log_level, lager_util:level_to_num(Level)),
            application:set_env(wlog, log_level_num, lager_util:level_to_num(Level));
        _ ->
            ignore
    end.

debug_msg(Expr) -> ?DEBUG_MSG(Expr).
debug_msg(Expr, Args) -> ?DEBUG_MSG(Expr, Args).
info_msg(Expr) -> ?INFO_MSG(Expr).
info_msg(Expr, Args) -> ?INFO_MSG(Expr, Args).
notice_msg(Expr) -> ?NOTICE_MSG(Expr).
notice_msg(Expr, Args) -> ?NOTICE_MSG(Expr, Args).
warning_msg(Expr) -> ?WARNING_MSG(Expr).
warning_msg(Expr, Args) -> ?WARNING_MSG(Expr, Args).
error_msg(Expr) -> ?ERROR_MSG(Expr).
error_msg(Expr, Args) -> ?ERROR_MSG(Expr, Args).
critical_msg(Expr) -> ?CRITICAL_MSG(Expr).
critical_msg(Expr, Args) -> ?CRITICAL_MSG(Expr, Args).
alert_msg(Expr) -> ?ALERT_MSG(Expr).
alert_msg(Expr, Args) -> ?ALERT_MSG(Expr, Args).
emergency_msg(Expr) -> ?EMERGENCY_MSG(Expr).
emergency_msg(Expr, Args) -> ?EMERGENCY_MSG(Expr, Args).

