%%%-------------------------------------------------------------------
%%% @doc Web time tool for local time
%%% 所有的时间戳都没有时区
%%% 所有的datetime都有时区
%%%-------------------------------------------------------------------
-module(wtime).
-include("wtime.hrl").

%% Get time api
-export([
    now/0,
    midnight/0,
    midnight/1,
    nextnight/0,
    timestamp/0,
    timestamp/1,
    date/0,
    date/1,
    local_time/0,
    datetime/0,
    datetime/1,
    datetime_str/0,
    datetime_str/1,
    hour_min_sec_str/0,
    now_os/0,
    now_ms/0,
    now_us/0
]).

%% Calc time api
-export([
    diff/2,
    weekday/0,
    weekday/1,
    month_day/0
]).

-type year() :: integer().
-type month() :: 1..12.
-type day() :: 1..31.
-type hour() :: 0..23.
-type minute() :: 0..59.
-type second() :: 0..59.
%%-type week() :: 1..7.
-type date() :: {year(), month(), day()}.
-type time() :: {hour(), minute(), second()}.
-type datetime() :: {date(), time()}.
%%-type daily_time() :: 0..86399.

%% =================================== Get time ==============================
-spec now() -> integer().
%% @doc 秒时间戳:UTC
now() ->
    case erlang:get(now) of
        undefined -> now_os();
        Sec -> Sec
    end.

-spec midnight() -> integer().
%% @doc 今天凌晨0点时间戳
midnight() ->
    midnight(wtime:now()).

-spec midnight(TimeStamp :: integer()) -> integer().
%% @doc 计算时间戳TimeStamp当天的0点时间戳
midnight(TimeStamp) ->
    TimeStamp - (TimeStamp + ?GREGORIAN_BEGIN_TIME) rem ?DAY_SECOND.

-spec nextnight() -> integer().
%% @doc 明天凌晨0点时间戳
nextnight() ->
    midnight() + ?DAY_SECOND.

-spec now_os() -> integer().
%% @doc 秒时间戳:UTC
now_os() ->
    erlang:system_time(second).

-spec now_ms() -> integer().
%% @doc 毫秒:UTC
now_ms() ->
    erlang:system_time(millisecond).

-spec now_us() -> integer().
%% @doc 微秒:UTC
now_us() ->
    erlang:system_time(microsecond).

-spec timestamp() -> integer().
timestamp() ->
    timestamp(wtime:datetime()).
-spec timestamp(DateTime :: date() | time() | datetime() ) -> integer().
%% @doc 某个时间点的时间戳:UTC
timestamp({Y, M, D}) ->
    timestamp({{Y, M, D}, {0, 0, 0}});
timestamp(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - ?GREGORIAN_BEGIN_TIME.

-spec date() -> date().
date() ->
    {Date, _} = wtime:datetime(),
    Date.

-spec date(Timestamp :: non_neg_integer()) -> date().
date(Timestamp) ->
    {Date, _} = wtime:datetime(Timestamp),
    Date.

-spec local_time() -> datetime().
local_time() ->
    calendar:local_time().

-spec datetime() -> datetime().
datetime() ->
    calendar:local_time().

-spec datetime(TimeStamp :: non_neg_integer()) -> datetime().
datetime(Timestamp) ->
    calendar:gregorian_seconds_to_datetime(Timestamp + ?GREGORIAN_BEGIN_TIME).

-spec datetime_str() -> string().
%% @doc 当前日期时间，格式如"2022-3-14 15:9:26"
datetime_str() ->
    datetime_str(wtime:local_time()).

-spec datetime_str(Time :: integer()) -> string().
%% @doc 时间戳转换为日期时间格式，结果格式:"2022-3-14 15:9:26"
datetime_str(Time) when erlang:is_integer(Time) ->
    datetime_str(datetime(Time));
datetime_str({{Y, M, D}, {HH, MM, SS}}) ->
    lists:flatten(io_lib:format("~w-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Y, M, D, HH, MM, SS])).

-spec hour_min_sec_str() -> string().
%% @doc 获取当前时间的时分秒格式:"HH:MM:SS"
hour_min_sec_str() ->
    {{_Y, _M, _D}, {HH, MM, SS}} = datetime(wtime:now()),
    lists:flatten(io_lib:format("~2..0B:~2..0B:~2..0B", [HH, MM, SS])).

%% ============================ calc time ================================
-spec diff(A :: date() | datetime() | non_neg_integer(), B :: date() | datetime() | non_neg_integer()) -> integer().
%% A > B 返回正数 
%% B > A 返回负数 
diff({_, _, _}=A, {_, _, _}=B) ->
    diff(calendar:datetime_to_gregorian_seconds({A, {0, 0, 0}}), calendar:datetime_to_gregorian_seconds({B, {0, 0, 0}}));
diff({_, _}=A, {_, _}=B) ->
    diff(calendar:datetime_to_gregorian_seconds(A), calendar:datetime_to_gregorian_seconds(B));
diff(A, B) when erlang:is_integer(A) andalso erlang:is_integer(B) ->
    A - B.

-spec weekday() -> integer().
weekday() ->
    weekday(wtime:datetime()).
-spec weekday(Time :: date() | datetime() | non_neg_integer()) -> integer().
weekday({Y, M, D}) ->
    calendar:day_of_the_week(Y, M, D);
weekday({{Y, M, D}, {_, _, _}}) ->
    calendar:day_of_the_week(Y, M, D);
weekday(Timestamp) ->
    calendar:day_of_the_week(wtime:date(Timestamp)).

month_day() ->
    {_, _, D} = wtime:datetime(),
    D.

