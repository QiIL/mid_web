-module(user_default).

-compile(nowarn_export_all).
-compile([export_all]).



%% 函数调用
wr(M, F) ->
    wlib_trace:watch_return(M, F).
wr(M, F, A) ->
    wlib_trace:watch_return(M, F, A).
%% A = 参数个数, P = PID
wr(P, M, F, A) ->
    wlib_trace:watch_return(P, M, F, A).

%% watch_filter( world_cache_server, do_loop_sec, dbg:fun2ms(fun([A]) when (A rem 2) == 0 -> true end) ).
wrf(M, F, MatchSpec) ->
    wlib_trace:watch_filter(M, F, MatchSpec).

wra(Mod) ->
    L = proplists:get_value(exports, Mod:module_info()),
    lists:foreach(fun({Fun, _}) -> wr(Mod, Fun) end, L).

uw() ->
    wlib_trace:unwatch_all(true).
uw(M, F) ->
    wlib_trace:unwatch(M, F).

