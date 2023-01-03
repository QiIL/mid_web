-module(trace).
-include_lib("stdlib/include/ms_transform.hrl").
%% API
-export([
    watch/2,
    watch/3,
    watch/4
]).

%% test
-export([
    start_test_tracer/0, 
    test_tracer_loop/0, 
    start_test_loop_server/0, 
    test_loop/0,
    test_add/2,
    test_add/3,
    test_divide/2,
    test_divide/3,
    test_multiply/2,
    test_multiply/3
]).

watch(M, F) ->
    watch(all, M, F, '_', match_spec()).
watch(PID, M, F) when is_pid(PID) orelse PID=:=all  orelse PID=:=new orelse PID=:=existing ->
    watch(PID, M, F, '_', match_spec());
watch(M, F, A) ->
    watch(all, M, F, A, match_spec()).
watch(PID, M, F, A) ->
    watch(PID, M, F, A, match_spec()).
watch(PID, M, F, A, MatchSpec) ->
    watch(PID, M, F, A, MatchSpec).

match_spec() ->
    dbg:fun2ms(fun(_) -> message(caller()), return_trace(), exception_trace() end).

start_test_tracer() -> 
    spawn(fun() -> ?MODULE:test_tracer_loop() end).

test_tracer_loop() ->
    receive
        {trace, Pid, send, Msg, To} ->
            % 处理send
            io:format("Process ~p sent message ~p to process ~p~n", [Pid, Msg, To]);
        {trace, Pid, 'receive', Msg} ->
            % 处理receive
            io:format("Process ~p received message ~p~n", [Pid, Msg]);
        {trace, Pid, return_from, {Module, Function, Arity}, ReturnValue} ->
            % 处理返回值
            io:format("Function ~p:~p/~p returned ~p in process ~p~n",
                [Module, Function, Arity, ReturnValue, Pid]);
        {trace, Pid, call, {Module, Function, Arguments}} ->
            % 处理调用
            io:format("Function ~p:~p invoked with arguments ~p in process ~p~n",
                [Module, Function, Arguments, Pid]);
        Msg ->
            io:format("Received unexpected message ~p~n", [Msg])
    end.

%%core() ->
%%    erlang:trace(PidSpec, true, [call,{tracer,TPid}]),
%%    erlang:trace_pattern({Mod,Func,Arity}, MatchSpec, [local]),
%%    ok.


test_add(ServerPid, X, Y) ->
    test_call(ServerPid, {add, X, Y}).

test_divide(ServerPid, X, Y) ->
    test_call(ServerPid, {divide, X, Y}).

test_multiply(ServerPid, X, Y) ->
    test_call(ServerPid, {multiply, X, Y}).

start_test_loop_server() ->
    spawn(fun() -> ?MODULE:test_loop() end).

% Server loop
test_loop() ->
    receive
        {Pid, {add, X, Y}} ->
            Pid ! test_add(X, Y);
        {Pid, {divide, X, Y}} ->
            Pid ! test_divide(X, Y);
        {Pid, {multiply, X, Y}} ->
            Pid ! test_multiply(X, Y)
    end,
    test_tracer_loop().

% Private functions
test_call(ServerPid, Msg) ->
    ServerPid ! {self(), Msg},
    receive
        RespMsg -> RespMsg
    end.

test_add(X, Y) ->
    io:format("Got here~n"),
    X + Y.

test_divide(X, Y) ->
    X / Y.

test_multiply(X, Y) ->
    % intentionally naive multiply
    Result = lists:foldl(fun(_, Seq) ->
        X + Seq
    end, 0, lists:seq(1, Y)),
    Result.
