-module(wlib_recon_trace).
-include_lib("stdlib/include/ms_transform.hrl").
%% API
-export([
    watch/1,
    watch/2,
    watch/3,
    watch/4,
    clear/0,
    clear/1,
    clear/2
]).

%% ===================== test recon_trace =====================================
watch(Mod) ->
    watch(Mod, '_', all, {50, 100}).

watch(Mod, Max) when is_integer(Max) ->
    watch(Mod, '_', all, Max);
watch(Mod, {_, _}=Max) ->
    watch(Mod, '_', all, Max);
watch(Mod, Func) ->
    watch(Mod, Func, all, {50, 100}).

watch(Mod, Func, {_, _}=Max) ->
    watch(Mod, Func, all, Max);
watch(Mod, Func, Max) when is_integer(Max) ->
    watch(Mod, Func, all, Max);
watch(Mod, Func, PID) when is_pid(PID) orelse PID=:=all orelse PID=:=new orelse PID=:=existing ->
    watch(Mod, Func, PID, {50, 100}).

watch(Mod, Func, PID, Max) ->
    recon_trace:calls({Mod, Func, match_spec()}, Max, [{scope, local}, {formatter, fun format/1}, {pid, PID}]).

match_spec() ->
    dbg:fun2ms(fun(_) -> message(caller()), return_trace(), exception_trace() end).

clear() ->
    recon_trace:clear().

clear(Mod) ->
    erlang:trace_pattern({Mod,'_','_'}, false, [local,meta,call_count,call_time]),
    erlang:trace_pattern({Mod,'_','_'}, false, []), 
    ok.

clear(Mod, Func) ->
    erlang:trace_pattern({Mod,Func,'_'}, false, [local,meta,call_count,call_time]),
    erlang:trace_pattern({Mod,Func,'_'}, false, []),
    ok.
%% copy from recon_trace:format/1
format(TraceMsg) ->
    {Type, Pid, {_, {Hour,Min,Sec}}, TraceInfo} = extract_info(TraceMsg),
    {HeadStr, FormatStr, FormatArgs} = case {Type, TraceInfo} of
        %% {trace, Pid, 'receive', Msg}
        {'receive', [Msg]} ->
            {"==Recv=======", "< ~99999p", [Msg]};
        %% {trace, Pid, send, Msg, To}
        {send, [Msg, To]} ->
            {"==Send=======", " > ~99999p: ~99999p", [To, Msg]};
        %% {trace, Pid, send_to_non_existing_process, Msg, To}
        {send_to_non_existing_process, [Msg, To]} ->
            {"==Send=======", " > (non_existent) ~99999p: ~99999p", [To, Msg]};
        %% {trace, Pid, call, {M, F, Args}}
        {call, [{M,F,Args}]} ->
            {"==Call=======", "~99999p:~99999p/~99999p <-- ~99999p", [M,F,erlang:length(Args),Args]};
        %% {trace, Pid, call, {M, F, Args}, Msg}
        {call, [{M,F,Args}, Msg]} ->
            {"==Call=======", "~99999p:~99999p/~99999p <-- ~99999p @~99999p", [M,F,erlang:length(Args),Args, Msg]};
        %% {trace, Pid, return_to, {M, F, Arity}}
        {return_to, [{M,F,Arity}]} ->
            {"==Return To==", " '--> ~99999p:~99999p/~99999p", [M,F,Arity]};
        %% {trace, Pid, return_from, {M, F, Arity}, ReturnValue}
        {return_from, [{M,F,Arity}, Return]} ->
            {"==Return=====", "~99999p:~99999p/~99999p --> ~99999p", [M,F,Arity, Return]};
        %% {trace, Pid, exception_from, {M, F, Arity}, {Class, Value}}
        {exception_from, [{M,F,Arity}, {Class,Val}]} ->
            {"==Exception!=", "~99999p:~99999p/~99999p {~99999p,~99999p}", [M,F,Arity, Class, Val]};
        %% {trace, Pid, spawn, Spawned, {M, F, Args}}
        {spawn, [Spawned, {M,F,Args}]}  ->
            {"==Spawn======", "spawned ~99999p as ~99999p:~99999p~99999p", [Spawned, M, F, Args]};
        %% {trace, Pid, exit, Reason}
        {exit, [Reason]} ->
            {"==Exit!======", "EXIT ~99999p", [Reason]};
        %% {trace, Pid, link, Pid2}
        {link, [Linked]} ->
            {"==Link=======", "link(~99999p)", [Linked]};
        %% {trace, Pid, unlink, Pid2}
        {unlink, [Linked]} ->
            {"==Unlink=====", "unlink(~99999p)", [Linked]};
        %% {trace, Pid, getting_linked, Pid2}
        {getting_linked, [Linker]} ->
            {"==Get Link===", "getting linked by ~99999p", [Linker]};
        %% {trace, Pid, getting_unlinked, Pid2}
        {getting_unlinked, [Unlinker]} ->
            {"==Get Unlink=", "getting unlinked by ~99999p", [Unlinker]};
        %% {trace, Pid, register, RegName}
        {register, [Name]} ->
            {"==Register===", "registered as ~99999p", [Name]};
        %% {trace, Pid, unregister, RegName}
        {unregister, [Name]} ->
            {"==Register===", "no longer registered as ~99999p", [Name]};
        %% {trace, Pid, in, {M, F, Arity} | 0}
        {in, [{M,F,Arity}]} ->
            {"==In=========", "scheduled in for ~99999p:~99999p/~99999p", [M,F,Arity]};
        {in, [0]} ->
            {"==In=========", "scheduled in", []};
        %% {trace, Pid, out, {M, F, Arity} | 0}
        {out, [{M,F,Arity}]} ->
            {"==Out========", "scheduled out from ~99999p:~99999p/~99999p", [M, F, Arity]};
        {out, [0]} ->
            {"==Out========", "scheduled out", []};
        %% {trace, Pid, gc_start, Info}
        {gc_start, [Info]} ->
            HeapSize = proplists:get_value(heap_size, Info),
            OldHeapSize = proplists:get_value(old_heap_size, Info),
            MbufSize = proplists:get_value(mbuf_size, Info),
            {"==GC Start===", "gc beginning -- heap ~99999p bytes",
                [HeapSize + OldHeapSize + MbufSize]};
        %% {trace, Pid, gc_end, Info}
        {gc_end, [Info]} ->
            HeapSize = proplists:get_value(heap_size, Info),
            OldHeapSize = proplists:get_value(old_heap_size, Info),
            MbufSize = proplists:get_value(mbuf_size, Info),
            {"==GC End=====", "gc finished -- heap ~99999p bytes",
                [HeapSize + OldHeapSize + MbufSize]};
        _ ->
            {"==Unknown====", "unknown trace type ~99999p -- ~99999p", [Type, TraceInfo]}
    end,
    io_lib:format(HeadStr ++ "~99999p:~99999p:~9.6.0f ~99999p " ++ FormatStr ++ "~n",
        [Hour, Min, Sec, Pid] ++ FormatArgs).

extract_info(TraceMsg) ->
    case tuple_to_list(TraceMsg) of
        [trace_ts, Pid, Type | Info] ->
            {TraceInfo, [Timestamp]} = lists:split(length(Info)-1, Info),
            {Type, Pid, to_hms(Timestamp), TraceInfo};
        [trace, Pid, Type | TraceInfo] ->
            {Type, Pid, to_hms(os:timestamp()), TraceInfo}
    end.

to_hms(Stamp = {_, _, Micro}) ->
    {{Y, Mo, D},{H, M, Secs}} = calendar:now_to_local_time(Stamp),
    Seconds = Secs rem 60 + (Micro / 1000000),
    {{Y,Mo,D}, {H,M,Seconds}};
to_hms(_) ->
    {{0,0,0}, {0,0,0}}.
