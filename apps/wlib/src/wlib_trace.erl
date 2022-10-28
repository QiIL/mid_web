%%% ---------------------------
%% @doc 运行时监控工具
%%% ---------------------------
-module(wlib_trace).

-include_lib("stdlib/include/ms_transform.hrl").

-export([
    i/0,
    i/1,
    %% 例子：watch_return( world_cache_server, do_loop_sec )
    watch_return/2,
    watch_return/3,
    watch_return/4,
    watch_return/5,

    %% 例子 watch_filter( world_cache_server, do_loop_sec, dbg:fun2ms(fun([A]) when (A rem 2) == 0 -> true end) ).
    %%  watch_filter(ylib_tool, random, {['$1'], [{'==','$1', 100}]} ).
    watch_filter/3,
    watch_filter/4,
    watch_filter/5,

    %% 例子 watch_return_filter(ylib_tool, random, fun(A) -> lists:member(A, [1,3,5])  end). %% 则当 ylib_tool:random/N 返回 1,3,5的时候才有输出
    watch_return_filter/3,
    watch_return_filter/4,
    watch_return_filter/5,

    %% 例子：watch_node([], ynode_rank_manager_server, maybe_dump_data).
    watch_node/3,
    watch_node/4,

    watch/1,
    watch/2,
    watch/3,
    watch/4,
    watch/5,

    unwatch_all/0,
    unwatch_all/1,
    unwatch/1,
    unwatch/2,
    trace_opt/1,
    format_trace/1,
    format_trace/2,
    match_spec/0,
    match_spec/1
]).

-export([
    tracer_loop/6
]).
-define(DEBUG_PRINT(__Format, __Arg), ok).
i() ->
    MPrint = erlang:whereis(mprint),
    MTrace = whereis(mtrace),
    Info =
        case erlang:is_pid(MTrace) of
            true ->
                Self = erlang:self(),
                erlang:send(MTrace, {'$info', Self}),
                receive
                    Msg -> Msg
                after 5000 ->
                    timeout
                end;
            _ ->
                undefined
        end,
    #{mprint=>MPrint, mtrace=>MTrace, opts =>Info }.
i(Node) ->
    rpc:call(Node, ?MODULE, i, []).
unwatch_all() ->
    unwatch_all(false).
unwatch_all(IsForce) ->
    case IsForce=:=false andalso (catch erlang:process_info(whereis(mtrace), links)) of
        {links, Links} ->
            Self = erlang:self(),
            case lists:member(Self, Links) of
                true ->
                    stop_trace("unwatch");
                _ ->
                    {error, Links}
            end;
        _ ->
            stop_trace("unwatch")
    end.
unwatch(Mod) ->
    clean_pattern(Mod).
unwatch(Mod, Func) ->
    clean_pattern(Mod, Func).

%% @doc 监视函数调用以及其返回值
-spec watch_return(term(), term()) -> {ok, pid()} | error.
watch_return(M, F) when is_atom(M) ->
    watch(all, M, F, '_', match_spec() ).
%% @doc 监视函数调用以及其返回值
-spec watch_return(term(), term(), term()) -> {ok, pid()} | error.
watch_return(PID, M, F) when is_pid(PID) orelse PID=:=all orelse PID=:=new orelse PID=:=existing ->
    watch(PID, M, F, '_', match_spec() );
watch_return(M, F, A) when is_atom(M) ->
    watch(all, M, F, A, match_spec() ).
%% @doc 监视函数调用以及其返回值
-spec watch_return(term(), term(), term(), term()) -> {ok, pid()} | error.
watch_return(PID, M, F, A) ->
    watch(PID, M, F, A, match_spec() ).
%% @doc 监视函数调用以及其返回值
-spec watch_return(term(), term(), term(), term(), term()) -> {ok, pid()} | error.
watch_return(PID, M, F, A, MatchSpec) ->
    watch(PID, M, F, A, MatchSpec).

%% @doc 监视函数调用以及其返回值，过滤掉多余的信息
-spec watch_filter(term(), term(), term()) -> {ok, pid()} | error.
watch_filter(M, F, FilterFunc) when erlang:is_function(FilterFunc) ->
    watch_filter(all, M, F, '_', undefined, FilterFunc);
watch_filter(M, F, PrintMatchSpec) ->
    watch_filter(all, M, F, '_', PrintMatchSpec, undefined).
%% @doc 监视函数调用以及其返回值，过滤掉多余的信息
-spec watch_filter(term(), term(), term(), term()) -> {ok, pid()} | error.
watch_filter(M, F, PrintMatchSpec, FilterFunc) ->
    watch_filter(all, M, F, '_', PrintMatchSpec, FilterFunc).
%% @doc 监视函数调用以及其返回值，过滤掉多余的信息
-spec watch_filter(term(), term(), term(), term(), term()) -> {ok, pid()} | error.
watch_filter(PID, M, F, PrintMatchSpec, FilterFunc) ->
    watch_filter(PID, M, F, '_', PrintMatchSpec, FilterFunc).
watch_filter(PID, M, F, A, PrintMatchSpec, FilterFunc) ->
    MatchSpec =
        case PrintMatchSpec of
            {MatchHead, MatchCondition} -> [{MatchHead, MatchCondition, [{message,{caller}},{return_trace},{exception_trace}]}];
            [{MatchHead, MatchCondition, [ok]}] -> [{MatchHead, MatchCondition, [{message,{caller}},{return_trace},{exception_trace}]}]; %% 为了方便输入，把ok和true替换为常规输出内容
            [{MatchHead, MatchCondition, [true]}] -> [{MatchHead, MatchCondition, [{message,{caller}},{return_trace},{exception_trace}]}];
            [{_MatchHead, _MatchCondition, _MatchReturn}] -> PrintMatchSpec;
            _ -> match_spec()
        end,
    watch(PID, M, F, A, MatchSpec, FilterFunc).

%% @doc 监视函数调用以及其返回值，过滤掉多余的信息
-spec watch_return_filter(term(), term(), term()) -> {ok, pid()} | error.
watch_return_filter(M, F, ReturnFilter) when erlang:is_function(ReturnFilter) ->
    watch_return_filter(all, M, F, '_', undefined, ReturnFilter).
%% @doc 监视函数调用以及其返回值，过滤掉多余的信息
-spec watch_return_filter(term(), term(), term(), term()) -> {ok, pid()} | error.
watch_return_filter(M, F, PrintMatchSpec, ReturnFilter) ->
    watch_return_filter(all, M, F, '_', PrintMatchSpec, ReturnFilter).
%% @doc 监视函数调用以及其返回值，过滤掉多余的信息
-spec watch_return_filter(term(), term(), term(), term(), term()) -> {ok, pid()} | error.
watch_return_filter(PID, M, F, PrintMatchSpec, ReturnFilter) ->
    watch_return_filter(PID, M, F, '_', PrintMatchSpec, ReturnFilter).
watch_return_filter(PID, M, F, A, PrintMatchSpec, ReturnFilter) ->
    Self = erlang:self(),
    {_, GLPid} = process_info(Self, group_leader),
    MatchSpec =
        case PrintMatchSpec of
            {MatchHead, MatchCondition} -> [{MatchHead, MatchCondition, [{message,{caller}},{return_trace},{exception_trace}]}];
            [{MatchHead, MatchCondition, [ok]}] -> [{MatchHead, MatchCondition, [{message,{caller}},{return_trace},{exception_trace}]}]; %% 为了方便输入，把ok和true替换为常规输出内容
            [{MatchHead, MatchCondition, [true]}] -> [{MatchHead, MatchCondition, [{message,{caller}},{return_trace},{exception_trace}]}];
            [{_MatchHead, _MatchCondition, _MatchReturn}] -> PrintMatchSpec;
            _ -> match_spec()
        end,
    HandleFunc =
        fun(TraceInfo) ->
            case TraceInfo of
                {trace,_From,return_from, _, ReturnValue} ->
                    {Time, LastCall} = pop_call(),
                    IsPrint = (catch ReturnFilter(ReturnValue)) =:= true;
                {trace,_From,exception_from, _, ReturnValue} ->
                    {Time, LastCall} = pop_call(),
                    IsPrint = (catch ReturnFilter(ReturnValue)) =:= true;
                {trace,_From,call,_,_Ext} ->
                    Time = {erlang:localtime(), erlang:system_time(millisecond) rem 1000},
                    LastCall = TraceInfo,
                    push_call({Time, TraceInfo}),
                    IsPrint = false;
                {trace,_From,call,_} ->
                    Time = {erlang:localtime(), erlang:system_time(millisecond) rem 1000},
                    LastCall = TraceInfo,
                    push_call({Time, TraceInfo}),
                    IsPrint = false;
                _ ->
                    Time = {erlang:localtime(), erlang:system_time(millisecond) rem 1000},
                    LastCall = TraceInfo,
                    io:format(GLPid, "~s~n", [?MODULE:format_trace(TraceInfo)]),
                    IsPrint = false
            end,
            case IsPrint of
                true ->
                    io:format(GLPid, "~s~n", [?MODULE:format_trace(Time, LastCall)]),
                    io:format(GLPid, "~s~n", [?MODULE:format_trace(TraceInfo)]);
                _ ->
                    ignore
            end
        end,
    Opts = #{mod=>M, func=>F, arity=>A, match=>MatchSpec, pid=>PID,handle_func=>HandleFunc},
    trace_opt(Opts).
pop_call() ->
    case erlang:get({?MODULE,pop_call}) of
        [T|L] -> erlang:put({?MODULE,pop_call}, L), T;
        _ -> undefined
    end.
push_call(TraceInfo) ->
    case erlang:get({?MODULE,pop_call}) of
        [_|_]=L -> erlang:put({?MODULE,pop_call}, [TraceInfo|L]);
        _ -> erlang:put({?MODULE,pop_call}, [TraceInfo])
    end.

%% @doc 监视其它节点上的函数调用以及其返回值
-spec watch_node(term(), term(), term()) -> [{ok, pid()}] | [error].
watch_node(Node, M, F) ->
    watch_node(Node, M, F, '_').
%% @doc 监视其它节点上的函数调用以及其返回值
-spec watch_node(term(), term(), term(), term()) -> [{ok, pid()}] | [error].
watch_node(Node, M, F, A) ->
    Nodes =
        case erlang:is_atom(Node) of
            true -> [Node];
            _ -> Node
        end,
    PrintPid =
        case erlang:whereis(mprint) of
            PPid when erlang:is_pid(PPid) ->
                PPid;
            _ ->
                PrintFun = fun(G)-> receive {SendNode, Msg} -> io:format("~ts ~ts~n", [SendNode, Msg]) end,  G(G) end,
                erlang:spawn_link(fun()-> erlang:register(mprint, erlang:self()), PrintFun(PrintFun) end)
        end,
    Opts = #{mod=>M, func=>F, arity=>A, link_pid=>PrintPid, handle_func=> fun(MM) -> erlang:send(PrintPid, {node(), ?MODULE:format_trace(MM)}) end },
    [ rpc:call(TraceNode, ?MODULE, trace_opt, [Opts]) || TraceNode <- Nodes].

%% @doc 监视函数调用
-spec watch(atom() | pid()) -> {ok, pid()} | error.
watch(M) when is_atom(M) ->
    watch(all, M, '_', '_');
watch(PID) when is_pid(PID) orelse PID=:=all orelse PID=:=new orelse PID=:=existing ->
    watch(PID, '_', '_', '_').
%% @doc 监视函数调用
-spec watch(atom() | pid(), function() | atom()) -> {ok, pid()} | error.
watch(M, F) when is_atom(M) ->
    watch(all, M, F, '_');
watch(PID, M) when is_pid(PID) orelse PID=:=all orelse PID=:=new orelse PID=:=existing ->
    watch(PID, M, '_', '_').
%% @doc 监视函数调用
-spec watch(term(), term(), term()) -> {ok, pid()} | error.
watch(M, F, A) when is_atom(M) ->
    watch(all, M, F, A);
watch(PID, M, F) when is_pid(PID) orelse PID=:=all orelse PID=:=new orelse PID=:=existing ->
    watch(PID, M, F, '_').
%% @doc 监视函数调用
-spec watch(term(), term(), term(), term()) -> {ok, pid()} | error.
watch(PID, M, F, A) when is_number(A) orelse A=:='_' ->
    watch(PID, M, F, A, match_spec(caller) );
watch(PID, M, F, MatchSpec) ->
    watch(PID, M, F, '_', MatchSpec).
%% @doc 监视函数调用
-spec watch(term(), term(), term(), term(), term()) -> {ok, pid()} | error.
watch(PID, M, F, Arity, MatchSpec) -> %%
    watch(PID, M, F, Arity, MatchSpec, undefined).
watch(PID, M, F, Arity, MatchSpec, FilterFunc) -> %%
    Opts = #{mod=>M, func=>F, arity=>Arity, match=>MatchSpec, pid=>PID,filter_func=>FilterFunc},
    trace_opt(Opts).


%% @doc
%% 监控基础函数
%% trace_opt(Opts) -> {ok, Pid} | {error, Reason}.
%% Opts =
%% #{
%%     pid => all, 监控的进程Pid
%%     mod => '_', 监控的模块
%%     func => '_', 监控的函数
%%     arity => '_', 监控的函数参数
%%     match => match_spec(), 匹配规则
%%     auto_load = true, 是否自动加载监控的模块
%%     handle_func => fun(Msg) -> io:format(GLPid, "~s~n", [format_trace(Msg)]) end, 处理监控消息，默认是直接输出
%%     filter_func = undefined, 过滤输出函数
%%     max_frequency = 10000, 每秒最多多少个协议，超过自动停止
%%     max_lines = 200000,  累计最多收到多少个协议则停止，包含过滤的
%%     link_pid = erlang:self(), 链接进程，该进程挂了则退出trace
%%     quite = false, 不输出额外信息
%%     debug = false 是否输出调试信息(调试ytrace)
%%  },
%% @end
-spec trace_opt(Opts :: map() ) -> {ok, TracerPid :: pid()} | {error, {TPid :: pid(), tracer_already_exists}}.
trace_opt(Opts) ->
    ?DEBUG_PRINT("==>~w", [Opts]),
    case catch setup(Opts) of
        {ok,_} = R ->
            Quite = maps:get(quite, Opts, false),
            IsDebug = maps:get(debug, Opts, false),
            AutoLoad = maps:get(auto_load, Opts, true),
            Mod = maps:get(mod, Opts, '_'),
            Func = maps:get(func, Opts, '_'),
            Arity = maps:get(arity, Opts, '_'),
            MatchSpec = maps:get(match, Opts, match_spec() ),
            case AutoLoad of true -> catch code:ensure_loaded(Mod); _ -> ok end,
            case Quite=:=false andalso IsDebug of true -> catch io:format("erlang:trace_pattern(~w,~w,~w)~n", [{Mod,Func,Arity}, MatchSpec, [local]]); _ -> ok end,
            ?DEBUG_PRINT("=====>~w", [{{Mod,Func,Arity}, MatchSpec}]),
            erlang:trace_pattern({Mod,Func,Arity}, MatchSpec, [local]),
            R;
        _Err ->
            _Err
    end.


setup(Opts) ->
    case whereis(mtrace) of
        TPid when is_pid(TPid) ->
            {_, TGLPid} = process_info(TPid, group_leader),
            {_, GLPid} = process_info(erlang:self(), group_leader),
            case TGLPid =:= GLPid of
                true ->
                    {ok, TPid};
                _ ->
                    {error, {TPid, tracer_already_exists}}
            end;
        _->
            {ok, TPid} = start_tracer(Opts),
            {ok, TPid}
    end.

match_spec() ->
    match_spec(caller_return).
match_spec(caller_return) ->
    dbg:fun2ms(fun(_) -> message(caller()), return_trace(), exception_trace() end);
match_spec(caller) ->
    dbg:fun2ms(fun(_) -> message(caller()) end);
match_spec(return) ->
    dbg:fun2ms(fun(_) -> message(false), return_trace(), exception_trace() end);
match_spec(_Default) ->
    dbg:fun2ms(fun(_) -> message(caller()), return_trace(), exception_trace() end).

format_trace(Msg) ->
    format_trace({erlang:localtime(), erlang:system_time(millisecond) rem 1000}, Msg).
format_trace({{{Y,Mo,D},{H,Mi,S}}, MS}, {trace,From,call,{Mod,Func,Args}}) ->
    io_lib:format("===Call=======[~p_~p_~p ~2..0w:~2..0w:~2..0w ~3..0w]==[~99999p]==[~99999p:~99999p/~99999p] Args=~99999p",[Y,Mo,D,H,Mi,S,MS,From,Mod,Func,length(Args),Args]);
format_trace({{{Y,Mo,D},{H,Mi,S}}, MS}, {trace,From,call,{Mod,Func,Args},Ext}) ->
    io_lib:format("===Call=======[~p_~p_~p ~2..0w:~2..0w:~2..0w ~3..0w]==[~99999p]==[~99999p:~99999p/~99999p] Args=~99999p  @~99999p",[Y,Mo,D,H,Mi,S,MS,From,Mod,Func,length(Args),Args,Ext]);
format_trace({{{Y,Mo,D},{H,Mi,S}}, MS}, {trace,From,return_from,{Mod,Func,Arity},ReturnValue}) ->
    io_lib:format("===Return=====[~p_~p_~p ~2..0w:~2..0w:~2..0w ~3..0w]==[~99999p]==[~99999p:~99999p/~99999p] Value=~99999p",[Y,Mo,D,H,Mi,S,MS,From,Mod,Func,Arity,ReturnValue]);
format_trace({{{Y,Mo,D},{H,Mi,S}}, MS}, {trace,From,exception_from,{Mod,Func,Arity},Exception}) ->
    io_lib:format("===Exception!=[~p_~p_~p ~2..0w:~2..0w:~2..0w ~3..0w]==[~99999p]==[~99999p:~99999p/~99999p] Value=~99999p",[Y,Mo,D,H,Mi,S,MS,From,Mod,Func,Arity,Exception]);
format_trace({{{Y,Mo,D},{H,Mi,S}}, MS}, M) ->
    io_lib:format("===unknown!=[~p_~p_~p ~2..0w:~2..0w:~2..0w ~3..0w]==~99999p",[Y,Mo,D,H,Mi,S,MS,M]).

get_filter_msg({trace,_From,call,{_Mod,_Func,Args}}) ->
    {call, Args};
get_filter_msg({trace,_From,call,{_Mod,_Func,Args},_Ext}) ->
    {call, Args};
get_filter_msg({trace,_From,return_from,{_Mod,_Func,_Arity},ReturnValue}) ->
    {return, ReturnValue};
get_filter_msg({trace,_From,exception_from,{_Mod,_Func,_Arity},Exception}) ->
    {exception, Exception};
get_filter_msg(M) ->
    {other, M}.

%% @doc tracer for remsh with auto-stop
%% MaxFreq  := max frequency auto stop, line-per-second
%%         MaxLines := max lines auto stop
start_tracer(Opts) ->
    Self = erlang:self(),
    {_, GLPid} = process_info(Self, group_leader),
    Quite = maps:get(quite, Opts, false),
    IsDebug = maps:get(debug, Opts, false), %%
    PidSpec = maps:get(pid, Opts, all), %% 要trace的进程
    LinkPid = maps:get(link_pid, Opts, Self), %% 外部进程链接，外部进程挂了，该trace停止
    MaxFreq = maps:get(max_frequency, Opts, 10000), %% 每秒最多多少个协议，超过自动停止
    MaxLines = maps:get(max_lines, Opts, 200000), %% 累计最多收到多少个协议则停止，包含过滤的
    HandleFunc = maps:get(handle_func, Opts, fun(Msg) -> io:format(GLPid, "~s~n", [format_trace(Msg)]) end), %% 默认处理函数
    FilterFunc = maps:get(filter_func, Opts, undefined), %%
    case Quite=:=false andalso IsDebug of true -> catch io:format("link:~w max_freq:~w max_lines:~w~n", [LinkPid, MaxFreq, MaxLines]); _ -> ok end,
    ?DEBUG_PRINT("~w", [{GLPid, LinkPid, MaxFreq, MaxLines, HandleFunc, FilterFunc}]),
    TPid = erlang:spawn_opt(fun() -> tracer(Opts, LinkPid, MaxFreq, MaxLines, HandleFunc, FilterFunc) end, [{priority,max}]),
    ?DEBUG_PRINT("~w", [{PidSpec, TPid}]),
    case catch erlang:trace(PidSpec, true, [call,{tracer,TPid}]) of
        Int when is_integer(Int) ->
            ok;
        _Err ->
            case Quite=:=false of true -> catch io:format("ERROR: mtrace failed to start:~w.~n", [_Err]); _ -> ok end,
            kill(mtrace)
    end,
    ?DEBUG_PRINT("~w", [{TPid}]),
    {ok,TPid}.
tracer(Opts, LinkPid, MaxFreq, MaxLines, HandleFunc, FilterFunc) ->
    true = erlang:register(mtrace, self()),
    erlang:process_flag(trap_exit, true),
    link(LinkPid),
    Ret = (catch ?MODULE:tracer_loop(Opts, LinkPid, HandleFunc, FilterFunc, MaxFreq, MaxLines)),
    Ret.

tracer_loop(Opts, LinkPid, HandleFunc, FilterFunc, MaxFreq, MaxLines) ->
    receive
        {'EXIT', LinkPid, _}=_Err ->
            ?DEBUG_PRINT("~w", [_Err]),
            stop_trace("link pid exit");
        {'$info', P} ->
            erlang:send(P, Opts);
        Msg ->
            trace_handle(Msg, HandleFunc, FilterFunc, MaxFreq, MaxLines)
    end,
    ?MODULE:tracer_loop(Opts, LinkPid, HandleFunc, FilterFunc, MaxFreq, MaxLines).
trace_handle(Msg, HandleFunc, FilterFunc, MaxFreq, MaxLines) ->
    check_freq_and_lines(erlang:system_time(second), MaxFreq, MaxLines),
    case FilterFunc=:=undefined orelse FilterFunc(get_filter_msg(Msg)) of
        true ->
            HandleFunc(Msg);
        _ ->
            ok
    end.

stop_trace(Reason) ->
    catch io:format("terminated:~ts~n", [Reason]), %% 必须要加catch
    case erlang:whereis(mprint) of
        PPid when erlang:is_pid(PPid) ->
            erlang:exit(PPid, kill);
        _ ->
            ignore
    end,
    case whereis(mtrace) of
        TPid when is_pid(TPid) ->
            clean_pattern(),
            kill(mtrace);
        _ ->
            ignore
    end.
clean_pattern() ->
    erlang:trace(all, false, [all]),
    erlang:trace_pattern({'_','_','_'}, false, [local,meta,call_count,call_time]),
    erlang:trace_pattern({'_','_','_'}, false, []),
    ok.
clean_pattern(Mod) ->
    erlang:trace_pattern({Mod,'_','_'}, false, [local,meta,call_count,call_time]),
    erlang:trace_pattern({Mod,'_','_'}, false, []),
    ok.
clean_pattern(Mod, Func) ->
    erlang:trace_pattern({Mod,Func,'_'}, false, [local,meta,call_count,call_time]),
    erlang:trace_pattern({Mod,Func,'_'}, false, []),
    ok.

kill(Name) ->
    This = self(),
    case whereis(Name) of
        undefined ->
            ok;
        This ->
            throw(exit);
        Pid ->
            unlink(Pid),
            exit(Pid, kill),
            wait_for_death(Pid)
    end.

wait_for_death(Pid) ->
    case is_process_alive(Pid) of
        true ->
            timer:sleep(10),
            wait_for_death(Pid);
        false ->
            ok
    end.

check_freq_and_lines(Now, MaxFreq, MaxLines) ->
    case erlang:get(last_line) of
        {Now, C1} when C1>MaxFreq -> stop_trace("max frequency");
        {Now, C1} -> ok;
        _ ->  C1 = 0
    end,
    erlang:put(last_line, {Now,C1+1}),
    case erlang:get(line_count) of
        undefined -> C2 = 0;
        C2 when C2>MaxLines -> stop_trace("max lines");
        C2 -> ok
    end,
    erlang:put(line_count, C2+1),
    ok.