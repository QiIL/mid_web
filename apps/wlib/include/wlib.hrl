-ifndef(WLIB_HRL).
-define(WLIB_HRL, true).
-include_lib("wlog/include/wlog.hrl").

%% ================================== 消息处理 ===============================
-define(TIME_LIMIT_FLAG, '$time_limit').
-define(DO_HANDLE_COMMON(Info),
    case Info of
        {func, __F, __A} -> erlang:apply(__F, __A);
        {func, __M, __F, __A} -> erlang:apply(__M, __F, __A);
        {func, __Function} when erlang:is_function(__Function) -> __Function();
        _ -> do_handle(Info)
    end).
-define(DO_HANDLE_INFO(Info, State),
    try
        ?DO_HANDLE_COMMON(Info)
    catch __CLASS:__REASON:__STACKTRACE ->
        ?ERROR_MSG("info:~w~n State=~w~n Reason:~w~n Stacktrace:~p", [Info,State, __REASON, __STACKTRACE]) ,
        error
    end).

-define(DO_HANDLE_CAST(Info, State),
    try
        ?DO_HANDLE_COMMON(Info)
    catch __CLASS:__REASON:__STACKTRACE ->
        ?ERROR_MSG("cast:~w~n State=~w~n Reason: ~w~n Stacktrace:~p", [Info,State, __REASON, __STACKTRACE]),
        error
    end).

-define(DO_HANDLE_CALL(Request, State),
    try
        case Request of
            {?TIME_LIMIT_FLAG, Time, Request2} ->
                case wtime:now_os()*1000 > Time of
                    true -> {error, call_timeout};
                    false -> ?DO_HANDLE_COMMON(Request2)
                end;
            _ -> ?DO_HANDLE_COMMON(Request)
        end
    catch __CLASS:__REASON:__STACKTRACE ->
        ?ERROR_MSG("call:~w~n State=~w~n Reason:~w~n Stacktrace:~p", [Request,State, __REASON, __STACKTRACE]),
        error
    end).

-define(CALL_TIMETOUT, 7000). %% 7秒超时，实际上可能是8秒

%% =============================错误处理===============================
-ifdef(OTP_RELEASE). %% this implies 21 or higher
-define(EXCEPTION(__CLASS, __REASON, __STACKTRACE), __CLASS:__REASON:__STACKTRACE).
-define(GET_STACK(__STACKTRACE), __STACKTRACE).
-else.
-define(EXCEPTION(__CLASS, __REASON, _), __CLASS:__REASON).
-define(GET_STACK(_), erlang:get_stacktrace()).
-endif.

-define(TRY_CATCH(__EXPRESSION, Tip, __REASON,__STACKTRACE),
    try
        __EXPRESSION
    catch
        ?EXCEPTION(_,__REASON,__STACKTRACE) ->
            ?ERROR_MSG("~ts: Reason=~w~n,Stacktrace=~p", [Tip, __REASON, ?GET_STACK(__STACKTRACE)]),
            error
    end).
-define(TRY_CATCH(__EXPRESSION, __REASON, __STACKTRACE),
    try
        __EXPRESSION
    catch
        ?EXCEPTION(_, __REASON, __STACKTRACE) ->
            ?ERROR_MSG("Reason=~w~n,Stacktrace=~p", [__REASON, ?GET_STACK(__STACKTRACE)]),
            error
    end).
-define(TRY_CATCH(__EXPRESSION,__REASON), ?TRY_CATCH(__EXPRESSION, __REASON, __STACKTRACE)).
-define(TRY_CATCH(__EXPRESSION), ?TRY_CATCH(__EXPRESSION, __REASON)).


-define(CATCH(Fun,Pass),
    case catch Fun of
        Pass->Pass;
        Error->error
    end).

-define(CATCH(Fun,Pass,Error),
    case catch Fun of
        Pass->Pass;
        Error->error
    end).


-endif.