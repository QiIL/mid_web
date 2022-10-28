%%%-------------------------------------------------------------------
%%% @doc need auth api
%%%-------------------------------------------------------------------
-module(auth_handler).
-include("mid_web.hrl").
%% API
-export([
    init/2
]).

init(Req, State) ->
    try
        do_auth(Req),
        handler:handle(auth_router, Req, State)
    catch
        throw:{service_error, Code, ErrData} ->
            reply:err(Req, Code, ErrData);
        E1:E2:E3 ->
            ?ERROR_MSG("Err: ~p:~p~p~n", [E1, E2, E3])
    end.

do_auth(Req) ->
    case cowboy_req:header(<<"token">>, Req) of
        undefined -> ?THROW_FORBIDDEN(<<"not token">>);
        _ -> ok
    end.
