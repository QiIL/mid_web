%% ===================================
%% @doc normal api
%% ===================================
-module(common_handler).
-include("mid_web.hrl").
%% API
-export([
    init/2
]).

init(Req, State) ->
    try
        handler:handle(common_router, Req, State)
    catch
        throw:{service_error, Code, ErrData} -> 
            reply:err(Req, Code, ErrData);
        E1:E2:E3 ->
            ?ERROR_MSG("Err: ~p:~p~p~n", [E1, E2, E3])
    end.
