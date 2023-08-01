%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @doc My favourite 404!!
%%%-------------------------------------------------------------------
-module(not_found_handler).
-include("mid_web.hrl").
%% API
-export([
    init/2
]).

init(Req, State) ->
    try
        reply:ok(Req, 404, <<"My favourite 404!">>),
        {ok, Req, State}
    catch
        throw:{service_error, Code, ErrData} ->
            reply:err(Req, Code, ErrData);
        E1:E2:E3 ->
            ?ERROR_MSG("Err: ~p:~p~p~n", [E1, E2, E3])
    end.