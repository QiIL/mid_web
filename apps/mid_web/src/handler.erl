%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @doc normal callback
%%%-------------------------------------------------------------------
-module(handler).
-include_lib("config/include/config_all.hrl").
-include("mid_web.hrl").

%% API
-export([handle/3]).

handle(RouterCfg, Req, State) ->
    case handle(RouterCfg, Req) of
        no_reply -> ok;
        ok -> reply:ok(Req);
        {ok, Data} -> reply:ok(Req, Data);
        {ok, Code, Data} -> reply:ok(Req, Code, Data);
        {ok, Code, Header, Data} -> reply:ok(Req, Code, Header, Data);
        {error, Data} -> reply:err(Req, Data);
        {error, Code, Data} -> reply:err(Req, Code, Data);
        {error, Code, Header, Data} -> reply:err(Req, Code, Header, Data)
    end,
    {ok, Req, State}.

handle(RouterCfg, Req) ->
    ?INFO_MSG("action: ~p~n", [cowboy_req:binding(action, Req)]),
    ?INFO_MSG("method: ~p~n", [cowboy_req:method(Req)]),
    case cowboy_req:binding(action, Req) of
        undefined -> ?THROW_NOT_FOUND();
        Action ->
            case wconf:find(RouterCfg, {cowboy_req:method(Req), Action}) of
                [#r_router{controller=Controller, func=Func}] ->
                    case erlang:function_exported(Controller, Func, 1) of
                        true ->
                            erlang:apply(Controller, Func, [Req]);
                        _ -> ?THROW_NOT_FOUND()
                    end;
                _ -> ?THROW_NOT_FOUND()
            end
    end.