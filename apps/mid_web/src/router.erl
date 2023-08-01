%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @doc 存放路由
%%%-------------------------------------------------------------------
-module(router).
-include("mid_web.hrl").

%% API
-export([
    get_router/0,
    update_router/0
]).

%% Host = {HostMatch, PathsList} | {HostMatch, Constraints, PathsList}.
%% Path = {PathMatch, Handler, InitialState} | {PathMatch, Constraints, Handler, InitialState}.
%% For example:
%% Host1 = "www.baidu.com"
%% Path1 = "/"
%% BindNamePath = "/rest/:name"

get_router() ->
    [
%%        {"/assets/[...", cowboy_static, {priv_dir, mid_web, "static/assets/index.html"}},
        {'_', [ 
            {"/api/auth/:action", auth_handler, []},
            {"/api/comm/:action", common_handler, []},
            {'_', not_found_handler, []}
        ]}
    ].

update_router() ->
    Router = cowboy_router:compile(get_router()),
    cowboy:set_env(?WEB_LISTENER, dispatch, Router).