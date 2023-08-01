%% -*- coding: utf-8 -*-
-module(mid_web_app).
-behavior(application).
-include("mid_web.hrl").
%% API
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    wlog:init(),
    config_misc:init_config(),
    Routers = router:get_router(),
    [Port] = wconf:find(env, port),
    Dispatch = cowboy_router:compile(Routers),
    cowboy:start_clear(?WEB_LISTENER,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    {ok, Sup} = mid_web_sup:start_link(),
    [IsDebug] = wconf:find(env, is_debug),
    case IsDebug of
        true -> 
            sync:go(),
            sync:onsync(fun(Mods) ->
                ?DEBUG_MSG("重新加载了以下模块 : ~p~n",[Mods])
            end); 
        _ -> next
    end,
    ?INFO_MSG("mid_web start success"),
    {ok, Sup}.

stop(_State) ->
    sync:stop(),
    ok = cowboy:stop_listener(?WEB_LISTENER),
    ok.
