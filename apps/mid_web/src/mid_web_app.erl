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
    ?INFO_MSG("mid_web start success"),
    {ok, Sup}.

stop(_State) ->
    ok = cowboy:stop_listener(?WEB_LISTENER),
    ok.
