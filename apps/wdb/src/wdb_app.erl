%% -*- coding: utf-8 -*-
-module(wdb_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    wdb_sup:start_link().

stop(_State) ->
    ok.
