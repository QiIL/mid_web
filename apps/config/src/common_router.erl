%%%-------------------------------------------------------------------
%%% @doc ALL Router
%%%-------------------------------------------------------------------
-module(common_router).
-include("config_all.hrl").
-include("config.hrl").
%% API
-export([find/1]).

?CFG_H
?C({<<"GET">>, <<"test">>}, #r_router{controller=common_controller, func=test})
?CFG_E.