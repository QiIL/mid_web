%%%-------------------------------------------------------------------
%%% @doc auth router cfg
%%%-------------------------------------------------------------------
-module(auth_router).
-include("config_all.hrl").
-include("config.hrl").
%% API
-export([find/1]).

?CFG_H
?C({<<"GET">>, <<"test">>}, #r_router{controller=auth_controller, func=test})
?CFG_E.