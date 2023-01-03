%%%-------------------------------------------------------------------
%%% @doc ./ctrl node_ctrl process
%%%-------------------------------------------------------------------
-module(wlib_ctrl).

%% API
-export([
    command/1
]).

command(CMD) ->
    io:format("CMD is string: ~p, CMD: ~p~n", [is_list(CMD), CMD]).
