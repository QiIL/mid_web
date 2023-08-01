%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @doc ./ctrl node_ctrl process
%%%-------------------------------------------------------------------
-module(wlib_ctrl).

%% API
-export([
    cmd/1
]).

cmd(CMD) ->
    io:format("CMD is a string: ~p, CMD: ~p~n", [is_list(CMD), CMD]).
