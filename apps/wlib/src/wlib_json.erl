%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @doc json 封装
%%%-------------------------------------------------------------------
-module(wlib_json).

%% API
-export([
    encode/1,
    decode/1,
    decode/2
    
]).

encode(Thing) ->
    jsx:encode(Thing).

decode(Thing) ->
    jsx:decode(Thing).

decode(Thing, Opt) ->
    jsx:decode(Thing, Opt).
