#!/usr/bin/env escript
%% -*- coding: utf-8 -*-
%% -*- erlang -*-
%% 编译单个模块
-mode(compile).
-define(EXIT(Code), erlang:halt(Code)).
-define(NONE, none).

-define(E(FormatStr, Args), io:format(unicode:characters_to_binary(FormatStr), Args)).
-define(E(FormatStr), io:format(unicode:characters_to_binary(FormatStr))).

-define(ROOT_DIR, (filename:dirname(filename:dirname(escript:script_name())))).


usage() -> 
    ?E("加载模块到运行中的服务：~p~n", [?MODULE]).

main([]) ->
    usage(),
    ?EXIT(0);
main(Mods) ->
    