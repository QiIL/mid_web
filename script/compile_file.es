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
-define(EBIN_LIB, (?ROOT_DIR ++ "/_build/default/lib")).
-define(SRC_WILDCARD, (?ROOT_DIR ++ "/apps/**/")).

%% 用法
usage() ->
    ?E("源码目录下编译单个文件，
    用法1（用于开发）: 编译到Dir/_build/default/lib/App/ebin
    ./compile_file Mod1 Mod2 ...ModN     - 模块名称
    
    用法2（用于生产热梗）: 编译到指定目录下的../App/ebin
    ./compile_file $compile_file_to_dir Dir Mod1 Mod2 ... ModN
    
    举例1
    ./compile_file wlib_tool
    在代码中查找 wlib_tool.erl 并编译到 /_build/default/lib/wlib
    
    举例2
    ./compile_file \\$compile_file_to_dir /tmp wlib_tool
    在代码中查找 wlib_tool.erl 并编译到 /tmp/wlib
    ~n").

main([]) ->
    usage(),
    ?EXIT(0);
main(["$compile_file_to_dir", Dir | Mods]) ->
    compile_file(Dir, Mods);
main(Mods) ->
    compile_file(?EBIN_LIB, Mods).

compile_file(OutDir, Mods) ->
    case filelib:is_dir(?ROOT_DIR++"/apps") of
        true ->
            filelib:ensure_dir(OutDir),
            ?E("开始编译文件到：~s~n", [OutDir]),
            case find_src_file(Mods) of
                {[], _} ->
                    ?E("没有需要编译的文件~~。~~~n");
                {[_|_]=NeedCompileMods, Skips} ->
                    [?E("file not find: ~s~n", [Skip]) || Skip <- Skips],
                    Opt = get_base_compile_opt(),
                    [compile(Mod, Opt, OutDir) || Mod <- NeedCompileMods],
                    ?EXIT(0);
                _Err ->
                    ?E("有点其他错误, ~p~n", [_Err]),
                    ?EXIT(1)
            end;
        _ ->
            ?E("需要在源码目录编译文件 or 未编译源码请执行make all~n"),
            ?EXIT(1)
    end.

find_src_file(Mods) ->
    find_src_file(Mods, [], []).
find_src_file([], List, List2) -> {lists:reverse(List), lists:reverse(List2)};
find_src_file([Mod|T], List, List2) ->
    io:format("find src file: ~p~n", [?SRC_WILDCARD++Mod++".erl"]),
    case filelib:wildcard(?SRC_WILDCARD++Mod++".erl") of
        [] -> find_src_file(T, List, [Mod | List2]);
        [FilePath] -> find_src_file(T, [FilePath|List], List2)
    end.

%% 开发环境下_build/default/lib/app都是软链
compile(FilePath, Opt, DeployDir) ->
    App = filename:basename(filename:dirname(filename:dirname(FilePath))),
    Now = os:system_time(second),
    OutDir = DeployDir++"/"++App++"/ebin/",
    filelib:ensure_dir(OutDir),
    IncludeOpts = [
        {i, DeployDir++"/"++App},
        {i, DeployDir++"/"++App++"/include"},
        {i, DeployDir++"/"++App++"/src"}
    ],
    NewOpt = [debug_info, no_spawn_compiler_process, {d,'COMPILE_TIME', Now}, {outdir, OutDir} | Opt] ++ IncludeOpts,
    ?E("编译文件: ~s~n", [FilePath]),
    ?E("编译选项:~p~n", [NewOpt]),
    P = "",
    case filelib:is_file(FilePath) andalso compile:file(FilePath, NewOpt) of
        {ok, _Data} ->
            ?E("~ts 编译成功:~p!~n~n", [P, _Data]);
        {ok, _, Warnings} ->
            ?E("~ts 编译成功!~n~n", [P]),
            ?E("~ts 警告:~n~p~n~n", [P, Warnings]);
        error ->
            ?E("~ts 编译失败!~n", [P]),
            ?EXIT(1);
        {error, Errors, Warnings} ->
            ?E("~ts 编译失败!~n"),
            ?E("~ts 错误:~n~p~n", [P, Errors]),
            ?E("~ts 警告:~n~p~n", [P, Warnings]),
            ?EXIT(1);
        false ->
            ?E("~ts 忽略编译!~n", [P])
    end.

get_base_compile_opt() ->
    {ok, KVS} = file:consult(?ROOT_DIR++"/rebar.config"),
    Opts = proplists:get_value(erl_opts, KVS, []),
    filter_useless_opt(Opts).
filter_useless_opt(L) ->
    filter_useless_opt(L, []).
filter_useless_opt([], L) -> L;
filter_useless_opt([{parse_transform,lager_transform}|T], L) -> 
    filter_useless_opt(T, L);
filter_useless_opt([{i, _}|T], L) ->
    filter_useless_opt(T, L);
filter_useless_opt([H|T], L) ->
    filter_useless_opt(T, [H|L]).
%%
%%get_app_version(App, FilePath) ->
%%    SrcDir = filename:dirname(FilePath),
%%    case filelib:is_file(SrcDir++"/"++App++".app.src") of
%%        true -> 
%%            AppSrc = SrcDir++"/"++App++".app.src",
%%            {ok, {_, _, Kvs}} = file:consult(AppSrc),
%%            case lists:keyfind(vsn, 1, Kvs) of
%%                false -> "";
%%                Version -> Version
%%            end;
%%        _ -> ""
%%    end. 