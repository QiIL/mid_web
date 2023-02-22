#!/usr/bin/env escript
%% -*- coding: utf-8 -*-
%% -*- erlang -*-
%% 修改配置文件
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
    ?E("设置erlang格式的文件命令，
    ./sed_config.es File Key1 Type1 Data1 Key2 Type2 Data2 
    支持类型: int string atom
    举例
    ./sed_config.es ./../config/env.config  ip string 127.0.0.1 port int 8080
    ~n").

main([]) ->
    usage(),
    ?EXIT(0);
main([File | Params]) ->
    try
        case filelib:is_file(File) of
            true -> ok;
            _ ->
                ?E("文件不存在: ~s~n", [File]),
                ?EXIT(1)
        end,
        {ok, FD} = file:consult(File),
        case erlang:length(Params) rem 3 of
            0 -> ok;
            _ ->
                ?E("参数数量错误，需为3的倍数: ~w~n", [Params]),
                ?EXIT(1)
        end,
        AllParams = transform_params(Params),
        NewFD = lists:foldl(
            fun({Key, Data}, Acc) ->
                lists:keystore(Key, 1, Acc, {Key, Data})
            end, FD, AllParams),
        NewConsult = [io_lib:format("~p.~n", [KVElem]) || KVElem <- NewFD],
        file:write_file(File, NewConsult),
        ok
    catch
        _:Err  ->
            ?E("key 替换报错: ~p~n", [Err]),
            ?EXIT(1)
    end.

transform_params(List) ->
    transform_params(List, []).
transform_params([], List) -> lists:reverse(List);
transform_params([Key, Type, Data | T], List) ->
    NewData = case Type of
        "int" -> erlang:list_to_integer(Data);
        "atom" -> erlang:list_to_atom(Data);
        "string" -> Data;
        _ -> ?E("参数类型错误，只支持：int atom string: ~w ~w ~w ~n", [Key, Type, Data]), ?EXIT(1)
    end,
    transform_params(T, [{erlang:list_to_atom(Key), NewData} | List]).
