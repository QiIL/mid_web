%% ==========================================
%% @doc init config 
%% ==========================================
-module(config_misc).
-define(CONFIG_APP, config).
%% API
-export([
    init_config/0,
    reload_config/1,
    reload_all/0
]).

init_config() ->
    reload_all(),
    ok.

reload_config(CfgMod) ->
    {ok, [[Root]]} = init:get_argument(root),
    VSN = application:get_key(?CONFIG_APP, vsn),
    EBinFile = wlib_tool:concat([Root, "lib/config-", VSN, "/ebin/", CfgMod, ".beam"]),
    case filelib:is_file(EBinFile) of
        true -> io:format("reload ebin:~w", [CfgMod]), c:l(CfgMod);
        _ -> try_reload_config(CfgMod)
    end.

%% only reload ./config/*.config
reload_all() ->
    {ok, [[Root]]} = init:get_argument(root),
    ConfigPath = wlib_tool:concat([Root, "/config"]),
    AllConfig = filelib:wildcard("*.config", ConfigPath),
    filelib:ensure_dir(ConfigPath++"/tmp/"),
    AbsPath = [{
        wlib_tool:to_atom(filename:rootname(filename:basename(FName))),
        wlib_tool:concat([ConfigPath, "/", FName]),
            ConfigPath ++ "/tmp/" ++ filename:rootname(filename:basename(FName)) ++ ".erl"
    } || FName <- AllConfig],
    load_cfg(AbsPath).

try_reload_config(CfgMod) ->
    {ok, [[Root]]} = init:get_argument(root),
    ConfigFile = wlib_tool:concat([Root, "/config/", CfgMod, ".config"]),
    case filelib:is_file(ConfigFile) of
        true ->
            OutPutPath = wlib_tool:concat([Root, "/config/tmp/", CfgMod, ".erl"]),
            load_cfg(CfgMod, ConfigFile, OutPutPath);
        _ ->
            ignore
    end.

%% 
load_cfg(Mod, FPath, OutputPath) ->
    load_cfg([{Mod, FPath, OutputPath}]).
load_cfg([]) -> ok;
load_cfg([{Mod, FPath, OutPutPath} | T]) ->
    Src = gen_source_code(Mod, FPath),
    file:write_file(OutPutPath, wlib_tool:to_binary(Src)),
    case compile:file(OutPutPath, [binary]) of
        {ok, Mod, Bin}  ->
            erlang:load_module(Mod, Bin);
        _Err ->
            io:format("err: ~p~n", [_Err])
    end,
    load_cfg(T).

gen_source_code(Mod, FPath) ->
    {ok, Keys} = file:consult(FPath),
    FindFund = gen_pattern_match_func(Keys,
        "find",
        fun({Key, _}) -> io_lib:format("~w", [Key]) end,
        fun({_, Val}) -> io_lib:format("~w", [Val]) end),
    wlib_tool:concat([
        "%% -*- coding: utf-8 -*-\n"
        "-module(", Mod, ").\n",
        "-export([find/1]).\n\n",
        FindFund
    ]).

%% =========================================================
%% 生成模式匹配的函数
gen_pattern_match_func(List, FuncName, ParamFunc, DataFunc) ->
    gen_pattern_match_func(List, FuncName, 1, undefined, fun(_Data) -> true end, ParamFunc, DataFunc).
%%gen_pattern_match_func(List, FuncName, FilterFunc, ParamFunc, DataFunc) ->
%%    gen_pattern_match_func(List, FuncName, 1, undefined, FilterFunc, ParamFunc, DataFunc).
gen_pattern_match_func(List, FuncName, ParamsNum, OtherMatch, Filter, ParamFunc, DataFunc) ->
    gen_pattern_match_func2(lists:reverse(List), FuncName, Filter, ParamFunc, DataFunc,
        [wlib_tool:concat([FuncName, "(", string:join(["_" || _ <-lists:seq(1, ParamsNum)], ", "),") -> ", OtherMatch, ".\n"])]).
gen_pattern_match_func2([], _, _, _, _, List) -> wlib_tool:concat(List);
gen_pattern_match_func2([Data|T], FuncName, FilterFunc, ParamFunc, DataFunc, List) ->
    case FilterFunc(Data) of
        true ->
            FuncStr = wlib_tool:concat([FuncName, "(", ParamFunc(Data), ") -> ", DataFunc(Data), ";\n"]),
            gen_pattern_match_func2(T, FuncName, FilterFunc, ParamFunc, DataFunc, [FuncStr|List]);
        _ ->
            gen_pattern_match_func2(T, FuncName, FilterFunc, ParamFunc, DataFunc, List)
    end.