#!/usr/bin/env escript
%% -*- coding: utf-8 -*-
%% -*- erlang -*-
%% 加载个某些模块到本物理机运行中的节点
-mode(compile).
-define(EXIT(Code), erlang:halt(Code)).
-define(NONE, none).

-define(E(FormatStr, Args), io:format(unicode:characters_to_binary(FormatStr), Args)).
-define(E(FormatStr), io:format(unicode:characters_to_binary(FormatStr))).
-define(P(F, D), ?E("[L~p]" ++ (F) ++ "\n", [?LINE] ++ (D))).

-define(ROOT_DIR, (filename:dirname(filename:dirname(escript:script_name())))).


usage() -> 
    ?E("加载模块到运行中的服务服务：~p~n", [?MODULE]),
    ?E("请使用 ./ctrl l mod1 mod2~n").

main([]) ->
    usage(),
    ?EXIT(0);
main(Mods) ->
    case get_running_node() of
        {_, []} ->
            ?E("本机服务器未启动,没有运行的节点!~n"),
            ?EXIT(1);
        {ProjectName, [_|_] = Nodes} ->
            connect_nodes(Nodes, ProjectName),
            reload_module(Mods)
    end,
    ok.

get_running_node() ->
    %%sys.env配置了PROJECT_NAME, 不然得自己设置  
    case os:getenv("PROJECT_NAME") of
        false -> ?E("项目名的环境变量缺失，检查sys.env的PROJECT_NAME"), ?EXIT(1);
        ProjectNameStr ->
            GetPidCMD = "ps -axww | grep " ++ ProjectNameStr ++ " | awk '{for (i=0; i<NF; ++i) {if($i==\"-name\"){print $(i+1)}}}'",
%%            ?E("查找命令为：~p~n", [GetPidCMD]),
            Ret = os:cmd(GetPidCMD),
%%            ?E("命令结果为：~p~n", [Ret]),
            {ProjectNameStr, string:tokens(Ret, "\n")}
    end.

connect_nodes(Nodes, ProjectName) ->
    [begin
        CookieExtend = os:getenv("COOKIE_EXTEND"),
        Cookie = erlang:list_to_atom(NodeName ++ "_" ++ CookieExtend),
        Node = list_to_atom(NodeName),
%%        ?E("Node: ~p, cookie: ~p, my: ~p", [Node, Cookie, erlang:node()]),
        erlang:set_cookie(Node, Cookie),
        case net_kernel:hidden_connect_node(Node) of
            true ->
                ok;
            false ->
                ?P("连接节点:~p失败!", [Node])
        end
    end || NodeName <- Nodes,
        case string:split(NodeName, "@") of
            [NodeStr, _Host] ->
                case string:split(NodeStr, "_", trailing) of % 服务的编号讲道理放后面
                    [ProjectName, _] -> true; % 稳一手，万一shell脚本很菜，搜出来一些别的，或者erlang启动了一些undefined@xxx的节点
                    _ -> false
                end;
            _NodeStr -> false
        end].

reload_module(Mods) ->
    AllModAtoms = [begin
        ModStr = case filename:extension(Mod) of
            "" -> Mod;
            ".erl" -> lists:sublist(Mod, 1, erlang:length(Mod)-4)
        end,
        erlang:list_to_atom(ModStr)
    end || Mod <- Mods],
    [begin
        case rpc:call(Node, wlib_reloader, reload_modules, [AllModAtoms]) of
            ok ->  ?P("节点~p加载模块成功", [Node]);
            Errs ->  ?P("节点~p加载模块失败：~99999p", [Node, Errs])
        end
    end || Node <- erlang:nodes(connected), Node=/=erlang:node()].