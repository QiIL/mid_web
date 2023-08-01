%% -*- coding: utf-8 -*-
-module(wdb).
-include("wdb.hrl").
-export([
    read/2,
    read/3,
    insert/2,
    insert_col/2,
    update/3,
    test_speed/0
]).

read(Tab, Key) ->
    io:format(<<"SELECT * FROM ~w WHERE id = ~p~n">>, [Tab, Key]),
    mysql_poolboy:query(?QUERY_POOL, io_lib:format(<<"SELECT * FROM ~w WHERE id = ~w~n">>, [Tab, Key])).
    
read(Tab, KeyName, Key) ->
    io:format(<<"SELECT * FROM ~w WHERE ~p = ~p~n">>, [Tab, KeyName, Key]),
    mysql_poolboy:query(?QUERY_POOL, <<"SELECT * FROM ? WHERE ? = ?;">>, [wlib_tool:to_list(Tab), wlib_tool:to_list(KeyName), wlib_tool:to_binary(Key)]).

insert(Tab, ValList) ->
    QueryStr = wlib_tool:concat([
        "INSERT INTO ",
        Tab,
        " VALUES ( ",
        string:join(["?" || _ <- lists:seq(1, erlang:length(ValList))], ", "),
        " ) "
    ]),
    Params = [wlib_tool:to_list(Val) || Val <- ValList],
    io:format("query: ~p~n params: ~p~n", [QueryStr, Params]),
    mysql_poolboy:query(?QUERY_POOL, QueryStr, Params).

insert_col(Tab, KVList) ->
    QueryStr = wlib_tool:concat([
        "INSERT INTO ",
        Tab,
        " ( ",
        string:join([wlib_tool:to_list(Key) || {Key, _} <- KVList], ", "),
        " ) VALUES ( ",
        string:join(["?" || _ <- lists:seq(1, erlang:length(KVList))], ", "),
        " ) "
    ]),
    Params = [wlib_tool:to_list(Val) || {_, Val} <- KVList],
    io:format("query: ~p~n params: ~p~n", [QueryStr, Params]),
    mysql_poolboy:query(?QUERY_POOL, QueryStr, Params).

update(Tab, Key, KVList) ->
    QueryStr = wlib_tool:concat([
        "UPDATE ",
        wlib_tool:to_binary(Tab),
        " SET ",
        [[wlib_tool:to_list(K), " = ", wlib_tool:to_list(V)] || {K, V} <- KVList],
        " WHERE ID = ", Key
    ]),
    io:format("query: ~p~n", [QueryStr]),
    mysql_poolboy:query(?QUERY_POOL, QueryStr, []).

%% 字符串拼接更快
test_speed() ->
    KVList = [{ID, wlib_tool:to_list(ID)} || ID <- lists:seq(1, 100)],
    T1 = timer:tc(fun() -> test_merge_string(KVList, 10000) end),
    T2 = timer:tc(fun() -> test_merge_binary(KVList, 10000) end),
    {T1, T2}.

test_merge_string(_KVList, 0) -> ok;
test_merge_string(KVList, Times) ->
    wlib_tool:concat([
        "INSERT INTO ",
        aa,
        " ( ",
        string:join([wlib_tool:to_list(Key) || {Key, _} <- KVList], ", "),
        " ) VALUES ( ",
        string:join(["?" || _ <- lists:seq(1, erlang:length(KVList))], ", "),
        " ) "
    ]),
    test_merge_string(KVList, Times-1).

test_merge_binary(_, 0) -> ok;
test_merge_binary(KVList, Times) ->
    {KeyParams, _ValParams} = make_insert_col_params(KVList),
    QMS = make_question_marks(KVList),
    <<
        <<"INSERT INTO ">>/binary,
        (wlib_tool:to_binary(a))/binary,
        <<" ( ">>/binary, KeyParams/binary,
        <<" ) VALUES ( ">>/binary,
        QMS/binary,
        <<" )">>/binary
    >>,
    test_merge_binary(KVList, Times-1).

make_question_marks(KVList) ->
    A = binary:copy(<<"?, ">>, erlang:length(KVList)-1),
    <<A/binary, <<"?">>/binary>>.

make_insert_col_params(List) ->
    make_insert_col_params(List, <<"">>, <<"">>).
make_insert_col_params([{Key, Val}| []], KeyBin, ValBin) ->
    {<<KeyBin/binary, (wlib_tool:to_binary(Key))/binary>>,
        <<ValBin/binary, (wlib_tool:to_binary(Val))/binary>>};
make_insert_col_params([{Key, Val}| T], KeyBin, ValBin) ->
    make_insert_col_params(T, <<KeyBin/binary, (wlib_tool:to_binary(Key))/binary, <<", ">>/binary>>,
        <<ValBin/binary, (wlib_tool:to_binary(Val))/binary, <<", ">>/binary>>).
    