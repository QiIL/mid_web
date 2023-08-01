%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @doc 各种工具
%%%-------------------------------------------------------------------
-module(wlib_tool).

%% API
-export([
    concat/1
]).

%% base api
-export([
    to_atom/1,
    to_list/1,
    to_binary/1,
    to_iolist/1
]).

concat(List) ->
    lists:concat([?MODULE:to_list(T) || T <- List]).

%% @doc convert other type to atom
%% @throws other_value
-spec to_atom(atom() | binary() | list() | integer() | term()) -> atom().
to_atom(Atom) when erlang:is_atom(Atom) ->
    Atom;
to_atom(Bin) when erlang:is_binary(Bin) ->
    erlang:binary_to_atom(Bin);
to_atom(List) when erlang:is_list(List) ->
    erlang:list_to_atom(List);
to_atom(Int) when erlang:is_integer(Int) ->
    erlang:list_to_atom(erlang:integer_to_list(Int));
to_atom(_Other) ->
    erlang:throw({other_value, {_Other, wlib_sys:get_stacktrace()}}).


%% @doc convert other type to list
%% @throws other_value
-spec to_list(list() | atom() | binary() | tuple() | integer() | float() | term() | pid()) -> list().
to_list(T) when erlang:is_list(T) ->
    T;
to_list(T) when erlang:is_atom(T) ->
    erlang:atom_to_list(T);
to_list(T) when erlang:is_tuple(T) -> 
    erlang:tuple_to_list(T);
to_list(T) when erlang:is_integer(T) -> 
    erlang:integer_to_list(T);
to_list(T) when erlang:is_binary(T) -> 
    erlang:binary_to_list(T);
to_list(T) when erlang:is_float(T) ->
    float_to_str(T);
to_list(T) when erlang:is_pid(T) ->
    erlang:pid_to_list(T);
to_list(_Other) ->
    erlang:throw({other_value, {_Other, wlib_sys:get_stacktrace()}}).

%% @doc convert other type to binary
%% @throws other_value
-spec to_binary(any()) -> binary().
to_binary(Bin) when erlang:is_binary(Bin) ->
    Bin;
to_binary(Atom) when erlang:is_atom(Atom) ->
    erlang:atom_to_binary(Atom);
to_binary(List) when erlang:is_list(List) ->
    erlang:list_to_binary(List);
to_binary(Int) when erlang:is_integer(Int) ->
    erlang:integer_to_binary(Int);
to_binary(Float) when erlang:is_float(Float) ->
    erlang:float_to_binary(Float);
to_binary(PID) when erlang:is_pid(PID) ->
    erlang:list_to_binary(erlang:pid_to_list(PID));
to_binary(_Other) ->
    erlang:throw({other_value, {_Other, ylib_sys:get_stacktrace()}}).

%% @doc
%% 不严格的的to_iolist，对于 16#80 - 16#FF 的字符不会转换为UTF-8，满足多数中文编码不一致的环境
%% 对返回结果
%%     1. 调用  fun erlang:iolist_to_binary/1 的结果可以用于io:format的输出
%%     2. 直接写入文件
%% @end
to_iolist(Bin) when erlang:is_binary(Bin) ->
    Bin;
to_iolist(ListOrInt) when erlang:is_list(ListOrInt);erlang:is_integer(ListOrInt) ->
    lists:reverse(to_iolist_r(ListOrInt, [], [])).

to_iolist_r([C | Less], List, Res) ->
    to_iolist_r(C, [Less | List], Res);
to_iolist_r(C, List, Res) when erlang:is_integer(C), C >=0, C =< 16#FF;erlang:is_binary(C) ->
    to_iolist_r([], List, [C | Res]);
to_iolist_r(C, List, Res)
    when erlang:is_integer(C), C > 16#FF, C < 16#D800;
    erlang:is_integer(C), C > 16#DFFF, C < 16#FFFE;
    erlang:is_integer(C), C > 16#FFFF, C =< 16#10FFFF ->
    to_iolist_r([], List, [wlib_utf8:codepoint_to_bytes(C) | Res]);
to_iolist_r([], [Less | List], Res) ->
    to_iolist_r(Less, List, Res);
to_iolist_r([], [], Res) ->
    Res.



%% @doc convert float to string: 1.58678->1.58
-spec float_to_str(integer() | float()) -> string().
float_to_str(N) when erlang:is_integer(N) ->
    integer_to_list(N) ++ ".00";
float_to_str(F) when erlang:is_float(F) ->
    io_lib:format("~.2f", [F]).
