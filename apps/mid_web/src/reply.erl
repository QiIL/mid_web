%%%-------------------------------------------------------------------
%%% @doc 返回封装
%%% 逻辑返回的封装：
%%% {code:Code, ret:Success, data:Data::Map}
%%%-------------------------------------------------------------------
-module(reply).
-include("mid_web.hrl").

%% API
-export([
    ok/1, ok/2, ok/3, ok/4,
    err/2, err/3, err/4
]).

ok(Req) ->
    ok(Req, 200, get_header(), <<>>).
ok(Req, Data) ->
    ok(Req, 200, get_header(), jsx:encode(Data)).
ok(Req, Code, Data) ->
    ok(Req, Code, get_header(), jsx:encode(Data)).

%% @return {code:Code, ret:1, data:Data}
ok(Req, Code, Header, Data) ->
    Ret = jsx:encode(#{<<"code">> => Code, <<"ret">> => 1, <<"data">> => Data}),
    cowboy_req:reply(Code, Header, Ret, Req).

err(Req, Data) ->
    err(Req, 400, get_header(), Data).
err(Req, Code, Data) ->
    err(Req, Code, get_header(), Data).
%% @return {code:Code, ret:0, data:Data}
err(Req, Code, Header, Data) ->
    Ret = jsx:encode(#{<<"code">> => Code, <<"ret">> => 0, <<"data">> => Data}),
    cowboy_req:reply(Code, Header, Ret, Req).

get_header() ->
    case wconf:find(env, is_debug) of
        [true] ->
            #{
                <<"content-type">> => <<"application/json; charset=utf-8">>,
                <<"Access-Control-Allow-Origin">> => <<"*">>,
                <<"Access-Control-Allow-Creadentials">> => <<"true">>
            };
        _ ->
            #{
                <<"content-type">> => <<"application/json; charset=utf-8">>
            }
    end.