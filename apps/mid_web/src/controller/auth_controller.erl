%%%-------------------------------------------------------------------
%%% @doc 
%%%-------------------------------------------------------------------
-module(auth_controller).

%% API
-export([test/1]).

test(_Req) ->
    {ok, <<"I am auth controller">>}.