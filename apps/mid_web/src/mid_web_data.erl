%%%-------------------------------------------------------------------
%%% @doc 初始化启动
%%%-------------------------------------------------------------------
-module(mid_web_data).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(mid_web_data_state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #mid_web_data_state{}}.

handle_call(_Request, _From, State = #mid_web_data_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #mid_web_data_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #mid_web_data_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #mid_web_data_state{}) ->
    ok.

code_change(_OldVsn, State = #mid_web_data_state{}, _Extra) ->
    {ok, State}.
