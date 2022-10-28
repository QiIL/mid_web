%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(wtime_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(wtime_server_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #wtime_server_state{}}.

handle_call(_Request, _From, State = #wtime_server_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #wtime_server_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #wtime_server_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #wtime_server_state{}) ->
    ok.

code_change(_OldVsn, State = #wtime_server_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
