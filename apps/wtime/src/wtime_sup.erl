-module(wtime_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    AChild = #{id => wtime_server,
        start => {wtime_server, start_link, []},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [wtime_server]},

    {ok, {#{strategy => simple_one_for_one,
        intensity => 5,
        period => 30},
        [AChild]}
    }.
