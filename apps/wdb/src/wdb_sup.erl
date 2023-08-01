-module(wdb_sup).

-behaviour(supervisor).
-include("wdb.hrl").
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {PoolOpt, MysqlOpt}} = application:get_env(mysql_poolboy, ?QUERY_POOL),
    {ok, {#{strategy => one_for_one,
        intensity => 5,
        period => 30},
        [mysql_poolboy:child_spec(mid_web, PoolOpt, MysqlOpt)]}
    }.
