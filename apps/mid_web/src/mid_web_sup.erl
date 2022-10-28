-module(mid_web_sup).
-behavior(supervisor).
%% API
-export([
    start_link/0,
    init/1
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok, {
        #{
            strategy => one_for_one,
            intensity => 10,
            period => 10
        }, 
        [
            #{
                id => mid_web_data,
                start => {mid_web_data, start_link, []},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [mid_web_data]
            }
        ]
    }}.