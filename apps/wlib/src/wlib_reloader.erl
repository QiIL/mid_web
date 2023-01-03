%%%-------------------------------------------------------------------
%%% @doc reload module
%%%-------------------------------------------------------------------
-module(wlib_reloader).
-include_lib("wlog/include/wlog.hrl").
-include("wlib.hrl").
%% API
-export([
    reload_all_config/0,
    reload_modules/1
]).

%% only reload ./config/*.config
reload_all_config() -> 
    config_misc:reload_all().

reload_modules(Mod) when is_atom(Mod) ->
    reload_modules([Mod]);
reload_modules(Modules) ->
    [ code:purge(Module) || Module<- Modules],
    Ret = code:atomic_load(Modules),
    ?INFO_MSG("Module:~w, reload:~w~n", [Modules, Ret]),
    case Ret of
        ok ->
            [ ?TRY_CATCH(reload_beam_hook(Module)) || Module <- Modules];
        _ ->
            ignore
    end,
    Ret.

reload_beam_hook(_) -> ok.