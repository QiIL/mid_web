-module(wconf).

%% API
-export([
    find/2,
    find/3
]).
%% @doc 根据键值查询配置
find(ConfigName,Key)->
    case ConfigName:find(Key) of
        undefined-> [];
        not_implement -> [];
        Val -> [Val]
    end.
%% @doc 根据键值查询配置，没有则返回默认值
find(ConfigName,Key, Default)->
    case ConfigName:find(Key) of
        undefined-> Default;
        not_implement -> Default;
        Val -> [Val]
    end.