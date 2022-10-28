-ifndef(CONFIG_HRL).
-define(CONFIG_HRL, true).
%%
%%-ifndef(CONFIG_GEN_ALL_KEY).
%%-define(CONFIG_GEN_ALL_KEY, config_gen_all_key).
%%-compile({parse_transform, config_gen_all_key}).
%%-endif.

-compile(nowarn_export_all).
-compile(export_all).

-define(CFG_H, find(K) -> case K of ).
-define(C(K,V),  K -> V; ).
-define(CFG_E,  _Other -> undefined
    end ).

%% 添加该宏会自动生成all_keys函数
-define(ALL_KEYS, all_keys() -> auto_gen).

-endif.