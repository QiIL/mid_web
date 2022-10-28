-ifndef(WLOG_HRL).
-define(WLOG_HRL, true).
-include_lib("lager/include/lager.hrl").

-define(IS_LOG_ALLOW(__Lv), (__Lv =< (wlog:get_num()))).
-define(MAYBE_LOG(__Lv, __Expr), (case ?IS_LOG_ALLOW(__Lv) of true -> __Expr; _ -> ignore end)).

%% 程序日志记录宏
-define(DEBUG_MSG(Meta, Format, Args),        ?MAYBE_LOG((?DEBUG),     (?lager_debug(Meta, Format, Args)))    ).
-define(DEBUG_MSG(Format, Args),              ?MAYBE_LOG((?DEBUG),     (?lager_debug(Format, Args)))          ).
-define(DEBUG_MSG(D),                         ?DEBUG_MSG(D, [])).

-define(INFO_MSG(Meta, Format, Args),         ?MAYBE_LOG((?INFO),      (?lager_info(Meta, Format, Args)))     ).
-define(INFO_MSG(Format, Args),               ?MAYBE_LOG((?INFO),      (?lager_info(Format, Args)))           ).
-define(INFO_MSG(D),                          ?INFO_MSG(D, [])).

-define(NOTICE_MSG(Meta, Format, Args),       ?MAYBE_LOG((?NOTICE),    (?lager_notice(Meta, Format, Args)))   ).
-define(NOTICE_MSG(Format, Args),             ?MAYBE_LOG((?NOTICE),    (?lager_notice(Format, Args)))         ).
-define(NOTICE_MSG(D),                        ?NOTICE_MSG(D, [])).

-define(WARNING_MSG(Meta, Format, Args),      ?MAYBE_LOG((?WARNING),   (?lager_warning(Meta, Format, Args)))  ).
-define(WARNING_MSG(Format, Args),            ?MAYBE_LOG((?WARNING),   (?lager_warning(Format, Args)))        ).
-define(WARNING_MSG(D),                       ?WARNING_MSG(D, [])).

-define(ERROR_MSG(Meta, Format, Args),        ?MAYBE_LOG((?ERROR),     (?lager_error(Meta, Format, Args)))    ).
-define(ERROR_MSG(Format, Args),              ?MAYBE_LOG((?ERROR),     (?lager_error(Format, Args)))          ).
-define(ERROR_MSG(D),                         ?ERROR_MSG(D, [])).

-define(CRITICAL_MSG(Meta, Format, Args),     ?MAYBE_LOG((?CRITICAL),  (?lager_critical(Meta, Format, Args))) ).
-define(CRITICAL_MSG(Format, Args),           ?MAYBE_LOG((?CRITICAL),  (?lager_critical(Format, Args)))       ).
-define(CRITICAL_MSG(D),                      ?CRITICAL_MSG(D, [])).

-define(ALERT_MSG(Meta, Format, Args),        ?MAYBE_LOG((?ALERT),     (?lager_alert(Meta, Format, Args)))    ).
-define(ALERT_MSG(Format, Args),              ?MAYBE_LOG((?ALERT),     (?lager_alert(Format, Args)))          ).
-define(ALERT_MSG(D),                         ?ALERT_MSG(D, [])).

-define(EMERGENCY_MSG(Meta, Format, Args),    ?MAYBE_LOG((?EMERGENCY), (?lager_emergency(Meta, Format, Args)))).
-define(EMERGENCY_MSG(Format, Args),          ?MAYBE_LOG((?EMERGENCY), (?lager_emergency(Format, Args)))      ).
-define(EMERGENCY_MSG(D),                     ?EMERGENCY_MSG(D, [])).

-define(NONE_MSG(Meta, Format, Args),         ?MAYBE_LOG((?LOG_NONE),  (ok))                                       ).
-define(NONE_MSG(Format, Args),               ?MAYBE_LOG((?LOG_NONE),  (ok))                                       ).
-define(NONE_MSG(D),                          ?NONE_MSG(D, [])).


-endif.