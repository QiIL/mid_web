-ifndef(MIN_WEB_HRL).
-define(MIN_WEB_HRL, true).
-define(WEB_LISTENER, mid_web_listener).
-include_lib("wlog/include/wlog.hrl").

-define(THROW_FORBIDDEN(Data), ?THROW_ERR(403, Data)).
-define(THROW_NOT_FOUND(), ?THROW_ERR(404, <<>>)).
-define(THROW_NOT_FOUND(Data), ?THROW_ERR(404, Data)).
-define(THROW_ERR(Data), erlang:throw({service_error, 400, Data})).
-define(THROW_ERR(Code, Data), erlang:throw({service_error, Code, Data})).

-endif.