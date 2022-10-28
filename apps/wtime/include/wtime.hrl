-ifndef(WTIME_HRL).
-define(WTIME_HRL, true).

-define(SEC_PER_MIN, 60).
-define(MIN_PER_HOUR, 60).
-define(HOUR_PER_DAY, 24).
-define(ONE_MINUTE, ?SEC_PER_MIN).
-define(ONE_HOUR, (?MIN_PER_HOUR * ?ONE_MINUTE)).
-define(ONE_DAY, (?HOUR_PER_DAY * ?ONE_HOUR)).

-define(DAY_SECOND, 86400).
-define(HOUR_SECOND, 3600).
-define(MINUTE_SECOND, 60).
-define(WEEK_SECOND, 604800).

-define(MONDAY, 1).
-define(TUESDAY, 2).
-define(WEDNESDAY, 3).
-define(THURSDAY, 4).
-define(FRIDAY, 5).
-define(SATURDAY, 6).
-define(SUNDAY, 7).


-define(GREGORIAN_BEGIN_TIME,
    (case persistent_term:get({?MODULE, universal_timestamp}, undefined) of
        undefined ->
            V = calendar:datetime_to_gregorian_seconds(calendar:universal_time_to_local_time({{1970, 1, 1}, {0, 0, 0}})),
            persistent_term:put({?MODULE, universal_timestamp}, V),
            V;
        V when erlang:is_integer(V) -> V
    end)
).

-endif.