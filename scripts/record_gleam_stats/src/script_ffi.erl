-module(script_ffi).

-export([timestamp/0]).

timestamp() ->
    Now = erlang:system_time(second),
    Timestamp = list_to_binary(calendar:system_time_to_rfc3339(Now)),
    Y = binary:part(Timestamp, 0, 4),
    M = binary:part(Timestamp, 5, 2),
    D = binary:part(Timestamp, 8, 2),
    <<Y/binary, "/", M/binary, "/", D/binary>>.
