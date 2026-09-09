-module(gloss_ffi).

-export([
    is_windows/0, environment_get/1, environment_set/2, environment_unset/1,
    environment_all/0, system_name/0
]).

system_name() ->
    case os:type() of
        {win32, _} -> ~"win32";
        {_, Name} -> erlang:atom_to_binary(Name)
    end.

is_windows() ->
    case os:type() of
        {win32, _} -> true;
        _ -> false
    end.

environment_get(Key) ->
    case os:getenv(unicode:characters_to_list(Key)) of
        false -> {error, nil};
        Value -> {ok, unicode:characters_to_binary(Value)}
    end.

environment_set(Key, Value) ->
    os:putenv(unicode:characters_to_list(Key), unicode:characters_to_list(Value)),
    nil.

environment_unset(Key) ->
    os:unsetenv(unicode:characters_to_list(Key)),
    nil.

environment_all() ->
    BinVars = lists:map(fun(VarString) ->
        [VarName, VarVal] = string:split(VarString, "="),
        {unicode:characters_to_binary(VarName), unicode:characters_to_binary(VarVal)}
    end, os:getenv()),
    maps:from_list(BinVars).
