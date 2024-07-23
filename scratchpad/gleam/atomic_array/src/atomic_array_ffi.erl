-module(atomic_array_ffi).
-export([new_signed/1, new_unsigned/1, get/2, get_or_panic/2]).

new_signed(Size) ->
    atomics:new(Size, [{signed, true}]).

new_unsigned(Size) ->
    atomics:new(Size, [{signed, false}]).

get_or_panic(Array, Index) ->
    atomics:get(Array, Index + 1).

get(Array, Index) ->
    try
        {ok, atomics:get(Array, Index + 1)}
    catch
        error:badarg -> {error, nil}
    end.
