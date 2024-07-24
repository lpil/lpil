-module(atomic_array_ffi).
-export([
    new_signed/1, new_unsigned/1, get/2, get_or_panic/2, size/1, set/3, add/3,
    exchange/3, compare_exchange/4
]).

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

size(Array) ->
    #{size := Size} = atomics:info(Array),
    Size.

set(Array, Index, Value) ->
    try
        atomics:put(Array, Index + 1, Value),
        {ok, nil}
    catch
        error:badarg -> {error, nil}
    end.

add(Array, Index, Amount) ->
    try
        atomics:add(Array, Index + 1, Amount),
        {ok, nil}
    catch
        error:badarg -> {error, nil}
    end.

exchange(Array, Index, Value) ->
    try
        {ok, atomics:exchange(Array, Index + 1, Value)}
    catch
        error:badarg -> {error, nil}
    end.

compare_exchange(Array, Index, Expected, Value) ->
    try
        case atomics:compare_exchange(Array, Index + 1, Expected, Value) of
          ok -> {ok, nil};
          Actual -> {error, {comparison_failed, Actual}}
        end
    catch
        error:badarg -> {error, comparison_out_of_bounds}
    end.
