-module(badminton_ffi).

-export([coerce_tuple_to_list/1]).

coerce_tuple_to_list(Data) when is_tuple(Data) -> tuple_to_list(Data);
coerce_tuple_to_list(Data) -> Data.
