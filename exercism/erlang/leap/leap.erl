-module(leap).

-export([leap_year/1]).

leap_year(Year) ->
  (0 =:= Year rem 4)
  and
  (0 /= Year rem 100)
  or
  (0 =:= Year rem 400).
