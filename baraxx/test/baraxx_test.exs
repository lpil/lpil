defmodule BaraxxTest do
  use ExUnit.Case
  doctest Baraxx

  test "greets the world" do
    assert Baraxx.hello() == :world
  end
end
