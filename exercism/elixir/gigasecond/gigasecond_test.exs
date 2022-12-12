if System.get_env("EXERCISM_TEST_EXAMPLES") do
  Code.load_file("example.exs")
else
  Code.load_file("gigasecond.exs")
end

ExUnit.start

defmodule GigasecondTest do
  use ExUnit.Case

  test "from 4/25/2011" do
    assert Gigasecond.from({2011, 4, 25}) == {2043, 1, 1}
  end

  test "from 6/13/1977" do
    assert Gigasecond.from({1977, 6, 13}) == {2009, 2, 19}
  end

  test "from 7/19/1959" do
    assert Gigasecond.from({1959, 7, 19}) == {1991, 3, 27}
  end

  test "from my birthday" do
    my_birthday = {1991, 1, 5}
    assert Gigasecond.from(my_birthday) == {2022, 9, 13}
  end
end

