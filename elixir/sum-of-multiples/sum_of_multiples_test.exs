if !System.get_env("EXERCISM_TEST_EXAMPLES") do
  Code.load_file("sum_of_multiples.exs", __DIR__)
end

ExUnit.start
ExUnit.configure exclude: :pending, trace: true

defmodule SumOfMultiplesTest do
  use ExUnit.Case

  test "sum to 1" do
    assert SumOfMultiples.to(1, [3, 5]) == 0
  end

  test "sum to 3" do
    assert SumOfMultiples.to(4, [3, 5]) == 3
  end

  test "sum to 10" do
    assert SumOfMultiples.to(10, [3, 5]) == 23
  end

  test "sum to 20" do
    assert SumOfMultiples.to(20, [3, 5]) == 78
  end

  test "sum to 100" do
    assert SumOfMultiples.to(100, [3, 5]) == 2318
  end

  test "sum to 1000" do
    assert SumOfMultiples.to(1000, [3, 5]) == 233168
  end

  test "configurable 7, 13, 17 to 20" do
    multiples = [7, 13, 17]
    assert SumOfMultiples.to(20, multiples) == 51
  end

  test "configurable 4, 6 to 15" do
    multiples = [4, 6]
    assert SumOfMultiples.to(15, multiples) == 30
  end

  test "configurable 5, 6, 8 to 150" do
    multiples = [5, 6, 8]
    assert SumOfMultiples.to(150, multiples) == 4419
  end

  test "configurable 43, 47 to 10000" do
    multiples = [43, 47]
    assert SumOfMultiples.to(10000, multiples) == 2203160
  end
end
