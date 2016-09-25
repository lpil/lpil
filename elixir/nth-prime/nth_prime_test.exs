if !System.get_env("EXERCISM_TEST_EXAMPLES") do
  Code.load_file("nth_prime.exs", __DIR__)
end

ExUnit.start
ExUnit.configure exclude: :pending, trace: true

defmodule NthPrimeTest do
  use ExUnit.Case, async: true

  test "first prime" do
    assert Prime.nth(1) == 2
  end

  test "second prime" do
    assert Prime.nth(2) == 3
  end

  test "sixth prime" do
    assert Prime.nth(6) == 13
  end

  test "100th prime" do
    assert Prime.nth(100) == 541
  end

  test "1000th prime" do
    assert Prime.nth(1000) == 7919
  end

  test "2000th prime" do
    assert Prime.nth(2000) == 17389
  end

  test "weird case" do
    catch_error Prime.nth(0)
  end
end
