if !System.get_env("EXERCISM_TEST_EXAMPLES") do
  Code.load_file("bracket_push.exs", __DIR__)
end

ExUnit.start
ExUnit.configure exclude: :pending, trace: true

defmodule BracketPushTest do
  use ExUnit.Case

  test "empty string" do
    assert BracketPush.check_brackets("")
  end

  test "appropriate bracketing in a set of brackets" do
    assert BracketPush.check_brackets("{}")
  end

  test "unclosed brackets" do
    refute BracketPush.check_brackets("{{")
  end

  test "more than one pair of brackets" do
    assert BracketPush.check_brackets("{}[]")
  end

  test "brackets are out of order" do
    refute BracketPush.check_brackets("}{")
  end

  test "nested brackets" do
    assert BracketPush.check_brackets("{[()]}")
  end

  test "unbalanced nested brackets" do
    refute BracketPush.check_brackets("{[}]")
  end

  test "bracket closure with deeper nesting" do
    refute BracketPush.check_brackets("{[)][]}")
  end

  test "bracket closure in a long string of brackets" do
    assert BracketPush.check_brackets("{[]([()])}")
  end

  test "should ignore non-bracket characters" do
    assert BracketPush.check_brackets("{hello[]([a()])b}c")
  end

  test "string with newlines" do
    assert BracketPush.check_brackets("[]\n{()}\n[(({}))]\n")
  end
end
