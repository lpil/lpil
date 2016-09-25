if !System.get_env("EXERCISM_TEST_EXAMPLES") do
  Code.load_file("isogram.exs", __DIR__)
end

ExUnit.start
ExUnit.configure exclude: :pending, trace: true

defmodule IsogramTest do
  use ExUnit.Case

  test "isogram lowercase" do
    assert Isogram.isogram?("subdermatoglyphic")
  end

  test "not isogram lowercase " do
    refute Isogram.isogram?("eleven")
  end

  test "isogram uppercase" do
    assert Isogram.isogram?("DEMONSTRABLY")
  end

  test "not isogram uppercase" do
    refute Isogram.isogram?("ALPHABET")
  end

  test "isogram with dash" do
    assert Isogram.isogram?("hjelmqvist-gryb-zock-pfund-wax")
  end

  test "not isogram with dash" do
    refute Isogram.isogram?("twenty-five")
  end

  test "isogram with utf-8 letters" do
    assert Isogram.isogram?("heizölrückstoßabdämpfung")
  end

  test "not isogram with utf-8 letters" do
    refute Isogram.isogram?("éléphant")
  end

  test "phrase is isogram" do
    assert Isogram.isogram?("emily jung schwartzkopf")
  end

  test "phrase is not isogram" do
    refute Isogram.isogram?("the quick brown fox")
  end

end
