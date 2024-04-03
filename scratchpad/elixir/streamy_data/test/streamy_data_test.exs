defmodule StreamyDataTest do
  use ExUnit.Case
  doctest StreamyData

  use ExUnitProperties

  property "bin1 <> bin2 always starts with bin1" do
    check all bin1 <- binary(),
              bin2 <- binary() do
      assert String.starts_with?(bin1 <> bin2, bin1)
    end
  end
end
