defmodule Roman do
  @doc """
  Convert the number to a roman number.
  """
  @spec numerals(pos_integer) :: String.t
  def numerals(number) do
    numerals(number, "")
  end

  def numerals(number, numerals) when number >= 1000 do
    numerals(number - 1000, numerals <> "M")
  end

  def numerals(number, numerals) when number >= 900 do
    numerals(number - 900, numerals <> "CM")
  end

  def numerals(number, numerals) when number >= 500 do
    numerals(number - 500, numerals <> "D")
  end

  def numerals(number, numerals) when number >= 400 do
    numerals(number - 400, numerals <> "CD")
  end

  def numerals(number, numerals) when number >= 100 do
    numerals(number - 100, numerals <> "C")
  end

  def numerals(number, numerals) when number >= 90 do
    numerals(number - 90, numerals <> "XC")
  end

  def numerals(number, numerals) when number >= 50 do
    numerals(number - 50, numerals <> "L")
  end

  def numerals(number, numerals) when number >= 40 do
    numerals(number - 40, numerals <> "XL")
  end

  def numerals(number, numerals) when number >= 10 do
    numerals(number - 10, numerals <> "X")
  end

  def numerals(number, numerals) when number >= 9 do
    numerals(number - 9, numerals <> "IX")
  end

  def numerals(number, numerals) when number >= 5 do
    numerals(number - 5, numerals <> "V")
  end

  def numerals(number, numerals) when number >= 4 do
    numerals(number - 4, numerals <> "IV")
  end

  def numerals(number, numerals) when number >= 1 do
    numerals(number - 1, numerals <> "I")
  end

  def numerals(0, numerals), do: numerals
end
