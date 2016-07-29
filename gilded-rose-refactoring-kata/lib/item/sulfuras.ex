defmodule Item.Sulfuras do
  defstruct name: nil, sell_in: nil, quality: nil
end

defimpl GildedRose.Update, for: Item.Sulfuras do

  def update(item) do
    item
  end
end
