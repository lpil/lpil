defmodule Item.Brie do
  defstruct name: nil, sell_in: nil, quality: nil
end

defimpl GildedRose.Update, for: Item.Brie do

  def update(%{ quality: 0 } = item) do
    item
  end

  def update(%{ quality: q } = item) when 49 < q do
    item
  end

  def update(item) do
    %{ item | quality: item.quality + 1 }
  end
end
