defmodule Item.Magic do
  defstruct name: nil, sell_in: nil, quality: nil
end

defimpl GildedRose.Update, for: Item.Magic do

  def update(%{ quality: 0 } = item) do
    item
  end

  def update(%{ sell_in: s } = item) when s < 0 do
    %{ item | quality: item.quality - 2 }
  end

  def update(%{ sell_in: s } = item) when -1 < s do
    %{ item | quality: item.quality - 1 }
  end

  def update(item) do
    item
  end
end
