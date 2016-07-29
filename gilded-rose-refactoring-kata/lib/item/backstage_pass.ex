defmodule Item.BackstagePass do
  defstruct name: nil, sell_in: nil, quality: nil
end

defimpl GildedRose.Update, for: Item.BackstagePass do
  def update(%{ quality: 0 } = item) do
    item
  end

  def update(%{ quality: q } = item) when 49 < q do
    item
  end

  def update(%{ sell_in: s } = item) when s < 0 do
    %{ item | quality: 0 }
  end

  def update(%{ sell_in: s } = item) when -1 < s and s < 6 do
    %{ item | quality: item.quality + 3 }
  end

  def update(%{ sell_in: s } = item) when 5 < s and s < 11 do
    %{ item | quality: item.quality + 2 }
  end

  def update(%{ sell_in: s } = item) when 11 < s do
    %{ item | quality: item.quality + 1 }
  end

  def update(item) do
    item
  end
end
