defmodule GildedRose do
  def update_quality(items) do
    Enum.map(items, &update_item/1)
  end

  def update_item(item) do
    GildedRose.Update.update(item)
  end
end

defprotocol GildedRose.Update do
  def update(item)
end
