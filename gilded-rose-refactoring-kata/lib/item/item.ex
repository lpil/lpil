defmodule Item do
  defstruct name: nil, sell_in: nil, quality: nil
end

defimpl GildedRose.Update, for: Item do

  def update(%Item{} = item) do
    item
    |> struct_type
    |> struct(Map.from_struct(item))
    |> GildedRose.Update.update
  end

  def struct_type(%Item{} = item) do
    case item.name do
      "Backstage passes to a TAFKAL80ETC concert" ->
        Item.BackstagePass
      "Aged Brie" ->
        Item.Brie
      "Sulfuras, Hand of Ragnaros" ->
        Item.Sulfuras
      _ ->
        Item.Plain
    end
  end
end
