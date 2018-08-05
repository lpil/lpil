defmodule Item.Sulfuras do
  defstruct Item.fields
end

defimpl GildedRose.Update, for: Item.Sulfuras do

  def update(item) do
    item
  end
end
