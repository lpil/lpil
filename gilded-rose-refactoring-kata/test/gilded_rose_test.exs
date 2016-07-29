defmodule GildedRoseTest do
  use ExUnit.Case

  import GildedRose, only: [update_item: 1]

  describe "`Backstage passes` item" do
    @pass %Item{ name: "Backstage passes to a TAFKAL80ETC concert", quality: 5, sell_in: 5 }

    test "sell_in < 0, quality = 0" do
      item = %{ @pass | sell_in: -1 }
      assert update_item(item).quality == 0
    end

    test "0 <= sell_in < 6, quality += 3" do
      item = %{ @pass | sell_in: 0 }
      assert update_item(item).quality == 8
      item = %{ @pass | sell_in: 5 }
      assert update_item(item).quality == 8
    end

    test "6 <= sell_in < 11, quality += 2" do
      item = %{ @pass | sell_in: 6 }
      assert update_item(item).quality == 7
      item = %{ @pass | sell_in: 10 }
      assert update_item(item).quality == 7
    end

    test "12 <= sell_in, += 1" do
      item = %{ @pass | sell_in: 12 }
      assert update_item(item).quality == 6
      item = %{ @pass | sell_in: 150 }
      assert update_item(item).quality == 6
    end

    test "quality stops growing at 50" do
      item = %{ @pass | sell_in: 12, quality: 49 }
      assert update_item(item).quality == 50
      item = %{ @pass | sell_in: 12, quality: 50 }
      assert update_item(item).quality == 50
      item = %{ @pass | sell_in: 12, quality: 51 }
      assert update_item(item).quality == 51
    end

    test "name and sell_in don't change" do
      assert update_item(@pass).name == @pass.name
      assert update_item(@pass).sell_in == @pass.sell_in
    end
  end
end
