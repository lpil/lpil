defmodule GildedRoseTest do
  use ExUnit.Case

  import GildedRose, only: [update_item: 1]

  describe "`Backstage passes` item" do
    @pass %Item{ name: "Backstage passes to a TAFKAL80ETC concert", quality: 5, sell_in: 5 }

    test "quality == 0, noop" do
      item = %{ @pass | quality: 0 }
      assert update_item(item).quality == 0
    end

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


  describe "`Aged Brie` item" do
    @brie %Item{ name: "Aged Brie", quality: 5, sell_in: 5 }

    test "quality == 0, noop" do
      item = %{ @brie | quality: 0 }
      assert update_item(item).quality == 0
    end

    test "quality != 0, quality += 1" do
      item = %{ @brie | quality: -4, sell_in: -3 }
      assert update_item(item).quality == -3
      item = %{ @brie | sell_in: 151 }
      assert update_item(item).quality == 6
    end

    test "quality stops growing at 50" do
      item = %{ @brie | sell_in: 12, quality: 49 }
      assert update_item(item).quality == 50
      item = %{ @brie | sell_in: 12, quality: 50 }
      assert update_item(item).quality == 50
      item = %{ @brie | sell_in: 12, quality: 51 }
      assert update_item(item).quality == 51
    end

    test "name and sell_in don't change" do
      assert update_item(@brie).name == @brie.name
      assert update_item(@brie).sell_in == @brie.sell_in
    end
  end


  describe "`Sulfuras, Hand of Ragnaros` item" do
    @hand %Item{ name: "Sulfuras, Hand of Ragnaros", quality: 5, sell_in: 5 }

    test "nothing ever changes" do
      for item <- [
        %{ @hand | quality: 0 },
        %{ @hand | quality: -4, sell_in: -3 },
        %{ @hand | sell_in: 151 },
        %{ @hand | sell_in: 12, quality: 49 },
        %{ @hand | sell_in: 12, quality: 50 },
        %{ @hand | sell_in: 12, quality: 51 },
      ] do
        assert update_item(item).quality == item.quality
        assert update_item(item).name == item.name
        assert update_item(item).sell_in == item.sell_in
      end
    end
  end


  describe "`+5 Dexterity Vest` item" do
    @vest %Item{ name: "+5 Dexterity Vest", quality: 5, sell_in: 5 }

    test "quality == 0, noop" do
      item = %{ @vest | quality: 0 }
      assert update_item(item).quality == 0
    end

    test "sell_in < 0, quality -= 2" do
      item = %{ @vest | sell_in: -1 }
      assert update_item(item).quality == 3
      item = %{ @vest | sell_in: -6, quality: 4 }
      assert update_item(item).quality == 2
    end

    test "-1 < sell_in, quality -= 1" do
      item = %{ @vest | sell_in: 0, quality: 4 }
      assert update_item(item).quality == 3
      item = %{ @vest | sell_in: 100, quality: 4 }
      assert update_item(item).quality == 3
    end

    test "name and sell_in don't change" do
      assert update_item(@vest).name == @vest.name
      assert update_item(@vest).sell_in == @vest.sell_in
    end
  end


  describe "`Elixir of the Mongoose` item" do
    @elixir %Item{ name: "Elixir of the Mongoose", quality: 5, sell_in: 5 }

    test "quality == 0, noop" do
      item = %{ @elixir | quality: 0 }
      assert update_item(item).quality == 0
    end

    test "sell_in < 0, quality -= 2" do
      item = %{ @elixir | sell_in: -1 }
      assert update_item(item).quality == 3
      item = %{ @elixir | sell_in: -6, quality: 4 }
      assert update_item(item).quality == 2
    end

    test "-1 < sell_in, quality -= 1" do
      item = %{ @elixir | sell_in: 0, quality: 4 }
      assert update_item(item).quality == 3
      item = %{ @elixir | sell_in: 100, quality: 4 }
      assert update_item(item).quality == 3
    end

    test "name and sell_in don't change" do
      assert update_item(@elixir).name == @elixir.name
      assert update_item(@elixir).sell_in == @elixir.sell_in
    end
  end

  describe "Any other item" do
    @generic %Item{ name: "Super duper", quality: 5, sell_in: 5 }

    test "quality == 0, noop" do
      item = %{ @generic | quality: 0 }
      assert update_item(item).quality == 0
    end

    test "sell_in < 0, noop" do
      item = %{ @generic | sell_in: -1 }
      assert update_item(item).quality == 5
      item = %{ @generic | sell_in: -18 }
      assert update_item(item).quality == 5
    end

    test "-1 < sell_in, quality - 1" do
      item = %{ @generic | sell_in: -1 }
      assert update_item(item).quality == 5
      item = %{ @generic | sell_in: 0 }
      assert update_item(item).quality == 4
      item = %{ @generic | sell_in: 1 }
      assert update_item(item).quality == 4
    end

    test "name and sell_in don't change" do
      assert update_item(@generic).name == @generic.name
      assert update_item(@generic).sell_in == @generic.sell_in
    end
  end
end
