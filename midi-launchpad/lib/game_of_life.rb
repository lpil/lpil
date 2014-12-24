require_relative 'game_cell'

class GameOfLife
  attr_reader :board

  def initialize(height, width)
    @board = Array.new(height) { Array.new(width) { GameCell.new } }
  end

  def next_gen!
    @board.each.with_index do |row, y|
      row.each.with_index do |cell, x|
        cell.neighbours = count_neighbours y, x
      end
    end
    @board.flatten.each { |cell| cell.next }
    self
  end

  private

  def count_neighbours(y, x)
    [
      [-1, -1], [-1,+0], [-1,+1],
      [+0, -1],          [+0,+1],
      [+1, -1], [+1,+0], [+1,+1]
    ]
    .reduce(0) do |a, e|
      ny = (y + e[0]) % @board.size
      nx = (x + e[1]) % @board[0].size
      a + @board[ny][nx].to_i
    end
  end
end
