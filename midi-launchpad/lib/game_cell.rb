class GameCell
  attr_writer :neighbours, :alive

  def initialize(alive = false)
    @alive = alive
  end

  def next
    @alive = if @alive
               @neighbours == 2 || @neighbours == 3
             else
               @neighbours == 3
             end
  end

  def to_i
    @alive ? 1 : 0
  end

  def to_s
    @alive ? 'o' : ' '
  end
end
