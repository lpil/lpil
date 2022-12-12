module Hamming
  def self.compute(xs, ys)
    xs.split('')
      .zip(ys.split '')
      .select { |x, y| x != y }
      .size
  end
end
