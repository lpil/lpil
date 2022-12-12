class Raindrops
  attr_reader :drops

  def self.convert(drops)
    new(drops).to_noises
  end

  def initialize(drops)
    @drops = drops
  end

  # If the number contains 3 as a prime factor, output 'Pling'.
  # If the number contains 5 as a prime factor, output 'Plang'.
  # If the number contains 7 as a prime factor, output 'Plong'.
  def to_noises
    noises = prime_factors(drops).each_with_object([]) do |n, a|
      case n
      when 3
        a << 'Pling'
      when 5
        a << 'Plang'
      when 7
        a << 'Plong'
      end
    end
    noises.size > 0 ? noises : drops.to_s
  end

  private

  def prime_factors(_num)
    [1]
  end
end
