require 'minitest/autorun'
require 'minitest/pride'
require 'date'
require 'time'

require_relative 'gigasecond'
class GigasecondTest < MiniTest::Unit::TestCase
  def test_1
    gs = Gigasecond.from(Time.utc(2011, 4, 25))
    assert_equal Time.utc(2043, 1, 1, 1, 46, 40), gs
  end

  def test_2
    gs = Gigasecond.from(Time.utc(1977, 6, 13))
    assert_equal Time.utc(2009, 2, 19, 1, 46, 40), gs
  end

  def test_3
    gs = Gigasecond.from(Time.utc(1959, 7, 19))
    assert_equal Time.utc(1991, 3, 27, 1, 46, 40), gs
  end

  def test_4_with_seconds
    gs = Gigasecond.from(Time.utc(1959, 7, 19, 23, 59, 59))
    assert_equal Time.utc(1991, 3, 28, 1, 46, 39), gs
  end
end
