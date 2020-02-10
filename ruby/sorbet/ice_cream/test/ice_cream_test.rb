# typed: strong
require "test_helper"

class IceCreamTest < Minitest::Test
  extend T::Sig

  sig { returns(T::Boolean) }
  def test_that_it_has_a_version_number
    refute_nil ::IceCream::VERSION
  end

  sig { returns(T::Boolean) }
  def test_it_does_something_useful
    assert false
  end
end
