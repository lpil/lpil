class Xcss::Node::Number < Xcss::Node
  getter! value : Float64

  def initialize(value)
    @value = value
  end

  def ==(other)
    other.is_a?(Xcss::Node::Number) &&
      self.value == other.value
  end

  def to_source
    self.value.to_s
  end
end
