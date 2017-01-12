class Xcss::Node::String < Xcss::Node
  getter! value : ::String

  def initialize(value)
    @value = value
  end

  def ==(other : Xcss::Node)
    other.is_a?(Xcss::Node::String) &&
      self.value == other.value
  end

  def to_source
    self.value.inspect
  end
end
