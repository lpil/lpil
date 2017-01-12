class Xcss::Node::Add < Xcss::Node
  getter lhs : Xcss::Node
  getter rhs : Xcss::Node

  def initialize(lhs, rhs)
    @lhs = lhs
    @rhs = rhs
  end

  def reducible?
    true
  end

  def reduce(env)
    if lhs.reducible?
      self.class.new(lhs.reduce(env), rhs)
    elsif rhs.reducible?
      self.class.new(lhs, rhs.reduce(env))
    else
      do_add(lhs, rhs)
    end
  end

  def ==(other)
    other.is_a?(Add) &&
      self.lhs == other.lhs &&
      self.rhs == other.rhs
  end

  def to_source
    "add(#{lhs.to_source}, #{rhs.to_source})"
  end

  private def do_add(x : Number, y : Number)
    Number.new(x.value + y.value)
  end

  private def do_add(x, y)
    raise "type error"
  end
end
