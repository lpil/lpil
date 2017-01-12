class Xcss::Node
  def reducible?
    false
  end

  def reduce(_env)
    self
  end

  def value
    nil
  end

  def ==(other : Xcss::Node)
    false
  end

  def to_source
    "/* #{self.inspect} */"
  end
end
