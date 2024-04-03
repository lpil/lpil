# TODO: Write documentation for `Sum`
module Sum
  VERSION = "0.1.0"

  def self.unwrap(maybe : Maybe, default)
    case maybe
    when Just
      maybe.value
    end
  end
end

abstract class Maybe(T)
  abstract def visit(f)
  abstract def put(x : T)

  def unwrap(default)
    value = nil
    self.visit(->(x : T) { value = x })
    value || default
  end

  def map(f)
    self.visit(->(x : T) { self.put f.call(x) })
    self
  end
end

class Just(T) < Maybe(T)
  property value : T

  def initialize(@value : T)
  end

  def visit(f)
    f.call(value)
  end

  def put(x)
    self.value = x
  end
end

class Nothing(T) < Maybe(T)
  def visit(_f)
  end

  def put(_f)
  end
end
