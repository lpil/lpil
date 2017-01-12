class Xcss::Environment
  include IO

  getter io : IO
  getter vars : Hash(String, Xcss::Node)

  def initialize(io = STDOUT, vars = {} of String => Xcss::Node)
    @io = io
    @vars = vars
  end

  def read(bytes)
    @io.read(bytes)
  end

  def write(bytes)
    @io.read(bytes)
  end
end

class Xcss::Machine

  def initialize(expression : Xcss::Node, environment = Environment.new)
    @expression = expression
    @environment = environment
  end

  def step!
    @expression = @expression.reduce(@environment)
  end

  def run!(debug = false)
    while @expression.reducible?
      puts @expression.to_source if debug
      step!
    end
    puts @expression.to_source if debug
    @expression
  end
end
