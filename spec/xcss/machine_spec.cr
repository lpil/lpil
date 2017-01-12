require "../spec_helper"
require "../../src/xcss/machine"

describe Xcss::Machine do
  describe "#run!" do
    it "is alive!" do
      env = Xcss::Environment.new(IO::Memory.new)
      expr = Xcss::Node::Add.new(
        Xcss::Node::Number.new(1.0),
        Xcss::Node::Number.new(2.0),
      )
      machine = Xcss::Machine.new(expr, env)
      machine.run!(debug: true)
    end
  end
end
