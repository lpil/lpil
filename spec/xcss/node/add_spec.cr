require "../../spec_helper"
require "../../../src/xcss/node/number"
require "../../../src/xcss/node/add"

describe Xcss::Node::Add do
  describe "#reducible?" do
    it "is reducible" do
      Xcss::Node::Add.new(
        Xcss::Node::Number.new(100.0),
        Xcss::Node::Number.new(100.0),
      ).reducible?.should eq(true)
    end
  end

  describe "#==" do
    it "can be equal" do
      x = Xcss::Node::Add.new(
        Xcss::Node::Number.new(100.0),
        Xcss::Node::Number.new(100.0),
      )
      y = Xcss::Node::Add.new(
        Xcss::Node::Number.new(100.0),
        Xcss::Node::Number.new(100.0),
      )
      x.should eq(y)
    end

    it "can be different" do
      x = Xcss::Node::Add.new(
        Xcss::Node::Number.new(100.0),
        Xcss::Node::Number.new(100.0),
      )
      y = Xcss::Node::Add.new(
        Xcss::Node::Number.new(0.0),
        Xcss::Node::Number.new(0.0),
      )
      x.should_not eq(y)
    end

    it "can be classes" do
      x = Xcss::Node::Add.new(
        Xcss::Node::Number.new(100.0),
        Xcss::Node::Number.new(100.0),
      )
      y = Xcss::Node::Number.new(0.0)
      x.should_not eq(y)
    end
  end

  describe "#reduce" do
    it "sums when both sides are Numbers" do
      e = Xcss::Environment.new
      res = Xcss::Node::Add.new(
        Xcss::Node::Number.new(100.0),
        Xcss::Node::Number.new(100.0),
      ).reduce(e)
      res.should eq(Xcss::Node::Number.new(200.0))
    end

    it "reduces lhs" do
      e = Xcss::Environment.new
      res = Xcss::Node::Add.new(
        Xcss::Node::Add.new(
          Xcss::Node::Number.new(100.0),
          Xcss::Node::Number.new(100.0),
        ),
        Xcss::Node::Number.new(100.0),
      ).reduce(e)
      res.should eq(
        Xcss::Node::Add.new(
          Xcss::Node::Number.new(200.0),
          Xcss::Node::Number.new(100.0),
        )
      )
    end
  end

  describe "#to_source" do
    it "looks like a function call :)" do
      Xcss::Node::Add.new(
        Xcss::Node::Number.new(1.0),
        Xcss::Node::Number.new(2.0),
      ).to_source.should eq("add(1.0, 2.0)")
    end
  end
end
