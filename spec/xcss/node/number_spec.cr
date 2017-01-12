require "../../spec_helper"
require "../../../src/xcss/node/number"
require "../../../src/xcss/node/string"

describe Xcss::Node::Number do
  describe "#reducible?" do
    it "is not reducible" do
      Xcss::Node::Number.new(100.0).reducible?.should eq(false)
    end
  end

  describe "#reduce" do
    it "no-ops" do
      e = Xcss::Environment.new
      n = Xcss::Node::Number.new(100.0)
      n.reduce(e).should be(n)
    end
  end

  describe "#==" do
    it "can be equal" do
      Xcss::Node::Number.new(2.0).should eq(Xcss::Node::Number.new(2.0))
    end

    it "can be different" do
      Xcss::Node::Number.new(2.0).should_not eq(Xcss::Node::Number.new(3.0))
    end

    it "can be different to another type" do
      Xcss::Node::Number.new(2.0).should_not eq(Xcss::Node::String.new("hi"))
    end
  end

  describe "#to_source" do
    it "is a number!" do
      Xcss::Node::Number.new(2.0).to_source.should eq("2.0")
    end
  end
end
