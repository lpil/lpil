require "../../spec_helper"
require "../../../src/xcss/node/number"
require "../../../src/xcss/node/string"

describe Xcss::Node::String do
  describe "#reducible?" do
    it "is not reducible" do
      Xcss::Node::String.new("hi").reducible?.should eq(false)
    end
  end

  describe "#reduce" do
    it "no-ops" do
      e = Xcss::Environment.new
      s = Xcss::Node::String.new("hi")
      s.reduce(e).should be(s)
    end
  end

  describe "#==" do
    it "can be equal" do
      Xcss::Node::String.new("a").should eq(Xcss::Node::String.new("a"))
    end

    it "can be different" do
      Xcss::Node::String.new("a").should_not eq(Xcss::Node::String.new("Z"))
    end

    it "can be different to another type" do
      Xcss::Node::String.new("").should_not eq(123)
      Xcss::Node::String.new("").should_not eq("string")
    end
  end

  describe "#to_source" do
    it "shows the string" do
      Xcss::Node::String.new(":)").to_source.should eq(%[":)"])
    end
  end
end
