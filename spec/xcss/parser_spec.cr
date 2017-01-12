require "../spec_helper"

describe Xcss::Parser do
  describe "#parse" do
    it "empty stylesheet" do
      output = Xcss::Parser.new("").parse
      expected = Xcss::Stylesheet.new
      output.should eq(expected)
    end

    it "stylesheet with misc top level literals" do
      output = Xcss::Parser.new("1 2 3").parse
      expected = Xcss::Stylesheet.new
      output.should eq(expected)
    end
  end
end
