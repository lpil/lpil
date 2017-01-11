require "../spec_helper"

describe Xcss::Token do
  it "can be initialised with a type" do
    token = Xcss::Token.new(:EOF)
    token.type.should eq(:EOF)
    token.value.should eq("")
  end

  it "can also take a value" do
    token = Xcss::Token.new(:identifier, "hello")
    token.type.should eq(:identifier)
    token.value.should eq("hello")
  end

  describe "#==" do
    it "can be equal" do
      token1 = Xcss::Token.new(:identifier, "hello")
      token2 = Xcss::Token.new(:identifier, "hello")
      token1.should eq(token2)
    end

    it "can be unequal" do
      token1 = Xcss::Token.new(:identifier, "hello")
      token2 = Xcss::Token.new(:number, "123")
      token1.should_not eq(token2)
    end
  end
end
