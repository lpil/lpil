require "./spec_helper"

describe Sum do
  # TODO: Write tests

  it "unwrap" do
    Just.new(1).unwrap(2).should eq(1)
    Nothing(Int32).new.unwrap(2).should eq(2)
  end

  it "map" do
    Just.new(1).map(->(x : Int32) { x + 1 }).unwrap(0).should eq(2)
    Nothing(Int32).new.map(->(x : Int32) { x + 1 }).unwrap(0).should eq(0)
  end
end
