defmodule FirstApp.BucketTest do
  use ExUnit.Case, async: true

  test "stores values by key" do
    {:ok, bucket} = FirstApp.Bucket.start_link
    assert FirstApp.Bucket.get(bucket, "milk") == nil

    FirstApp.Bucket.put(bucket, "milk", 3)
    assert FirstApp.Bucket.get(bucket, "milk") == 3
  end
end
