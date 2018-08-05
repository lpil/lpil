defmodule FirstApp.BucketTest do
  use ExUnit.Case, async: true

  # Allow FirstApp.Bucket to be referred to as Bucket
  alias FirstApp.Bucket

  setup do
    {:ok, bucket} = Bucket.start_link
    {:ok, bucket: bucket}
  end

  test "stores values by key", %{bucket: bucket} do
    assert Bucket.get(bucket, "milk") == nil

    Bucket.put(bucket, "milk", 3)
    assert Bucket.get(bucket, "milk") == 3
  end

  test "can delete by key", %{bucket: bucket} do
    Bucket.put(bucket, "milk", "cookies")
    assert Bucket.get(bucket, "milk") == "cookies"

    x = Bucket.delete(bucket, "milk")
    assert x == "cookies"

    assert Bucket.get(bucket, "milk") == nil
  end

  test "delete returns the deleted value", %{bucket: bucket} do
    Bucket.put(bucket, "milk", "cookies")
    assert Bucket.get(bucket, "milk") == "cookies"

    x = Bucket.delete(bucket, "milk")
    assert x == "cookies"

    assert Bucket.get(bucket, "milk") == nil
  end
end
