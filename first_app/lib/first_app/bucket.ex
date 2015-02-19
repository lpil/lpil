defmodule FirstApp.Bucket do
  @doc """
  Starts a new bucket.
  """
  def start_link do
    Agent.start_link fn -> HashDict.new end
  end

  @doc """
  Add a value to the bucket
  """
  def put(bucket, key, value) do
    # See how we turn the HashDict bit into a fn with &
    Agent.update(bucket, &HashDict.put(&1, key, value))
  end

  @doc """
  Get a value from the bucket
  """
  def get(bucket, key) do
    Agent.get(bucket, &HashDict.get(&1, key))
  end
end
