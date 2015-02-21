defmodule FirstApp.RegistryTest do
  use ExUnit.Case, async: true

  setup do
    {:ok, registry} = FirstApp.Registry.start_link
    {:ok, registry: registry}
  end

  test "returns :error when a bucket is not found", %{registry: registry} do
    assert FirstApp.Registry.lookup(registry, "shopping") == :error
  end

  test "spawns buckets", %{registry: registry} do
    assert FirstApp.Registry.lookup(registry, "shopping") == :error

  end
end
