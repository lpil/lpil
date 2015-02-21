defmodule FirstApp.RegistryTest do
  use ExUnit.Case, async: true
  alias FirstApp.Registry

  setup do
    {:ok, registry} = Registry.start_link
    {:ok, registry: registry}
  end

  test "returns :error when a bucket is not found", %{registry: registry} do
    assert Registry.lookup(registry, "shopping") == :error
  end

  test "spawns buckets", %{registry: registry} do
    assert Registry.lookup(registry, "shopping") == :error
    Registry.create(registry, "shopping")
    assert {:ok, bucket} = Registry.lookup(registry, "shopping")
  end

  test "Can stop", %{registry: registry} do
    # This is a crappy test.
    assert :ok == Registry.stop registry
  end
end
