defmodule FirstApp.RegistryTest do
  use ExUnit.Case, async: true
  alias FirstApp.Registry

  defmodule Forwarder do
    use GenEvent

    def handle_event(event, parent) do
      send parent, event
      {:ok, parent}
    end
  end

  setup do
    {:ok, manager}  = GenEvent.start_link
    {:ok, registry} = Registry.start_link(manager)
    GenEvent.add_mon_handler(manager, Forwarder, self())
    {:ok, registry: registry}
  end

  test "returns :error when a bucket is not found", %{registry: registry} do
    assert Registry.lookup(registry, "shopping") == :error
  end

  test "spawns buckets", %{registry: registry} do
    assert Registry.lookup(registry, "shopping") == :error
    Registry.create(registry, "shopping")
    assert {:ok, _bucket} = Registry.lookup(registry, "shopping")
  end

  test "Can stop", %{registry: registry} do
    # This is a crappy test.
    assert :ok == Registry.stop registry
  end

  test "removes buckets when they exit", %{registry: registry} do
    Registry.create(registry, "shopping")
    {:ok, bucket} = Registry.lookup(registry, "shopping")

    Agent.stop(bucket)
    assert Registry.lookup(registry, "shopping") == :error
  end

  test "sends events on create", %{registry: registry} do
    Registry.create(registry, "shopping")
    {:ok, bucket} = Registry.lookup(registry, "shopping")
    assert_receive {:create, "shopping", ^bucket}
  end

  test "sends events on crash", %{registry: registry} do
    Registry.create(registry, "shopping")
    {:ok, bucket} = Registry.lookup(registry, "shopping")
    Agent.stop(bucket)
    assert_receive {:exit, "shopping", ^bucket}
  end
end
