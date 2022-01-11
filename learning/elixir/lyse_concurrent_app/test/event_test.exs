defmodule LYSE.EventTest do
  use ExUnit.Case, async: true

  alias LYSE.Event

  test "Events message back after the time has passed" do
    pid = Event.start("Alarm", 100)
    refute_receive _anything, 50
    assert_receive {:done, "Alarm"}, 60
    refute Process.alive?(pid)
  end

  test "Events can be cancelled" do
    pid = Event.start("Lunch", 100)
    :ok = Event.cancel(pid)
    refute Process.alive?(pid)
    refute_receive _anything, 100
  end

  test "Cancelling OK if event already dead" do
    pid = Event.start("Lunch", 100)
    Process.exit(pid, :kill)
    refute Process.alive?(pid)
    :ok = Event.cancel(pid)
    refute Process.alive?(pid)
    refute_receive _anything, 100
  end
end
