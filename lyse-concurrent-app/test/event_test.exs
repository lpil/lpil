defmodule LYSE.EventTest do
  use ExUnit.Case, async: true

  alias LYSE.Event

  test "Events message back after the time has passed" do
    state = %Event{ server: self(), name: "Alarm", ms: 100 }
    pid   = spawn(Event, :loop, [state])
    refute_receive _anything, 50
    assert_receive {:done, "Alarm"}, 60
    refute Process.alive?(pid)
  end

  test "Events can be cancelled" do
    state = %Event{ server: self(), name: "Alarm", ms: 100 }
    pid   = spawn Event, :loop, [state]
    ref   = make_ref()
    send pid, {self(), ref, :cancel}
    assert_receive {^ref, :ok}, 10
    refute Process.alive?(pid)
    refute_receive _anything, 100
  end
end
