defmodule LYSE.EventServerTest do
  use ExUnit.Case

  alias LYSE.EventServer

  test "integration test, and what." do
    EventServer.start
    EventServer.subscribe(self())
    EventServer.add_event("name", "desc", 100)
    refute EventServer.listen(10)
    assert :ok == EventServer.cancel("name")
    EventServer.add_event("name2", "desc", 50)
    assert {:done, "name2", "desc"} ==  EventServer.listen(55)
    assert :ok == EventServer.terminate
  end
end
