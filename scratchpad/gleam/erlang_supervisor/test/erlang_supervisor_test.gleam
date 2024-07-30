import erlang_supervisor
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Pid}
import gleeunit

pub fn main() {
  gleeunit.main()
}

@external(erlang, "gen_event", "start_link")
pub fn start_misc_process() -> Result(Pid, Dynamic)

pub fn start_one_for_one_test() {
  let assert Ok(pid) =
    erlang_supervisor.new(erlang_supervisor.OneForOne)
    |> erlang_supervisor.add(
      erlang_supervisor.worker_child("first", start_misc_process)
      |> erlang_supervisor.significant(False)
      |> erlang_supervisor.timeout(100),
    )
    |> erlang_supervisor.start_link

  let assert True = process.is_alive(pid)
  process.sleep(500)
  let assert True = process.is_alive(pid)
}

pub fn start_rest_for_one_test() {
  let assert Ok(pid) =
    erlang_supervisor.new(erlang_supervisor.RestForOne)
    |> erlang_supervisor.add(
      erlang_supervisor.worker_child("first", start_misc_process)
      |> erlang_supervisor.significant(False)
      |> erlang_supervisor.timeout(100),
    )
    |> erlang_supervisor.start_link

  let assert True = process.is_alive(pid)
  process.sleep(500)
  let assert True = process.is_alive(pid)
}

pub fn start_one_for_all_test() {
  let assert Ok(pid) =
    erlang_supervisor.new(erlang_supervisor.OneForAll)
    |> erlang_supervisor.add(
      erlang_supervisor.worker_child("first", start_misc_process)
      |> erlang_supervisor.significant(False)
      |> erlang_supervisor.timeout(100),
    )
    |> erlang_supervisor.start_link

  let assert True = process.is_alive(pid)
  process.sleep(500)
  let assert True = process.is_alive(pid)
}
