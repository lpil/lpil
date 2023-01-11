if Mix.env() == :dev do
  defmodule GleamBuilder do
    def start_link(_args \\ nil) do
      GenServer.start_link(__MODULE__, nil)
    end

    def init(_args) do
      # Watch for changes to Gleam files
      {:ok, pid} = FileSystem.start_link(dirs: ["assets/app/src", "assets/app/test"])
      FileSystem.subscribe(pid)
      run_gleam_compiler()
      {:ok, nil}
    end

    def handle_info({:file_event, _watcher, _event}, state) do
      run_gleam_compiler()
      {:noreply, state}
    end

    def run_gleam_compiler() do
      System.cmd("gleam", ["build"],
        cd: "assets/app",
        into: IO.stream(:stdio, :line),
        stderr_to_stdout: true
      )
    end
  end
end
