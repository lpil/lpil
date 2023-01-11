---
title: Using Gleam JS with Elixir's Phoenix
tags:
  - Gleam
  - Elixir
  - How to
---

I was talking to [Josh](https://github.com/joshprice) over at
[Alembic](https://alembic.com.au/) and he mentioned that he was interested in
taking advantage of Gleam's compile-to-JavaScript capability and using it as the
front end for an Elixir Phoenix app. What a great idea!

I'm going to show you how to set these two up together, so you can try it too. :)

I'll assume you already have Elixir and Gleam installed. If you've not check out
the documentation for [Elixir](https://elixir-lang.org/install.html) and
[Gleam](https://gleam.run/getting-started/installing/) respectively. I'm using
Elixir 1.14.2, Gleam 0.25.3, and Phoenix 1.6.15. If you have issues with any
later versions let me know and I'll update this post.

## Hello, Joe!

First off we're going to need a Phoenix application. You can use an existing one
if you have one, but I don't, so I'm going to create a new one.

```sh
# Create a new Phoenix app named `my_app`
mix archive.install hex phx_new
mix phx.new my_app

# Enter the project and setup the database
cd my_app
mix ecto.create
```

Next up we're going to need a Gleam project too. In Phoenix projects the front
end lives in the `./assets` directory, so we'll create it in there.

```sh
# Create a new Gleam project named `app`
gleam new assets/app
```

By default Gleam runs on the Erlang virtual machine, so we'll need to tell it to
compile to JavaScript instead.

Open up `assets/app/gleam.toml` and add `target = "javascript"`.

```toml
name = "app"
version = "0.1.0"
target = "javascript" # <- Add this line

[dependencies]
gleam_stdlib = "~> 0.25"

[dev-dependencies]
gleeunit = "~> 0.7"
```

The entrypoint to the Gleam project is the `main` function in
`assets/app/src/app.gleam`. It prints a greeting, which in this case will go to
the browser's developer console.

I've edited the greeting here to make it extra clear that the message is coming
from the Gleam frontend.

```rust
import gleam/io

pub fn main() {
  io.println("Hello from the Gleam frontend!")
}
```

In a terminal window, run the Gleam build tool to compile Gleam code.

```sh
cd assets/app
gleam build
cd ../..
```

The Gleam code has been compiled to JavaScript, but unless it is included into
the JavaScript bundle it won't do anything.

Open up the JavaScript file `assets/js/app.js` and at the bottom of the file
import the compiled Gleam `app` module, calling its `main` function.

```js
import { main } from "../app/build/dev/javascript/app/app.mjs";
main();
```

Now run the Phoenix server with `mix phx.server` and open up
`http://localhost:4000` in the browser. Open up the developer console and you
should see the message "Hello from the Gleam frontend!". Success! 💃

## Automatic recompilation

Check the console, it prints "Hello from app!"

Add file system watcher dep

```elixir
{:file_system, "~> 0.2", only: :dev}
```

Add a new watcher for Gleam to `config/dev.exs`

```elixir
    # Compile Gleam files on change
    gleam: {GleamBuilder, :start_link, []}
```

Create define the watcher

```elixir
# This watcher uses dev deps so let's only define it in dev to
# avoid warnings during compilation in production.
if Mix.env() == :dev do

  defmodule GleamBuilder do
    @gleam_dirs ["assets/app/src", "assets/app/test"]

    def start_link(_args \\ nil) do
      GenServer.start_link(__MODULE__, nil)
    end

    def init(_args) do
      # Watch for changes to Gleam files
      {:ok, pid} = FileSystem.start_link(dirs: @gleam_dirs)
      FileSystem.subscribe(pid)

      # Run the compiler once for the initial code
      run_gleam_compiler()
      {:ok, nil}
    end

    def handle_info({:file_event, _watcher, _event}, state) do
      # A Gleam file has changed, run the compiler
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

```
