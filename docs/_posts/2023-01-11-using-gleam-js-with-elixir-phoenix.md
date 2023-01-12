---
title: Using Gleam JS with Elixir's Phoenix
tags:
  - Gleam
  - Elixir
  - How to
---

I was talking to [Josh](https://github.com/joshprice) over at
[Alembic](https://alembic.com.au/) and he mentioned that he was interested in
taking advantage of Gleam's compile-to-JavaScript capability and using it to
make a front end for an Elixir Phoenix app. What a great idea!

I'm going to show you how to set these two up together, so you can try it too. :)

I'll assume you already have Elixir and Gleam installed. If you've not check out
the documentation for [Elixir](https://elixir-lang.org/install.html) and
[Gleam](https://gleam.run/getting-started/installing/) respectively. I'm using
Elixir 1.14.2, Gleam 0.25.3, and Phoenix 1.6.15. If you have issues with any
later versions let me know and I'll update this post.

## Hello, Joe!

First off we're going to need a Phoenix application. You can use an existing one
if you have one, but I'm going to create a new one.

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
the JavaScript bundle it won't get run.

Open up the JavaScript file `assets/js/app.js` and at the bottom of the file
import the compiled Gleam `app` module, calling its `main` function.

```js
import { main } from "../app/build/dev/javascript/app/app.mjs";
main();
```

Now run the Phoenix server with `mix phx.server` and open up
`http://localhost:4000` in the browser. In the developer console there is the
message "Hello from the Gleam frontend!". Success! 💃

## Automatic recompilation

We've got Gleam code now running in the browser, but having to manually run the
compiler is annoying. What we really want is for the code to automatically be
recompiled when edits are saved. To do this we're going to take advantage of
Phoenix's development watchers feature.

By default Phoenix comes with one watcher that runs esbuild, a tool that bundles
front end assets together for use in the browser. We're going to add a second
one that runs the Gleam compiler.

We're going to need a way to detect when a Gleam file has changed, so add the
`file_system` package to your `mix.exs` file.

```elixir
  defp deps do
    [
      # ...other deps here...
      {:file_system, "~> 0.2", only: :dev}
    ]
  end
```

Add a new watcher for Gleam to `config/dev.exs`.

```elixir
config :my_app, MyAppWeb.Endpoint,
  # ...other config here...
  watchers: [
    # ...other watchers here...
    gleam: {GleamBuilder, :start_link, []} # <- Add this entry
  ]
```

With this configuration when the application starts in dev mode Phoenix will
call the `start_link` function on the `GleamBuilder` module, and that function
will be responsible for building the Gleam code while the application is
running.

Open up the `lib/gleam_builder.ex` file and enter the code below to define the
new watcher.

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

Wow there's a lot going on here!

The watcher is only used in `:dev` mode and uses development dependencies, so it
would emit warnings if it were compiled in `:prod` mode. To avoid this we wrap
the whole module definition in an `if` statement that checks the environment at
compile time.

The watcher is an Elixir `GenServer` which is started by the `start_link` function.

In the `init` function it starts a `FileSystem` process for the Gleam code
directories and then calls the `FileSystem.subscribe/1` function in order to get
notified with a message any time there are changes. It also runs the Gleam
compiler once in `init` to build the code initially.

The `handle_info` function is called for each message from the `FileSystem`
process. It runs the Gleam compiler again to rebuild the code with the
just-saved changes.

And that's it! If you start the Phoenix server again you'll see the latest
greeting get printed to the browser developer console each time you edit and
save the `assets/app/src/app.gleam` file. Or any other Gleam file for that
matter.

## What's next?

Now that the project is set up you probably want to start writing some Gleam
code!

You could import and use some JavaScript DOM functions using Gleam's
[external function feature](https://gleam.run/book/tour/external-functions.html),
or you could build a more sophisticated React based front end using
[react-gleam](https://github.com/brettkolodny/react-gleam) or
[Lustre](https://github.com/hayleigh-dot-dev/gleam-lustre/).

Whatever you do, have fun! Be sure to share anything cool that you make on the
[Gleam Discord server](https://discord.gg/Fm8Pwmy).

P.S. If you want to see all the changes I made to the Phoenix app check out
[this git commit](https://github.com/lpil/lpil/pull/30/commits/e1db86af9d7701cd6f02ff038f4c9daeb480261a).
