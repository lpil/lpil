# gh_counts

A Gleam project that counts the number of Gleam files, repos, and users on
GitHub in a hacky fashion using the GitHub search API.

How accurate it is isn't clear as the search API seems non-deterministic, but
it'll do as a rough estimate.

## Quick start

```sh
# Run the eunit tests
rebar3 eunit

# Run the Erlang REPL
rebar3 shell

# Build and run the escript
rebar3 escriptize
_build/default/bin/gh_counts $GH_TOKEN
```
