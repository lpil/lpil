# gh_counts

A Gleam project that counts the number of Gleam files, repos, and users on
GitHub in a hacky fashion using the GitHub search API.

The GitHub search API seems non-deterministic so we crawl this API multiple
times and unify the results from each crawl.. This means that this script
takes a long time to run, but it does now return consistent results.

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
