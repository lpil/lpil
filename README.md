# Frankencat!

## Quick Reference

An instance of the Neo4j database is expected to be running on `localhost`.

```sh
# Install deps
mix deps.get
yarn install

# Run the tests
mix test
mix test.watch

# Run the app
mix phx.server

# Build a docker image
docker build . -t fcat

# Run docker container from image
docker run --env ERLANG_COOKIE=some-secret-value -p 4000:4000 -it --rm fcat
```
