# Particle

[![CircleCI](https://circleci.com/gh/lpil/particle.svg?style=shield&circle-token=c54e5a3e5e0bd68e4bd37e0de6be4d3d85ba7f95)](https://circleci.com/gh/lpil/particle)

## Quick Reference

An instance of the Neo4j database is expected to be running on `localhost`.

```sh
# Install deps
mix deps.get
yarn install

# Run the database (if you don't already have it)
docker run -e ORIENTDB_ROOT_PASSWORD=orientdb -t -d -p 2424:2424 -p 2480:2480 --name orientdb orientdb
# Set up the database
make db-setup

# Run the tests
mix test
mix test.watch

# Run the app
mix phx.server

# Build a docker image
docker build . -t particle

# Run docker container from image
docker run --env ERLANG_COOKIE=some-secret-value -p 4000:4000 -it --rm particle
```
