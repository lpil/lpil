# Boilerplate

An example Elixir web application with the usual odds and ends, to be used as
the base for whatever project.

- Cowboy webserver
- Phoenix web framework
- Yesql & Ecto for persistence & validation
- Ueberauth for authentication

## Quick Reference

An instance of the Postgresql database is expected to be running on
`localhost` with user `postgres` and password `postgres`.

```sh
# Install deps
mix deps.get
cd assets; yarn install; cd -

# Set up the database
mix ecto.setup

# Run the tests
mix test
mix test.watch

# Run the app
mix phx.server

# Build a docker image
docker build . -t myapp

# Run docker container from image
docker run --env ERLANG_COOKIE=some-secret-value -p 4000:4000 -it --rm particle
```
