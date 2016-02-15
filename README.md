Fawkes
======

Dumbledore's pet phoenix.

Also a blog web app, written with Phoenix.


## Setup

You'll need Elixir, Node, and Postgresql installed.

```sh
# Install Elixir deps
mix deps.get
# Install Javascript deps
npm install

# Start the database
pg_ctl start

# Create and migrate database
mix ecto.create
mix ecto.migrate
mix ecto.seed
MIX_ENV=test mix ecto.create
MIX_ENV=test mix ecto.migrate

# Run the tests
mix test.watch

# Run the server
mix phoenix.server
```

You may need to ensure the `postgres` database user has the password
`postgres`, and that they have the required permissions.

```sql
CREATE ROLE postgres LOGIN;
```


## What are we using?

- Elixir
- Phoenix web framework
- Dogma Elixir linter
- ES6 Javascript
- Webpack frontend build tool
- ESLint Javascript linter
