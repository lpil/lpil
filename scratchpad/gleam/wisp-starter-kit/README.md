# app

This is not finished yet!

[![Package Version](https://img.shields.io/hexpm/v/app)](https://hex.pm/packages/app)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/app/)

### Goals

- [x] Wisp app
  - [x] Routing
  - [ ] Default error pages
  - [x] Logging
  - [ ] Have a default stylesheet that gets loaded
  - [ ] Have a default javascript that gets loaded
  - [ ] Tests for all this jazz!
- [x] HTML!
  - [ ] Default layout
  - [ ] Some pretty welcome page
  - [ ] Form helpers
  - [ ] Show errors in forms
  - [ ] Oh yes, tests too!
  - [ ] Make the default error pages look pretty
- [ ] PostgreSQL database
  - [ ] Migrations
- [ ] Authentication
  - [ ] Register
  - [ ] Verify email
  - [ ] Log in
  - [ ] Log out
  - [ ] Gosh we really need a lot of tests for this
- [ ] Email sending
  - [ ] Emails go _somewhere_ viewable in development
- [ ] Reading config from environment

### Stretch goals

- [ ] Admin UI
  - [ ] View database records
  - [ ] Search/filter database records
  - [ ] Delete database records
  - [ ] Update database records

```sh
gleam add app
```
```gleam
import app

pub fn main() {
  // TODO: An example of the project in use
}
```

Further documentation can be found at <https://hexdocs.pm/app>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
gleam shell # Run an Erlang shell
```
