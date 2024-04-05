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
  - [ ] Migrations (https://github.com/amacneil/dbmate)
- [ ] Authentication
  - [ ] Register
  - [ ] Verify email
  - [ ] Log in
  - [ ] Log out
  - [ ] Gosh we really need a lot of tests for this
- [ ] Email sending
  - [ ] Emails go _somewhere_ viewable in development
- [x] Reading config from environment

### Stretch goals

- [ ] Admin UI
  - [ ] View database records
  - [ ] Search/filter database records
  - [ ] Delete database records
  - [ ] Update database records


## Getting started

You will need PostgreSQL installed and running on `localhost`. You can set the
standard `PGPASSWORD` and `PGUSER` environment variables if your user requires
credentials.

```sh
# Create a .env file with your configuration
cp .env.example .env
vim .env

# Create the databases
createdb wisp_starter_kit
createdb wisp_starter_kit_test
```
