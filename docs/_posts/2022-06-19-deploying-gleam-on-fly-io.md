---
title: Deploying Gleam on Fly.io
tags:
  - Gleam
  - deployment
---

The recently released Gleam v0.22 includes a new command to prepare a Gleam
application for deployment. With this in mind I thought it would be a good time
to show how deploy Gleam into production with my new favourite platform,
[Fly.io](https://fly.io).

Full disclosure, Fly.io are [sponsoring my work on Gleam](https://github.com/sponsors/lpil).
I hadn't used their platform prior to their sponsoring, but now that I have I
believe they the user experience of any deployment platform today, beating even
my old favourite of Heroku.

I've already got Gleam, Erlang, and the Fly.io command line installed, logged in
to Fly.io, and added a payment card to my Fly.io organisation. If you're
following along at home you'll likely want to do these too.

Right. Let's get started.

## A Gleam web app

I'm going to need an application to deploy, so I'm going to quickly make a super
basic one. First I create a new project and add the required dependencies.

```sh
gleam new pidgey
cd pidgey
gleam add mist gleam_http gleam_erlang
```

`mist` is a web server, and `gleam_http` and `gleam_erlang` provide helper types
and functions for working with HTTP and the Erlang runtime respectively.

Next I'll open up `src/pidgey.gleam` and write a tiny web application that
always returns the body "[Hello, Joe!](https://www.youtube.com/watch?v=uKfKtXYLG78)".

```rust
import mist
import gleam/io
import gleam/erlang
import gleam/bit_builder
import gleam/http/response.{Response}

pub fn main() {
  // Start the web server
  assert Ok(_) = mist.run_service(8080, web_service)

  // Put the main thread to sleep while the server works
  erlang.sleep_forever()
}

fn web_service(_request) {
  let body = bit_builder.from_string("Hello, Joe!")
  Response(200, [], body)
}
```

Now I can run it with `gleam run`, and test it out in another terminal with curl.

```sh
curl localhost:8080
# Hello, Joe!
```

Neat :)

## Preparing to deploy

Fly.io has built in support for some languages, but Gleam isn't one of them, so
I'll have to use their docker support instead.

Within the Gleam project I create a file named `Dockerfile` with these contents.

```dockerfile
FROM ghcr.io/gleam-lang/gleam:v0.22.0-erlang-alpine

# Add project code
COPY . /build/

# Compile the project
RUN cd /build \
  && gleam export erlang-shipment \
  && mv build/erlang-shipment /app \
  && rm -r /build

# Run the server
WORKDIR /app
ENTRYPOINT ["/app/entrypoint.sh"]
CMD ["run"]
```

If we were deploying a very large application we might want to add some caching,
and we should probably create a new Linux user for the application to run as,
but this is good enough for now. There's lots of resources online for docker
best practices if you'd like to know more, and I'll probably make a more
detailed example for Gleam in future.

## Ship it!

Now to host the web app on Fly.io by running `flyctl launch`.

```sh
flyctl launch
# Creating app in /tmp/pidgey
# Scanning source code
# Detected a Dockerfile app
# ? App Name (leave blank to use an auto-generated name): pidgey
# ? Select organization: Louis Pilfold (personal)
# ? Select region: lhr (London, United Kingdom)
# Created app pidgey in organization personal
# Wrote config file fly.toml
# ? Would you like to setup a Postgresql database now? No
# ? Would you like to deploy now? Yes
# Deploying pidgey
```

The command asks a few questions:
- What should the app be named?
- What organisation should it belong to?
- Which region should it be deployed to?
- Should a PostgreSQL database also be deployed?
- Should the app be deployed now?

After this it builds the application using the docker file, and within a minute
the application is live! To open it in a web browser I run `flyctl open`.

To deploy future versions of the application I can run `flyctl deploy` after
saving my changes.

And that's it. I'm really impressed with [Fly.io](https://fly.io) and I expect
I'll be using them to host all my Gleam web applications going forward. Give
them a try, I think you'll like their platform.

Happy hacking!
