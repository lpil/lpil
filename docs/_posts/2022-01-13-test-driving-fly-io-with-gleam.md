---
title: Test driving fly.io with Gleam
hidden: true
tags:
  - Gleam
  - deployment
---

So I've been gearing up to write some guides on how to deploy [Gleam][gleam] web
applications, but I wasn't sure what platform I should use.

Big clouds like AWS and GCP are powerful but are also intimidating, and they
provide many exciting new ways to blow your feet off and empty your bank account
if you make a mistake.

I'm rather fond of more straightforward virtual server providers such as
DigitalOcean and Linode, but setting up a deployment with just a Linux box and
an SSH connection is a bit involved. I suspect most people don't enjoy wrangling
hacky little deployment scripts as much as I do.

This leaves us with the various different platform as a service offerings which
promise to automated the entire deployment process. In the past I've used
Heroku, Render, DigitalOcean Apps, and a few others that no longer exist, but
none of them really stand out to me as the obvious choice. I need to try some
more.

Rather serendipitously [fly.io][fly.io] (a nifty looking platform as a service)
has just started sponsoring Gleam, so that seems like a good place to start. In
this blog I'll take the tiny Gleam [echo server][echo] example project and see
how quickly I can get it running on fly.io. As I write this I've had a quick
scan over the website, but otherwise I'm completely new to the platform.

Note- While I picked fly.io first because they started sponsoring Gleam, they're
not paying me to write this and I'm going to be honest with my experience.
Although if it's really bad I'll probably just quietly delete this post rather
than publishing it... But then you are reading this in the future after it has
been published, so I guess it went ok!

Alright, let's go.

## Getting started

The homepage has a big "Get Started" button that takes me to a page showing me
how to install their CLI and register with it. I'm using a mac computer here,
but they've similar looking commands for Windows and Linux too.

```shell
brew install superfly/tap/flyctl
flyctl auth signup # This opens a browser tab...
```

I opt for GitHub single sign on (rather than username/password), it
authenticates the CLI, and the browser tab takes me to a documentation page with
links a few different first-steps options. One is "introduction", which offers
more background on the platform. One is "the speedrun", for folks who are
familiar with docker etc and wish to get going quickly, and one is a more "hands
on" piece of step by step learning.

This choose-your-own adventure style onboarding seems rather clever, they don't
have to try and write one tutorial that works for everybody. I might have to
keep this in mind when working on Gleam's onboarding.

Right. I want to be a speedrunner. let's see how that goes.

## *click*

Whoa, they were not joking when they said get going fast. This is a page with 2
lines of text, a list of 6 steps, and a rather lovely picture of a hot air
balloon. There's not even a scroll-bar.

I've already done steps 1 and 2 (see above), and step 6 is "your app is now running", so really it's just 3 steps.

```shell
flyctl launch
flyctl info
flyctl open
```

Apparently if I'm familiar with docker this'll be enough. I'm a little
skeptical. Let's see how I can mess this up.

I'll start by adding a Dockerfile

I run `flyctl launch` and up pops a little multiple-choice quiz. It's quite
slick for a terminal program. It also prints this:

> Could not find a Dockerfile, nor detect a runtime or framework from source
> code. Continuing with a blank app.

Gleam's too new to have built-in support, so I better `ctrl-c` this quiz and add
a Dockerfile.

```dockerfile
FROM ghcr.io/gleam-lang/gleam:v0.19.0-erlang-alpine

# Create a group and user to run as
RUN addgroup -S echogroup && adduser -S echouser -G echogroup
USER echouser

# Add app code
WORKDIR /app/
COPY . ./

# Compile the app and set the entrypoint
RUN gleam build
CMD ["gleam", "run"]
```

That'll do. It's only a tiny app so no need to worry about caching or other
fancier docker features.

On with the `flyctl launch` quiz.

1. Application name. "gleam-echo" will do.
2. Region. I pick London.

> **ERROR** We need your payment information to continue! 

That's a reasonable requirement, but I feel that could have been mentioned
earlier, and not with a sudden bright red error message. 

I click the link in the error message, put my card details in the Stripe form
that opens in a browser window, and try `flyctl launch` again.

1. Application name. "gleam-echo".
2. Region. London.
3. Would you like to deploy now? You bet I do.

After a few seconds waiting it connects to a server somewhere and starts
building the OCI image using the Dockerfile. I'm impressed! I was sure it was
going to build locally and crash as it's an amd64 image and I'm using an ARM
computer.

It pushes the image to their image registry, deploys the app instance, and waits
for health checks to pass. It seems to be hanging forever...

I guess the command `flyctl logs` and it works, I get the app logs, and I can
see the application has started. That's good, it's not crashing, that makes me
think it's a configuration issue.

I open the documentation site and spot a page in the sidebar called "Run a Go
App". Go is easy to read, so hopefully I can spot anything I'm lacking there. Near the top of the page they have the source code for the tiny Go app.

```go
package main

import (
    "embed"
    "html/template"
    "log"
    "net/http"
    "os"
)

//go:embed templates/*
var resources embed.FS

var t = template.Must(template.ParseFS(resources, "templates/*"))

func main() {
    port := os.Getenv("PORT")
    if port == "" {
        port = "8080"
    }

    http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
        data := map[string]string{
            "Region": os.Getenv("FLY_REGION"),
        }

        t.ExecuteTemplate(w, "index.html.tmpl", data)
    })

    log.Println("listening on", port)
    log.Fatal(http.ListenAndServe(":"+port, nil))
}
```

Can you see what I see?

```go
  port := os.Getenv("PORT")
  if port == "" {
      port = "8080"
  }
```

My Gleam app doesn't read the `PORT` environment variable. I better add that.

```javascript
let port =
  os.get_env("PORT")
  |> result.then(int.parse)
  |> result.unwrap(3000)
```

Nope, no dice. It still hangs, and in the logs the app prints that it is using
the same port 3000 as before, so that variable must not have been set.

I open the `fly.toml` file which has been written into the project by the
`flyctl launch` command. It contains about 40 lines of configuration, including this:

```toml
[[services]]
  http_checks = []
  internal_port = 8080 # <-- Aha!
  processes = ["app"]
  protocol = "tcp"
  script_checks = []
```

This time it works! 52 seconds from start to deployment complete, including the
time it took to build and upload the the OCI image. That's a lot faster than my
experiments with Heroku and DigitalOcean Apps.

I run steps 5 and 6 from the speedrun guide.

`flyctl info` prints some basic information about the deployment. Release
number, hostname, ports exposed, that sort of thing. Potentially useful but
nothing overly exciting.

Running `flyctl open` opens a new browser tab with the app running. Pretty cool!
And it feels very snappy and has TLS too on the hostname `gleam-echo.fly.dev`.

Lastly let's teardown the application. In the output of `flyctl --help` I spot a
`destroy` command, and running `flyctl destroy gleam-echo` deletes the
deployment (after a quick "are you sure?" check).

## First impressions

I'm impressed! There's clearly some bits that are not as slick as they could
be, but the level of polish otherwise gives me confidence that any rough patches
will be worked out over time.

Even with the port problem it didn't take me long to get it live, and I've
certainly been more stressed trying to get an application running on Heroku
using docker.

If I were to suggest some improvements they would be:

1. Mention at the start of the speedrun guide that you need to have a dockerfile
   or to be using one of the supported languages.
2. Ask for the credit card information earlier in the process rather than when
   you think the application is about to deploy, and don't ask for it with big
   red ERROR text.
3. Ask what port the application is listening on during the `flyctl launch` quiz.

I think I'm going to have to test out fly.io a lot more ✨

## Postscript: hitting a bug

For fun I tried the same thing again on a Linux box, but ran into a
show-stopping bug where `flyctl launch` couldn't connect to the build server. I
think this is because the generated hostname of this Linux box I'm using has an
underscore in it. I've reported this to fly.io, we'll see how quickly it gets
resolved.

[gleam]: https://gleam.run/
[fly.io]: https://fly.io/
[echo]: https://github.com/example-echo-server
