FROM bitwalker/alpine-elixir:1.6.0 as release-builder

ENV HOME=/opt/app
ENV MIX_ENV=prod

# ARG ERLANG_COOKIE
# ENV ERLANG_COOKIE $ERLANG_COOKIE

# Install Hex + Rebar
RUN mix do local.hex --force, local.rebar --force

# Cache elixir deps
COPY config/ $HOME/config/
COPY mix.exs mix.lock $HOME/

COPY apps/web/mix.exs $HOME/apps/web/
COPY apps/web/config/ $HOME/apps/web/config/

COPY apps/core/mix.exs $HOME/apps/core/
COPY apps/core/config/ $HOME/apps/core/config/

RUN mix do deps.get --only $MIX_ENV, deps.compile

# Release
COPY . $HOME/
WORKDIR $HOME
RUN mix release --env=$MIX_ENV --verbose

########################################################################

FROM alpine:3.6

ENV LANG=en_US.UTF-8 \
    HOME=/opt/app/ \
    TERM=xterm

ENV APP_VERSION=0.1.0

RUN apk add --no-cache ncurses-libs openssl bash

EXPOSE 4000
ENV PORT=4000 \
    MIX_ENV=prod \
    REPLACE_OS_VARS=true \
    SHELL=/bin/bash

COPY --from=release-builder \
  $HOME/_build/prod/rel/baraxx/releases/$APP_VERSION/baraxx.tar.gz \
  $HOME

WORKDIR $HOME
RUN tar -xzf baraxx.tar.gz

ENTRYPOINT ["/opt/app/bin/baraxx"]
CMD ["help"]
