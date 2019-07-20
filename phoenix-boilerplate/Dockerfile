FROM bitwalker/alpine-elixir-phoenix:1.6.5 as release-builder

ENV MIX_ENV=prod \
  REPLACE_OS_VARS=true \
  HOME=/app/
WORKDIR $HOME

RUN apk add --update alpine-sdk coreutils curl curl-dev

# Elixir deps
COPY package.json ./
COPY yarn.lock ./
COPY mix.exs mix.lock ./
COPY config/ config/
RUN mix do local.hex --force, local.rebar --force \
  && mix do deps.get --only $MIX_ENV, deps.compile \
  && npm -g install yarn \
  && yarn install

COPY . .

# Compile everything
RUN yarn run build \
 && mix do compile, phx.digest \
 && mix release --env=$MIX_ENV --verbose

########################################################################

FROM alpine:3.6
LABEL maintainer="TODO"

EXPOSE 4000 \
  PORT=4000 \
  MIX_ENV=prod \
  REPLACE_OS_VARS=true \
  SHELL=/bin/bash \
  LANG=en_US.UTF-8 \
  HOME=/app/ \
  TERM=xterm \
  APP_VERSION=0.1.0
WORKDIR $HOME

COPY --from=release-builder \
  $HOME/_build/prod/rel/particle/releases/$APP_VERSION/particle.tar.gz \
  $HOME

RUN apk add --no-cache ncurses-libs openssl bash \
  && tar -xzf particle.tar.gz \
  && adduser -S www-particle \
  && mkdir -p /app \
  && chown www-particle /app
USER www-particle

ENTRYPOINT ["/app/bin/particle"]
# CMD ["foreground"]
