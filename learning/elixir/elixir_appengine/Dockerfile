FROM bitwalker/alpine-elixir:1.7.1

# Set exposed ports
EXPOSE 8080
ENV PORT=8080 \
    MIX_ENV=prod

# Add current Mix project
ADD . /app
WORKDIR /app

CMD mix run --no-halt
