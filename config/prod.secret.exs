use Mix.Config

# Values with the ${NAME} syntax will be replaced with environment
# by Distillery, the release bundler.

config :boilerplate, BoilerplateWeb.Endpoint,
  # Secret key.
  # e.g. DIolo3iSU44aMkda9O3YvaQe7k3V0kkqhYPSyFm6noqMTO/ryBELdfqglrr4Vce2
  secret_key_base: "${SECRET_KEY_BASE}"

# https://github.com/sntran/ueberauth_auth0
config :ueberauth, Ueberauth.Strategy.Auth0.OAuth,
  domain: "${AUTH0_DOMAIN}",
  client_id: "${AUTH0_CLIENT_ID}",
  client_secret: "${AUTH0_CLIENT_SECRET}"
