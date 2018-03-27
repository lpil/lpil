use Mix.Config

# Values with the ${NAME} syntax will be replaced with environment
# by Distillery, the release bundler.

config :fcat, FcatWeb.Endpoint,
  # Secret key.
  # e.g. DIolo3iSU44aMkda9O3YvaQe7k3V0kkqhYPSyFm6noqMTO/ryBELdfqglrr4Vce2
  secret_key_base: "${SECRET_KEY_BASE}"
