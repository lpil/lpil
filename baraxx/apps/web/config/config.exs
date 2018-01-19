use Mix.Config

config :web, enable_web_server: Mix.env() != :test
