# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

import_config "logger.exs"
import_config "phoenix.exs"
import_config "ecto.exs"
import_config "bcrypt.exs"
import_config "swoosh.exs"

import_config "#{Mix.env()}.secret.exs"

if Mix.env() == :dev do
  config :mix_test_watch,
    clear: true
end
