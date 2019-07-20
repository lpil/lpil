use Mix.Config

case Mix.env() do
  :test ->
    config :bcrypt_elixir, :log_rounds, 4

  _ ->
    :ok
end
