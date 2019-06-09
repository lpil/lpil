use Mix.Config

config :appsignal, :config,
  name: "particle",
  push_api_key: "6084790e-f1eb-41cc-bc76-c52b7cd64c22",
  active: Mix.env() != :test,
  env: Mix.env()

config :particle, ParticleWeb.Endpoint, instrumenters: [Appsignal.Phoenix.Instrumenter]

config :phoenix, :template_engines,
  eex: Appsignal.Phoenix.Template.EExEngine,
  exs: Appsignal.Phoenix.Template.ExsEngine
