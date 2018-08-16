use Mix.Config

config :ueberauth, Ueberauth,
  base_path: "/login",
  providers: [
    auth0: {Ueberauth.Strategy.Auth0, []}
  ]

case Mix.env() do
  :test ->
    config :ueberauth, Ueberauth.Strategy.Auth0.OAuth,
      domain: "example.auth0.com",
      client_id: "client_id",
      client_secret: "client_secret"

  _other ->
    # Get these values from secrets
    nil
end
