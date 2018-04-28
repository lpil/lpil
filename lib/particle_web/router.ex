defmodule ParticleWeb.Router do
  use ParticleWeb, :router

  # Middleware stack used for routes serving browser requests
  pipeline :browser do
    plug(:accepts, ["html"])
    plug(:fetch_session)
    plug(:fetch_flash)
    plug(:protect_from_forgery)
    plug(:put_secure_browser_headers)
  end

  # Middleware stack used for routes that require auth
  pipeline :authenticated do
    plug(ParticleWeb.EnsureAuthenticated)
  end

  # Middleware stack used for API requests
  pipeline :api do
    plug(:accepts, ["json"])
  end

  # Unauthenticated browser routes
  scope "/", ParticleWeb do
    pipe_through(:browser)

    get("/", PageController, :index)
    get("/login/:provider", AuthController, :request, as: :login)
    get("/login/:provider/callback", AuthController, :callback)
  end

  # Authenticated browser routes
  scope "/", ParticleWeb do
    pipe_through([:browser, :authenticated])

    get("/dashboard", DashboardController, :show)
  end

  # API routes
  scope "/v1", ParticleWeb do
    pipe_through(:api)

    get("/status", StatusController, :show)
  end
end
