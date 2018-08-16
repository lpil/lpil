defmodule BoilerplateWeb.Router do
  use BoilerplateWeb, :router

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
    plug(BoilerplateWeb.Session.Enforce)
  end

  # Middleware stack used for API requests
  pipeline :api do
    plug(:accepts, ["json"])
  end

  # Unauthenticated browser routes
  scope "/", BoilerplateWeb do
    pipe_through(:browser)

    get("/", Page.Controller, :index, as: :page)

    get("/login", Session.Controller, :new, as: :session)
    post("/login", Session.Controller, :create, as: :session)
  end

  # Authenticated browser routes
  scope "/", BoilerplateWeb do
    pipe_through([:browser, :authenticated])

    get("/dashboard", Dashboard.Controller, :show, as: :dashboard)
  end

  # API routes
  scope "/v1", BoilerplateWeb do
    pipe_through(:api)

    get("/status", Status.Controller, :show)
  end
end
