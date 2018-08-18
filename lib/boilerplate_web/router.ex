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

  pipeline :email_confirmed do
    plug(BoilerplateWeb.Session.EmailConfirmed)
  end

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
    delete("/session", Session.Controller, :delete, as: :session)

    get("/register", Registration.Controller, :new, as: :registration)
    post("/register", Registration.Controller, :create, as: :registration)
  end

  # Authenticated but not confirmed browser routes
  scope "/", BoilerplateWeb do
    pipe_through([:browser, :authenticated])

    resources(
      "/email-confirmation",
      EmailConfirmation.Controller,
      only: [:index, :create, :show],
      as: :email_confirmation
    )
  end

  # Authenticated browser routes
  scope "/", BoilerplateWeb do
    pipe_through([:browser, :authenticated, :email_confirmed])

    get("/dashboard", Dashboard.Controller, :show, as: :dashboard)
  end

  # API routes
  scope "/v1", BoilerplateWeb do
    pipe_through(:api)

    get("/status", Status.Controller, :show)
  end

  # Magic development routes
  if Mix.env() == :dev do
    scope "/dev" do
      pipe_through([:browser])

      forward("/mailbox", Plug.Swoosh.MailboxPreview, base_path: "/dev/mailbox")
    end
  end
end
