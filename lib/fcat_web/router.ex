defmodule FcatWeb.Router do
  use FcatWeb, :router

  pipeline :browser do
    plug(:accepts, ["html"])
    plug(:fetch_session)
    plug(:fetch_flash)
    plug(:protect_from_forgery)
    plug(:put_secure_browser_headers)
  end

  pipeline :api do
    plug(:accepts, ["json"])
  end

  scope "/", FcatWeb do
    pipe_through(:browser)

    get("/", PageController, :index)

    get("/login/:provider", AuthController, :request)
    get("/login/:provider/callback", AuthController, :callback)
  end

  scope "/v1", FcatWeb do
    pipe_through(:api)
  end
end
