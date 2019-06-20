defmodule Fawkes.Router do
  @moduledoc false
  use Fawkes.Web, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
    plug Guardian.Plug.VerifySession
    plug Guardian.Plug.LoadResource
  end

  pipeline :browser_admin do
    plug Guardian.Plug.EnsureAuthenticated, handler: Fawkes.ErrorController
  end


  scope "/", Fawkes do
    pipe_through :browser
    pipe_through :browser_admin

    resources "/articles", ArticleController,
      only: ~w(new show create)a
  end

  scope "/", Fawkes do
    pipe_through :browser

    resources "/session", SessionController,
      only: ~w(new create delete)a,
      singleton: true

    # This is a catch all route, so it goes last.
    resources "/", BlogController,
      only: ~w(index show)a
  end
end
