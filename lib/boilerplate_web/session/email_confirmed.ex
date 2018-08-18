defmodule BoilerplateWeb.Session.EmailConfirmed do
  @moduledoc """
  A Plug middleware that checks that the logged in user has confirmed their
  email. If the email is not confirmed in it redirects the user elsewhere.
  """

  alias Boilerplate.User
  alias BoilerplateWeb.Router
  import Boilerplate.Gettext, only: [gettext: 1]

  def init(opts) do
    opts
  end

  def call(conn, _opts) do
    case conn.assigns.user.email_confirmed_at do
      nil ->
        conn
        |> Phoenix.Controller.put_flash(:error, gettext("Please confirm your email to continue"))
        |> Phoenix.Controller.redirect(to: Router.Helpers.email_confirmation_path(conn, :index))
        |> Plug.Conn.halt()

      _ ->
        conn
    end
  end
end
