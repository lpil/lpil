defmodule ParticleWeb.AuthController do
  use ParticleWeb, :controller
  alias Particle.{Metrics, User}
  require Logger

  # Implements request handlers for authentication methods.
  plug(Ueberauth)

  # Catch requests not handled by the Ueberauth plug.
  def request(conn, _params) do
    raise Phoenix.Router.NoRouteError, conn: conn, router: ParticleWeb.Router
  end

  # This callback/2 function is called by Ueberauth after
  # an auth attempt has been made. Here we handle success or failure.

  # def callback(%{assigns: %{ueberauth_failure: _fails}} = conn, _params) do
  #   Metrics.increment_counter("auth/login/failure")
  #   conn
  #   |> put_flash(:error, "Failed to authenticate.")
  #   |> redirect(to: "/")
  # end

  def callback(%{assigns: %{ueberauth_auth: auth}} = conn, _params) do
    email = auth.info.email
    {:ok, user} = User.fetch_or_insert(%{email: email})

    Metrics.increment_counter("auth/login/success")
    Logger.info("auth/login/success #{email}", id: user.id, email: email)

    conn
    |> put_session(:uid, user.id)
    |> redirect(to: dashboard_path(conn, :show))
  end
end
