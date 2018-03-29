defmodule FcatWeb.AuthController do
  use FcatWeb, :controller
  alias Fcat.Metrics

  # Implements request handlers for authentication methods.
  plug(Ueberauth)

  # Catch requests not handled by the Ueberauth plug.
  def request(conn, _params) do
    raise Phoenix.Router.NoRouteError, conn: conn, router: FcatWeb.Router
  end

  # This callback/2 function is called by Ueberauth after
  # an auth attempt has been made. Here we handle success or failure.
  #
  def callback(%{assigns: %{ueberauth_failure: _fails}} = conn, _params) do
    Metrics.increment_counter("auth/login/failure")

    conn
    |> put_flash(:error, "Failed to authenticate.")
    |> redirect(to: "/")
  end

  def callback(%{assigns: %{ueberauth_auth: auth}} = conn, _params) do
    Metrics.increment_counter("auth/login/success")

    user_email = auth.info.email
    # TODO
    user = user_email

    conn
    |> put_flash(:info, "Successfully authenticated. #{inspect(auth)}")
    |> put_session(:current_user, user)
    |> redirect(to: "/")

    # TODO: Implement user persistence

    # case UserFromAuth.find_or_create(auth) do
    #   {:ok, user} ->
    #     conn
    #     |> put_flash(:info, "Successfully authenticated.")
    #     |> put_session(:current_user, user)
    #     |> redirect(to: "/")
    #   {:error, reason} ->
    #     conn
    #     |> put_flash(:error, reason)
    #     |> redirect(to: "/")
    # end
  end
end
