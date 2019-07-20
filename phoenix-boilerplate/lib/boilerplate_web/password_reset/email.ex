defmodule BoilerplateWeb.PasswordReset.Email do
  import Boilerplate.Gettext

  use Phoenix.Swoosh,
    view: BoilerplateWeb.PasswordReset.View,
    layout: {BoilerplateWeb.LayoutView, :email}

  def password_reset_email(user, reset_token) do
    new()
    |> from(BoilerplateWeb.noreply_email_address())
    |> to(user.email)
    |> subject(gettext("Reset your Boilerplate password"))
    |> render_body("password_reset_email.html", %{token_id: reset_token.id})
  end
end
