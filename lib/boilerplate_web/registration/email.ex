defmodule BoilerplateWeb.Registration.Email do
  import Boilerplate.Gettext

  use Phoenix.Swoosh,
    view: BoilerplateWeb.Registration.View,
    layout: {BoilerplateWeb.LayoutView, :email}

  def confirmation_email(user, confirmation_token) do
    new()
    |> from(BoilerplateWeb.noreply_email_address())
    |> to(user.email)
    |> subject(gettext("Welcome to Boilerplate! Please confirm your email"))
    |> render_body("confirmation_email.html", %{
      token_id: confirmation_token.id
    })
  end
end
