defmodule BoilerplateWeb.Mailer do
  @moduledoc """
  Responsible for the sending of emails.
  """

  use Swoosh.Mailer, otp_app: :boilerplate
end
