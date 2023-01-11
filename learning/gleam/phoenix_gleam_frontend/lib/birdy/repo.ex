defmodule Birdy.Repo do
  use Ecto.Repo,
    otp_app: :birdy,
    adapter: Ecto.Adapters.Postgres
end
