# Script for populating the database. You can run it as:
#
#     mix run priv/repo/seeds.exs
#
# Inside the script, you can read and write to any of your
# repositories directly:
#
#     Fawkes.Repo.insert!(%Fawkes.SomeModel{})
#
# We recommend using the bang functions (`insert!`, `update!`
# and so on) as they will fail if something goes wrong.

# TODO: Only do this in dev

alias Fawkes.Repo
alias Fawkes.User

attrs = %{
  email: "louis@lpil.uk",
  username: "lpil",
  password: "1234567890",
  password_confirmation: "1234567890",
}
%User{} |> User.registration_changeset |> Repo.insert!
