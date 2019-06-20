defmodule Fawkes.GuardianSerializer do
  @moduledoc """
  Responsible for teaching the Guardian authentication framework how to convert
  a user into a token and back again.
  """

  @behaviour Guardian.Serializer

  alias Fawkes.Repo
  alias Fawkes.User

  def for_token(user = %User{}) do
    {:ok, "User:#{user.id}"}
  end
  def for_token(_) do
    {:error, "Unknown resource type"}
  end

  def from_token("User:" <> id) do
    {:ok, Repo.get(User, id)}
  end
  def from_token(_) do
    {:error, "Unknown resource type"}
  end
end
