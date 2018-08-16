defmodule Fixture do
  @moduledoc """
  Fixture data for use in tests.
  """

  def user_params(params \\ []) do
    params
    |> Enum.into(%{
      name: "Ginny Weasley",
      email: "ginny#{i()}@weasley.family",
      password: "potterisacutie!"
    })
  end

  def organisation_params(params \\ []) do
    params
    |> Enum.into(%{name: "Ravenclaw#{i()}"})
  end

  defp i do
    System.unique_integer([:positive])
  end
end
