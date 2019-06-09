defmodule Fixture do
  @moduledoc """
  Fixture data for use in tests.
  """

  alias Particle.User

  def user do
    User.insert(user_params())
  end

  def user_params do
    %{email: "e@ma.il"}
  end
end
