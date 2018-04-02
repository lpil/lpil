defmodule Particle do
  @moduledoc """
  Particle keeps the contexts that define your domain
  and business logic.

  Contexts are also responsible for managing your data, regardless
  if it comes from the database, an external API or others.
  """

  @type validation_error :: {:error, atom, atom, String.t()}
  @type invalid :: {:invald, [validation_error]}
end
