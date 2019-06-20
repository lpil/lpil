defmodule Fawkes.PublishableQuery do
  @moduledoc """
  Queries relating to items that have `published_at` date.
  """

  import Ecto.Query, only: [from: 1, from: 2]

  @doc """
  Return entities that are "published".

  This is determined by whether or not the `published_at` date is in the past.
  """
  def published(query) do
    now = Ecto.DateTime.utc
    from(a in query, where: a.published_at <= ^now)
  end
end
