defmodule Fcat.Metrics do
  @moduledoc """
  A module for collection metrics.

  Currently a wrapper layer for Appsignal. At time of writing we're
  evaluating it for the first time so we don't want to have to rework
  half the application in the event we decide to remove it. Instead
  we'll just change the implementation of these functions.
  """

  @spec increment_counter(String.t(), pos_integer) :: :ok
  def increment_counter(counter_key, delta \\ 1) do
    Appsignal.increment_counter(counter_key, delta)
  end
end
