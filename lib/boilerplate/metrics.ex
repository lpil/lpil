defmodule Boilerplate.Metrics do
  @moduledoc """
  A module for reporting metrics.

  Currently a wrapper layer for Appsignal. At time of writing we're
  evaluating it for the first time so we don't want to have to rework
  half the application in the event we decide to remove it. Instead
  we'll just change the implementation of these functions.

  Also logging!
  """

  require Logger

  @doc """
  Increment a counter, aggregated across the entirel cluster.
  """
  @spec increment_counter(String.t(), pos_integer) :: :ok
  def increment_counter(counter_key, delta \\ 1) do
    # Log
    Logger.debug(["Metrics counter ", counter_key, " +", to_string(delta)])
  end

  @doc """
  Calls and records a function as an event in the current transaction
  (i.e. A database query in a HTTP request/response cycle.), and
  returns the value of the function.

  Does not record metrics if not in a transaction.

  ## Arguments

  - `name`: Name of the category of the event (sql.query, net.http)
  - `title`: Title of the event ('User load', 'Http request to google.com')
    specific event (`select * from users where id=?`)
  """
  @spec record_event(String.t(), (() -> term)) :: term
  def record_event(name, fun) do
    {microseconds, result} = :timer.tc(fun)

    # Log
    Logger.debug(["Metrics event ", name, ?\s, to_string(microseconds), "us"])

    result
  end
end
