defmodule Term do
  @moduledoc """
  Tiny transformation helper functions, useful for creating pipelines from
  forms that don't have a function-call syntax. Everything should have a
  function call syntax.

  Could probably be extracted into a library if desired.
  """

  @doc """
  Wrap a term in an ok tuple.

      iex> Term.ok(1)
      {:ok, 1}
  """
  @spec ok(term) :: {:ok, term}
  def ok(x) do
    {:ok, x}
  end

  @doc """
  Wrap a term in an error tuple.

      iex> Term.error(1)
      {:error, 1}
  """
  @spec error(term) :: {:error, term}
  def error(x) do
    {:error, x}
  end

  @doc """
  Replace a nil value with a default.

      iex> Term.default(nil, 1)
      1

      iex> Term.default("ok", 1)
      "ok"
  """
  @spec default(nil | term, term) :: term
  def default(x, default) do
    x || default
  end
end
