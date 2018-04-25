defmodule Term do
  @moduledoc """
  Tiny transformation helper functions, useful for creating pipelines from
  forms that don't have a function-call syntax. Everything should have a
  function call syntax.

  Could probably be extracted into a library if desired.
  """

  @doc """
  Wrap a term in an tag tuple.

      iex> Term.tag(1, :invalid)
      {:invalid, 1}

      iex> Term.tag(50, :maybe)
      {:maybe, 50}
  """
  @spec tag(term, atom) :: {atom, term}
  def tag(x, tag) do
    {tag, x}
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

  @doc """
  Parse a struct from a dict with string keys.
  """
  @spec parse_struct(map, atom) :: map
  def parse_struct(props, struct_atom) do
    atom_props =
      props
      |> Enum.map(fn {k, v} -> {to_atom_or_nil(k), v} end)
      |> Enum.filter(fn {k, _} -> k != nil end)

    struct!(struct_atom, atom_props)
  end

  @doc """
  Convert a string to an existing atom, or nil.

      iex> Term.to_atom_or_nil("ok")
      :ok

      iex> Term.to_atom_or_nil("fwewkghwkjeghwjkgeh")
      nil
  """
  @spec to_atom_or_nil(String.t()) :: atom() | nil
  def to_atom_or_nil(string) do
    String.to_existing_atom(string)
  rescue
    _ -> nil
  end
end
