defmodule NextBreakFits do
  defmacrop doc_cons(left, right) do
    quote do: {:doc_cons, unquote(left), unquote(right)}
  end

  defmacrop doc_nest(doc, indent) do
    quote do: {:doc_nest, unquote(doc), unquote(indent)}
  end

  defmacrop doc_break(break) do
    quote do: {:doc_break, unquote(break)}
  end

  defmacrop doc_group(group) do
    quote do: {:doc_group, unquote(group)}
  end

  defmacrop doc_fits(group) do
    quote do: {:doc_fits, unquote(group)}
  end

  # Algebra API

  def concat(doc1, doc2) do
    doc_cons(doc1, doc2)
  end

  def nest(doc, level) do
    doc_nest(doc, level)
  end

  def break(string \\ " ") when is_binary(string) do
    doc_break(string)
  end

  def next_break_fits(doc) do
    doc_fits(doc)
  end

  def group(doc) do
    doc_group(doc)
  end

  def format(doc, width) do
    format(width, 0, [{0, :flat, doc}])
  end

  @type t :: any()
  @typep mode :: :flat | :break | :next_break_fits

  @spec fits?(
          width :: non_neg_integer(),
          column :: non_neg_integer(),
          break? :: boolean(),
          entries
        ) :: boolean()
        when entries: [{integer(), mode(), t()}]

  defp fits?(w, k, b?, _) when k > w and b?, do: false
  defp fits?(_, _, _, []), do: true
  defp fits?(w, k, _, {:tail, b?, t}), do: fits?(w, k, b?, t)

  ## Breaks no flat

  defp fits?(w, k, b?, [{i, _, doc_fits(x)} | t]),
    do: fits?(w, k, b?, [{i, :next_break_fits, x} | t])

  defp fits?(_, _, _, [{_, :next_break_fits, doc_break(_)} | _]), do: true

  ## Breaks

  defp fits?(_, _, _, [{_, :break, doc_break(_)} | _]), do: true

  defp fits?(w, k, b?, [{i, :break, doc_group(x)} | t]),
    do: fits?(w, k, b?, [{i, :flat, x} | t])

  ## Catch all

  defp fits?(w, k, b?, [{_, _, s} | t]) when is_binary(s), do: fits?(w, k + byte_size(s), b?, t)
  defp fits?(w, k, _, [{_, _, doc_break(s)} | t]), do: fits?(w, k + byte_size(s), true, t)

  defp fits?(w, k, b?, [{i, m, doc_nest(x, j)} | t]),
    do: fits?(w, k, b?, [{i + j, m, x} | t])

  defp fits?(w, k, b?, [{i, m, doc_cons(x, y)} | t]),
    do: fits?(w, k, b?, [{i, m, x}, {i, m, y} | t])

  defp fits?(w, k, b?, [{i, m, doc_group(x)} | t]),
    do: fits?(w, k, b?, [{i, m, x} | t])

  defp format(_, _, []), do: []
  defp format(w, k, [{i, m, doc_cons(x, y)} | t]), do: format(w, k, [{i, m, x}, {i, m, y} | t])
  defp format(w, k, [{_, _, s} | t]) when is_binary(s), do: [s | format(w, k + byte_size(s), t)]
  defp format(w, k, [{i, m, doc_fits(x)} | t]), do: format(w, k, [{i, m, x} | t])

  defp format(w, k, [{i, mode, doc_nest(x, j)} | t]),
    do: format(w, k, [{i + j, mode, x} | t])

  defp format(w, k, [{i, mode, doc_break(s)} | t]) do
    if mode == :break do
      [indent(i) | format(w, i, t)]
    else
      [s | format(w, k + byte_size(s), t)]
    end
  end

  defp format(w, k, [{i, _, doc_group(x)} | t]) do
    if fits?(w, k, false, [{i, :flat, x}]) do
      format(w, k, [{i, :flat, x} | t])
    else
      format(w, k, [{i, :break, x} | t])
    end
  end

  defp indent(i), do: "\n" <> :binary.copy(" ", i)
end
