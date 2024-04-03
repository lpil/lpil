defmodule Foo do
  def head([]) do
    nil
  end

  def head([x|_]) do
    x
  end
end

IO.puts Foo.head([1,2,3,4])
