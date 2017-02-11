---
title: How to make a programming language
tags:
  - Languages
  - Interpreters
---

What actually happens in an interpreter when it is evaluating your code?
Presumably some very impressive voodoo understandable only to doctors of
computer science. The kind of people who are fluent in C++ and can blast
through the Euler Project in Haskell before their morning Weetabix.

Right? Wrong. It's actually relatively straightforward and an interpreter can
be built by mere mortals such as you and I. Let's explore some basic language
interpreter concepts in Elixir, a nice friendly high level language. No
pointer arithmetic or segfaults here!

The first thing to know is that all code syntax can be expressed as a tree.
Take the expression below, in which `+` is called with the numbers `1` and
`2`.

This can be expressed as a tree with `+` as the root nodes, and the numbers as
its child nodes.

```elixir
1 + 2
```
```
call: +
 ├── number: 1
 └── number: 2
```

We can do the same for more complex expression.

```elixir
1 + 2 == 3
```
```
call: ==
 ├── call: +
 │    ├── number: 1
 |    └── number: 2
 └── number: 3
```

Here the `==` is the root node and it has two children nodes, a call
to `+` and the number 3. The call to `+` has two child nodes, the number 1
and the number 2.

All source code can be expressed as a tree data structure (an [abstract syntax
tree][ast] to be precise). Let's implement this in Elixir using a struct for
each type of node we've seen so far, as well as a boolean node, which would be the
result of `==`.

[ast]: https://en.wikipedia.org/wiki/Abstract_syntax_tree

```elixir
defmodule Number do
  defstruct :value
  def new(value), do: %Number{value: value}
end

defmodule Add do
  defstruct :left, :right
  def new(left, right), do: %Add{left: left, right: right}
end

defmodule Equals do
  defstruct :left, :right
  def new(left, right), do: %Equals{left: left, right: right}
end

defmodule Boolean do
  defstruct :value
  def new(value), do: %Boolean{value: value}
end
```

Now we can model `1 + 1` like this:

```elixir
Add.new(Number.new(1),
        Number.new(1))
```

And we can model `1 + 2 == 4` like this:


```elixir
Equals.new(Add.new(Number.new(1),
                   Number.new(2)),
           Number.new(4))
```

Now can can express programs in our code, providing those programs do nothing
other than add numbers and check for equality. How do we go one step further
and execute this tree? First we need to think about what happens when we step
through code.

One way to think about this is to model it as the tree going through a series
of reduction steps. A node tries to reduce its first child, and if it
can then that's one reduction step. If not it tries to reduce its next child,
and so on until it has no more children to reduce, after that the only step is
to reduce itself.

This all seems a bit abstract, so let's look at `1 + 2 == 4` again.

First `==` tried tries to reduce its first child, which is the left hand side
expression `1 + 2`.

`+` can be reduced, but first it needs to try and reduce its children. `1` and
`2` are numbers, which can't be reduced, so `+` reduces itself it a number by
summing the value of its two children, making `3`.

That's one reduction step. The expression is now `3 == 4`.

For the next step `==` tries to reduce its first child, `3`, but cannot as
numbers cannot be reduced. Then it tries to reduce its second child, `4`, but
again it cannot. Now there are no more children to reduce, it reduces itself
to `false` by checking if the children are equal.

```elixir
# Initial expression
1 + 2 == 4

# Reduce step 1
3 == 4

# Reduce step 2
false
```

Or in the Elixir AST:

```elixir
# Initial expression
Equals.new(Add.new(Number.new(1),
                   Number.new(2)),
           Number.new(4))

# Reduce step 1
Equals.new(Number.new(3),
           Number.new(4))

# Reduce step 2
Boolean.new(false)
```

To teach the Elixir AST how to reduce I'm doing to define an Elixir protocol
and then implement it for each of the structs (it's a bit like an interface in
an OO language).

This protocol will have a function `reduce`, which takes a node and returns
either a reduced node, or the atom `:noop`, signifying that it cannot be
reduced.

```elixir
defprotocol Node do
  def reduce(node)
end
```

Numbers cannot be reduced, so they return `:noop`.

```elixir
defimpl Node, for: Number do
  def reduce(_number) do
    :noop
  end
end
```
```elixir
result = Node.reduce(Number.new(1))
assert result == :noop
```

And the same for Boolean.

```elixir
defimpl Node, for: Boolean do
  def reduce(_boolean) do
    :noop
  end
end
```
```elixir
result = Node.reduce(Boolean.new(true))
assert result == :noop
```

Add is more complex. It tries to reduce each of its children, and then reduces
itself if they both `:noop`.

```elixir
defimpl Node, for: Add do
  def reduce(add) do
    case Node.reduce(add.left) do
      {:ok, reduced} ->
        Add.new(reduced, add.right)
      :noop ->
        case Node.reduce(add.right) do
          {:ok, reduced} ->
            Add.new(add.left, reduced)
          :noop ->
            {:ok, Number.new(add.left.value + add.right.value)}
        end
    end
  end
end
```
```elixir
result = Node.reduce(Add.new(Number.new(1), Number.new(1)))
assert result == {:ok, Number.new(2)}
```

This code's pretty grim, so I'll use Elixir's monadic `with` to tidy it up a
bit. This code does exactly the same thing, except the happy path is written
first, and then the other paths are in the `else` block.

```elixir
defimpl Node, for: Add do
  def reduce(add) do
    with {:left, :noop} <- {:left, Node.reduce(add.left)},
         {:right, :noop} <- {:right, Node.reduce(add.right)} do
      {:ok, Number.new(add.left.value + add.right.value)}
    else
      {:left, {:ok, reduced}} ->
        {:ok, Add.new(reduced, add.right)}

      {:right, {:ok, reduced}} ->
        {:ok, Add.new(add.left, reduced)}
    end
  end
end
```

And then lastly there's `Equals`, which is pretty much the same as `Add`.

```elixir
defimpl Node, for: Add do
  def reduce(add) do
    with {:left, :noop} <- {:left, Node.reduce(add.left)},
         {:right, :noop} <- {:right, Node.reduce(add.right)} do
      {:ok, Boolean.new(add.left.value == add.right.value)}
    else
      {:left, {:ok, reduced}} ->
        {:ok, Add.new(reduced, add.right)}

      {:right, {:ok, reduced}} ->
        {:ok, Add.new(add.left, reduced)}
    end
  end
end
```

Can you spot the difference? It's just this line in the middle.

```elixir
{:ok, Boolean.new(add.left.value == add.right.value)}
```

Maybe there's an abstraction to be extracted here. For now I'll leave it as it
is- duplication is better than the wrong abstraction.

Right, so if we can reduce expressions by a single step the only thing left to
do is perform this step repeatedly until the expression can be reduced no
more.

```elixir
defmodule Program do
  def run(expression) do
    case Node.reduce(expression) do
      {:ok, reduced} ->
        run(reduced)
      :noop ->
        expression
    end
  end
end
```

And if we try it out...

```elixir
ast = Equals.new(Add.new(Number.new(1),
                         Number.new(2)),
                 Number.new(4))

result = Program.run(ast)
assert result == Boolean.new(false)
```

Voila! A new programming language is born. Not a very useful one, but it'll
grow more powerful as more types of node are added, each bringing new
behaviour to the language.

I've been exploring these ideas over the last month and I've now got a tiny
little language written in Elixir, including a parser that turns the source
code into the AST introduced in this post. The source for this project can be
found on GitHub [here][repo].

[repo]: https://github.com/lpil/soup

If you're interested in learning more about language and computation I
recommend reading Tom Stuart's [Understanding Computation][book].

[book]: http://computationbook.com/

Tara! :)
