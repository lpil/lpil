---
layout: post
title: Automatically Running Rust Unit Tests
categories:
  - Rust
  - Testing
---

[Rust][rust-lang] is a really cool new programming language from Mozilla. It
features guaranteed memory safety with manual memory management, zero cost
abstractions, pattern matching, and a type system that reminds me of Haskell,
with algebraic data types and traits. Sounds cool, right?

If you're like me, you probably want to write tests with your code, especially
when working with a language that doesn't have a REPL ([yet...][repl-yet]).
You'll probably also want the relevant tests to run automatically when you save
a file after modification. [Cargo][cargo], the rust package manager/build
tool, doesn't have ability to do this yet, but we can get more or less the same
functionality using the lovely Ruby tool [Guard][guard].

I'm new to Rust, and the language has yet to reach version 1, so there may be
situations in which this method doesn't always work, but this will work for
modules as they are described in the Rust book at time of writing.

## TL;DR

~~~
$ gem install guard-shell
~~~

{% highlight ruby %}
# Guardfile
guard :shell do
  watch(/src\/.*\.rs/) do |m|
    path = m.first

    mod = unless %w(src/main.rs src/lib.rs).include? path
            path.sub(/src\//, '')
                .sub(/\/tests\.rs/, '.rs')
                .sub(/\/mod\.rs/, '.rs')
                .gsub(/\//, '::')
                .sub(/\.rs/, '::tests')
          end

    puts "\n\n\n\n"
    puts "\e[33mcargo test #{mod}\e[0m"
    `cargo test #{mod}`
  end
end
{% endhighlight %}
{% comment %}close itals -> *{% endcomment %}

~~~
$ guard
~~~

## The Process

#### Install Guard

Guard monitors the file system for changes. Let's install the guard-shell, a
guard that allows us to run shell commands when guard detects that we've
modified a file.

~~~
$ gem install guard-shell
~~~

Alternative, put it in a Gemfile and manage Ruby dependencies with
[Bundler][bundler]. This is probably a better idea long term.

In your Rust project directory run `guard init shell` from the shell to create
a Guardfile. It'll look something like this, preceded by a load of comments:

{% highlight ruby %}
guard :shell do
  watch(/(.*).txt/) {|m| `tail #{m[0]}` }
end
{% endhighlight %}
{% comment %}close itals -> *{% endcomment %}

If you're familiar with Ruby you'll see that there is a `watch` function being
called with a regexp, and into that function a block is being passed that
accepts one argument. The regexp here dictates which files we want the block to
be executed for, and the arg is an array of string paths of the files Guard has
detected a change in. Right now it just `tail`s a file when it's modified,
let's make it do something useful.

#### Configure Guard

In Rust unit tests for a module are kept in a sub-module named 'tests', and
the directory tree structure mirrors the module tree structure. For example,
the module `game` can be found in the file `src/game/mod.rs`, and the module
`game::player` can be found in the file `src/game/player.rs`. Cool, so using
information we can work out what the module name would be from the path of the
modified file.

Let's change that regexp to match Rust (`.rs`) files within the source
directory.

{% highlight ruby %}
guard :shell do
  watch(/src\/(.*)\.rs/) {|m| `tail #{m[0]}` }
end
{% endhighlight %}
{% comment %}close itals -> *{% endcomment %}

Now, let's use the path string to create a string containing the name of the
tests module we want to run for that file. See the comments for a step-by-step
walk-through.

{% highlight ruby %}
guard :shell do
  watch(/src\/.*\.rs/) do |m|

    # Get the path from the array
    path = m.first

               # Drop the 'src/' directory part
    mod = path.sub(/src\//, '')

               # Swap '/mod.rs' for '.rs', as the module name of those
               # files is that of the parent directory.
              .sub(/\/mod\.rs/, '.rs')

               # Handle the tests that are in a file in the sub-directory,
               # rather than in the same file.
              .sub(/\/tests\.rs/, '.rs')

               # Swap '/' directory separtors for '::' module separtors
              .gsub(/\//, '::')

               # Swap the file extension for '::tests', as the tests
               # are contained within a sub-module called 'tests'
              .sub(/\.rs/, '::tests')
          end
  end
end
{% endhighlight %}
{% comment %}close itals -> *{% endcomment %}

And now that we have have the module name, we can run it with with
`$ cargo run module::name`

{% highlight ruby %}
guard :shell do
  watch(/src\/.*\.rs/) do |m|
    path = m.first

    mod = path.sub(/src\//, '')
                .sub(/\/tests\.rs/, '.rs')
                .sub(/\/mod\.rs/, '.rs')
                .gsub(/\//, '::')
                .sub(/\.rs/, '::tests')
          end

    `cargo test #{mod}`
  end
end
{% endhighlight %}
{% comment %}close itals -> *{% endcomment %}

However there is a problem here- it won't work for the top level `src/main.rs`
or `src/lib.rs` files. I've decided in this case I want to run all the tests
with `cargo test`, so I'm going to add a conditional so that the module path is
not computed for these files.

{% highlight ruby %}
guard :shell do
  watch(/src\/.*\.rs/) do |m|
    path = m.first

    mod = unless %w(src/main.rs src/lib.rs).include? path
            path.sub(/src\//, '')
                .sub(/\/tests\.rs/, '.rs')
                .sub(/\/mod\.rs/, '.rs')
                .gsub(/\//, '::')
                .sub(/\.rs/, '::tests')
          end

    `cargo test #{mod}`
  end
end
{% endhighlight %}
{% comment %}close itals -> *{% endcomment %}

And lastly, let's print a few newlines and the module path in a colour so that
we can see where each round of output starts a little easier.

{% highlight ruby %}
guard :shell do
  watch(/src\/.*\.rs/) do |m|
    path = m.first

    mod = unless %w(src/main.rs src/lib.rs).include? path
            path.sub(/src\//, '')
                .sub(/\/tests\.rs/, '.rs')
                .sub(/\/mod\.rs/, '.rs')
                .gsub(/\//, '::')
                .sub(/\.rs/, '::tests')
          end

    puts "\n\n\n\n"
    puts "\e[33mcargo test #{mod}\e[0m"
    `cargo test #{mod}`
  end
end
{% endhighlight %}
{% comment %}close itals -> *{% endcomment %}

And that's it. Run `$ guard` from the project directory and you're good to go.
:)

Cheers,  
Louis

[rust-lang]: http://www.rust-lang.org/
[repl-yet]: https://github.com/rust-lang/rust/issues/9898
[cargo]: https://github.com/rust-lang/cargo
[guard]: https://github.com/guard/guard
[bundler]:http://bundler.io/
