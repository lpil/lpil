---
title:  "DRYing up Minitest tests with metaprogramming"
categories:
  - Web dev
  - Rails
  - TDD
---

I've recently been doing a lot of Rails work with test-driven development
(naturally), and I've found it's very easy to get a hell of a lot of
duplication in Minitest tests.

For instance, if I want to test that my User model responds to various method
calls, I could end up with code like this:

```ruby
class UserTest < ActiveSupport::TestCase

  def test_user_responds_to_first_name
      assert User.new.respond_to?(:first_name),
        'Users should respond to first_name'
  end

  def test_user_responds_to_last_name
      assert User.new.respond_to?(:last_name),
        'Users should respond to last_name'
  end

  def test_user_responds_to_email
      assert User.new.respond_to?(:email),
        'Users should respond to email'
  end

  # and so on...
end
```

The problem here is that code like this heavily violates the D.R.Y. principle-
Don't repeat yourself. Repetitive code is a pain to work with and maintain; If
we need to make a slight change to our testing process here we have to perform
the same edit in every place this snippet crops up in our code, which could
take quite some time with a comprehensive test suite. Worse still, it's much
more likely that we will make a mistake if we have to make an edit twenty
times, rather than just once.

# Iterate and assert

One obvious solution would be to load the name symbols of the methods we want
to check into an array, and then iterate though this array, asserting the model
responds to each symbol in turn. It might look something like this:

```ruby
class UserTest < ActiveSupport::TestCase

  def test_user_responds_to_various_methods
    %i(first_name last_name email admin password created_at updated_at
       password_confirmation remember_token collection_id authenticate
      ).each do |method|
      assert User.new.respond_to?(method),
        "Users should respond to #{method}"
    end
  end
end
```

That's better. By adding a bit of basic logic we've checked that a whole bunch
of methods exist, and we've managed to not repeat ourselves at all.

... I still don't like this...

The problem here is that we've got a lot of assertions in one test, so if an
assertion fails, the whole test fails, and the methods later in the array don't
get tested at all. If this test fails on the `first_name` assertion, is
`first_name` the only method that we've a problem with? We've no way of telling
without either adding more tests, or checking manually.

# Iterate and metaprogram

What I really want is an automated way of creating one test per method to be
checked. How can we do this? Metaprogramming, of course!

```ruby
class UserTest < ActiveSupport::TestCase

  %i(first_name last_name email admin password created_at updated_at
     password_confirmation remember_token collection_id authenticate
    ).each do |method|
    class_eval %{
      def test_user_responds_to_#{method}
        assert User.new.respond_to?(:#{method}),
          'Users should respond to #{method}'
      end
    }
  end
end
```

Here we're using `class_eval`, a method which evaluates the string passed to it
if it were regular Ruby code in the context of the class (in this case, my
UserTest class). So, by passing in a string that looks like a method definition
we can add a new method to the class at runtime. If we add an array of method
names, and some string interpolation, and we can create a new test for each
method that we wish to test on the model! 

The only thing I tripped up on is that because of how string interpolation
works with symbols, you need to re-add the `:` before the symbol when passing
it to the `respond_to?` method. See the eighth line of the code block above.

You don't have to just use this technique for checking whether model responds a
certain method call, anywhere were you've got multiple near-identical test
methods could be a good place to use metaprogramming in this way. For example,
checking whether a non-admin user can successfully perform action X on a
controller.

So there we have it, DRY-er tests that don't hide the details from us. And
because we've done it with metaprogramming, we get Rubyist street cred.
Banging.

Cheers,  
Louis
