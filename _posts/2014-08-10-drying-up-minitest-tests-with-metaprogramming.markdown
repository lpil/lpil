---
layout: post
title:  "DRYing up Minitest tests with metaprogramming"
categories:
  - Web dev
  - Rails
  - TDD
---

I've recently been doing a lot of Rails work and test driven development
(naturally), and I've found it's very easy to get a hell of a lot of
duplication in Minitest tests.

For instance, if I want to test that my User model responds to various method
calls, I could end up with code like this:

{% highlight ruby linenos %}
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
{% endhighlight %}

This is already too repetitive, and we've only checked 3 methods! If you're
anything like me there's probably a lot more than 3 methods you want to check.
If we can DRY this out, there's no reason why we shouldn't test every method we
add to the model.

# Iterate and assert

One obvious solution would be to load the name symbols of the methods we want
to check into an array, and then iterate though the array, asserting the model
responds to each symbol in turn. It might look something like this:

{% highlight ruby linenos %}
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
{% endhighlight %}

That's better. By adding a bit of basic logic We've checked that a whole bunch
of methods exist, and we've managed to not repeat ourselves at all.

... I still don't like this...

The problem here is that we've got a lot of assertions in one test, so if an
assertion fails, the whole test fails, and the methods later in the array don't
get tested at all. If this test fails on the `first_name` assertion, is
`first_name` the only method that we've a problem with? I've no way of telling
without either adding more tests, or checking manually.

# Iterate and metaprogram

What I really want is an automated way of creating one test per method to be
checked. How can we do this? Metaprogramming, of course!

{% highlight ruby linenos %}
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
{% endhighlight %}

Here we're using `class_eval`, a method which evaluates the string passed to it
if it were regular Ruby code in the context of the class (in this case, my
TestCase class). So, by passing in a string that looks like a method definition
we can add a new method to the class at runtime. Add an array of method names,
and some string interpolation, and we can create a new test for each method
that we wish to test on the model!

The only thing I tripped up on is that because of how string interpolation
works with symbols, you need to re-add the `:` before the symbol when passing
it to the `respond_to?` method. See line 8 of the code block above.

So there we have it, DRY-er tests that don't hide the details from us. And
because we've done it with metaprogramming, we get Rubyist street cred.
Banging.

Cheers,  
Louis
