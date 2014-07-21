require 'test_helper'

class UserTest < ActiveSupport::TestCase

  def test_user_responds_to_correct_fields
    user = FactoryGirl.build(:user)
    %i(first_name last_name email reporter uploader).each do |field|
      assert user.respond_to?(field), "User should respond to #{field}"
    end
  end

  def test_user_factory_default_valid
    assert FactoryGirl.build(:user).valid?
  end

  def test_user_invalid_without_email
    refute FactoryGirl.build(:user, email: '').valid?
  end

  def test_some_valid_emails
    %w(user@foo.COM A_US-ER@f.b.org first.last@foo.jp a+b@baz.uk).each do |e|
      assert FactoryGirl.build(:user, email: e).valid?, "#{e} should be valid"
    end
  end

  def test_some_invalid_emails
    %w(user@foo,com user_at_foo.org example.user@foo. foo@bar_baz.com
       foo@bar+baz.com).each do |e|
      refute FactoryGirl.build(:user, email: e).valid?,
        "#{e} shouldn't be valid"
    end
  end
end
