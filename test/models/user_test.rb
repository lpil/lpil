require 'test_helper'

class UserTest < ActiveSupport::TestCase

  def test_user_responds_to_correct_fields
    user = FactoryGirl.build(:user)
    %i(first_name last_name email reporter uploader password
       password_confirmation remember_token).each do |field|
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

  def test_dont_allow_duplicate_emails
    user = FactoryGirl.create(:user)
    refute user.dup.valid?
  end

  def test_dont_allow_duplicate_case_insensitive_emails
    user = FactoryGirl.create(:user)
    dup_user = user.dup
    dup_user.email = dup_user.email.upcase
    refute dup_user.valid?
  end

  def test_user_has_authenticate_method
    assert FactoryGirl.build(:user).respond_to?(:authenticate),
      'User should have authenticate method'
  end

  def test_passwords_must_be_longish
    user = FactoryGirl.build(:user)
    user.password = user.password_confirmation = 'a' * 7
    refute user.valid?, 'passwords must be longish'
  end

  def test_valid_user_authenticate_returns_user
    user = FactoryGirl.build(:user)
    assert_equal user, user.authenticate(user.password)
  end

  def test_invalid_user_authenticate_returns_false
    user = FactoryGirl.build(:user)
    refute user.authenticate('this is not the right password')
  end

  def test_user_invalid_without_password
    refute FactoryGirl.build(:user, password: '').valid?
  end

  def test_user_invalid_without_password_confirmation
    refute FactoryGirl.build(:user, password_confirmation: '').valid?
  end

  def test_emails_should_be_lowercase_after_save
    assert_equal 'uppercase@email.co.uk',
      FactoryGirl.create(:user, email: 'UPPERCASE@EMAIL.CO.UK').email,
      'Emails should be downcase'
  end

  def test_saved_user_should_have_remember_token
    user = FactoryGirl.create :user
    assert user.remember_token
  end

  def test_blank_reporter_attr_defaults_to_false
    user = FactoryGirl.create :user, reporter: nil
    assert user.reporter == false,
      "Blank user reporter attr doesn't become false"
  end

  def test_blank_uploader_attr_defaults_to_false
    user = FactoryGirl.create :user, uploader: nil
    assert user.uploader == false,
      "Blank user uploader attr doesn't become false"
  end
end
