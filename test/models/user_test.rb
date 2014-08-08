require 'test_helper'

class UserTest < ActiveSupport::TestCase

  %w(first_name last_name email reporter uploader password created_at
     updated_at password_confirmation remember_token collection_id authenticate
     remember_token
     ).each do |field|
    class_eval %{
      def test_user_responds_to_#{field}
          assert User.new.respond_to?(:#{field}),
            'Users should respond to #{field}'
      end
    }
  end
end

class PrecreatedUserTest < ActiveSupport::TestCase
  def setup
    @user = FactoryGirl.create :user
  end

  def test_these_tests_use_already_existing_records
    refute @user.new_record?, 'These tests should be using saved records'
  end

  %w(email collection).each do |field|
    class_eval %{
      def test_existing_user_should_be_invalid_without_#{field}
        @user.#{field} = nil
        refute @user.valid?, 'New user without #{field} should not be valid'
      end
    }
  end

  def test_these_valid_user_email_are_valid
    %w(user@foo.COM A_US-ER@f.b.org first.last@foo.jp a+b@baz.uk
      ).each do |email|
      @user.email = email
      assert @user.valid?, "User with email #{email} should be valid"
    end
  end

  def test_these_invalid_user_email_are_not_valid
    %w(user@foo,com user_at_foo.org example.user@foo. foo@bar_baz.com
       foo@bar+baz.com).each do |email|
      @user.email = email
      refute @user.valid?, "User with email #{email} shouldn't be valid"
    end
  end

  def test_dont_allow_duplicate_emails
    user = FactoryGirl.build(:user, email: @user.email)
    refute user.valid?, 'User using existing email should not be valid'
  end

  def test_validation_for_dupe_user_emails_should_be_case_insensitive
    user = FactoryGirl.build(:user, email: @user.email.upcase)
    refute user.valid?,
      "User using existing email shouldn't be valid, even if case is different"
  end

  def test_valid_user_authenticate_returns_user
    assert_equal @user, @user.authenticate(@user.password),
      'A successful user.authenticate call should return the user'
  end

  def test_invalid_user_authenticate_returns_false
    refute @user.authenticate('this is not the right password'),
      'An unsuccessful user.authenticate call should return false'
  end

  def test_users_can_be_updated_without_password_given
    assert @user.update(first_name: 'John'),
      'User should be able to be updated without updating the password'
  end
end

class PrebuiltUserTest < ActiveSupport::TestCase
  def setup
    @user = FactoryGirl.build :user
  end

  def test_these_tests_use_new_records
    assert @user.new_record?, 'These tests should be using unsaved records'
  end

  %w(password password_confirmation email collection).each do |field|
    class_eval %{
      def test_new_user_should_be_invalid_without_#{field}
        @user.#{field} = nil
        refute @user.valid?, 'New user without #{field} should not be valid'
      end
    }
  end

  { uploader: 'nil', reporter: 'nil', admin: 'nil' }.each do |field, default|
    class_eval %{
      def test_new_user_blank_#{field}_field_defaults_to_#{default}
        @user.#{field} = nil
        @user.save!
        assert @user.#{field} == #{default},
          'Blank new user #{field} field should default to #{default} on save'
      end
    }
  end

  def test_passwords_must_be_longish
    min_length = 8
    @user.password = @user.password_confirmation = 'a' * (min_length - 1)
    refute @user.valid?, 'passwords must be longish'
    @user.password = @user.password_confirmation = 'a' * min_length
    assert @user.valid?, 'passwords must be longish'
  end

  def test_emails_should_be_lowercase_after_save
    email = 'uppercase@email.co.uk'
    @user.email = email.upcase
    @user.save!
    assert_equal email, @user.reload.email,
      'Emails of saved users should be downcase'
  end
end
