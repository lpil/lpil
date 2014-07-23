require 'test_helper'

class SessionsHelperTest < ActionView::TestCase
  def test_sign_in_and_return_current_user
    user = FactoryGirl.create :user
    sign_in user
    assert_equal current_user, user,
      'current_user does not match user that signed in'
  end

  def test_signed_in_returns_true_after_sign_in
    user = FactoryGirl.create :user
    sign_in user
    assert signed_in?, 'signed_in? should return true after signing in'
  end

  def test_signed_in_returns_false_if_not_signed_in
    refute signed_in?, 'signed_in? should return false if not signed in'
  end
end
