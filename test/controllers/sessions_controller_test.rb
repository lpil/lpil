require 'test_helper'

class SessionsControllerTest < ActionController::TestCase
  def setup
    @user = FactoryGirl.create :user
  end

  def test_guest_can_get_new
    get :new
    assert response.success?, 'Guest should be able to get new'
  end

  def test_signed_in_user_is_redirected_when_get_new
    @request.cookies[:remember_token] = @user.remember_token
    get :new
    assert_redirected_to categories_path,
      'Signed in users should be redirected when get new'
  end

  def test_can_sign_in
    post :create, session: { email: @user.email, password: @user.password }
    assert_equal @user.remember_token, cookies[:remember_token],
      'Signing in should add remember_token cookie'
  end

  def test_can_sign_out
    @request.cookies[:remember_token] = @user.remember_token
    delete :destroy
    assert_nil cookies[:remember_token],
      'Signing out should remove remember token cookie'
  end
end
