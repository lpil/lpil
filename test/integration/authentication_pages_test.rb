require 'test_helper'

class UserPagesTest < Capybara::Rails::TestCase
  def sign_in(user)
    visit signin_path
    fill_in 'Email', with: user.email
    fill_in 'Password', with: user.password
    click_button 'Sign in'
  end

  def test_signin_page_content
    visit signin_path
    assert page.has_selector?('h1', text: 'Sign in'),
      "Sign in page missing 'Sign in' h1"
    assert page.has_title?('Sign in'),
      "Sign in page missing 'Sign in' title"
  end

  def test_content_after_signin
    user = FactoryGirl.create :user
    sign_in user

    assert page.has_link?('Profile', href: user_path(user)),
      'Profile page link missing after sign in'
    assert page.has_link?('Sign out', href: signout_path),
      'Signout link missing after sign in'
    refute page.has_link?('Sign in', href: signin_path),
      "There should not be a 'Sign in' button after signing in"
  end

  def test_signin_page_should_redirect_if_signed_in
    user = FactoryGirl.create :user
    sign_in user
    visit signin_path
    refute page.has_selector?('h1', text: 'Sign in'),
      "Visiting signin_path when signed in should not show 'Sign in' h1"
  end

  def test_flash_for_signin_with_incorrect_email
    visit signin_path
    click_button 'Sign in'
    assert page.has_selector?('.alert-box.error', text: 'Unknown email'),
      "'Unknown email' flash message missing after incorrect login email"
  end

  def test_flash_for_signin_with_incorrect_password
    user = FactoryGirl.create :user
    visit signin_path
    fill_in 'Email', with: user.email
    click_button 'Sign in'
    assert page.has_selector?('.alert-box.error',
                              text: 'Incorrect password'),
      "'Incorrect password' flash message missing after incorrect password"
  end

  def test_signout_has_flash_after
    page.driver.submit :delete, signout_path, {}
    assert page.has_selector?('.alert-box.success', text: 'Signed out'),
      "Page after sign out is missing flash message"
  end

  def test_user_delete_button_does_not_show_for_admins
    user = FactoryGirl.create :user
    admin = FactoryGirl.create :admin
    sign_in admin
    visit user_path(user)
    assert page.has_selector?('a', text: 'Delete user'),
      'User page should have delete link for admin'
  end

  def test_user_delete_button_does_not_show_for_non_admins
    user = FactoryGirl.create :user
    non_admin = FactoryGirl.create :user
    sign_in non_admin
    visit user_path(user)
    refute page.has_selector?('a', text: 'Delete user'),
      'User page should not have delete link for non-admin'
  end

  def admins_can_edit_user_status_of(attribute)
    admin = FactoryGirl.create :admin
    user = FactoryGirl.create :user, attribute.downcase => false
    sign_in admin
    visit edit_user_path(user)
    check attribute
    click_button 'Save user'
    user.reload
    assert user[attribute.downcase]
  end

  def test_admins_can_edit_user_status_of_reporter
    admins_can_edit_user_status_of 'Reporter'
  end

  def test_admins_can_edit_user_status_of_admin
    admins_can_edit_user_status_of 'Admin'
  end

  def test_admins_can_edit_user_status_of_uploader
    admins_can_edit_user_status_of 'Uploader'
  end
end
