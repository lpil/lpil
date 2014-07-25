require 'test_helper'

class AuthorisationTest < ActionDispatch::IntegrationTest
  # This is the blacklist. Add any authenticated pages to this hash, and then
  # add the key to the whitelist array for the users who should be able to
  # access that page.
  all_disallowed_paths = {
    # User pages
    all_users_page: "visit '/users'",
    new_user_page:  "visit '/users/new'",
    user_view_page: 'visit user_path FactoryGirl.create :user',
    user_edit_page: 'visit edit_user_path FactoryGirl.create :user'
  }

  user_allowed_paths = [
  ]

  reporter_allowed_paths = [
  ]

  uploader_allowed_paths = [
  ]

  admin_allowed_paths = [
    # Users pages
    :all_users_page, :new_user_page, :user_view_page, :user_edit_page
  ]

  #
  # Here be meta-programming...
  #

  admin_disallowed_paths =
    all_disallowed_paths.except(*admin_allowed_paths)

  uploader_disallowed_paths =
    all_disallowed_paths.except(*uploader_allowed_paths)

  reporter_disallowed_paths =
    all_disallowed_paths.except(*reporter_allowed_paths)

  user_disallowed_paths =
    all_disallowed_paths.except(*user_allowed_paths)

  ensure_path_inaccessable = ->(visitor, path_name, path_setup) do
    class_eval %{
def test_#{visitor}_cant_visit_#{path_name}
  #{path_setup}
  assert page.has_selector?('h1', text: 'Sign in'),
    'Did not redirect to sign in page. Sign in h1 missing'
  assert page.has_selector?('input', visible: 'Sign in'),
    'Did not redirect to sign in page. Sign in button missing'
  assert page.has_selector?('label', text: 'Email'),
    'Did not redirect to sign in page. Email label missing'
  assert page.has_selector?('label', text: 'Password'),
    'Did not redirect to sign in page. Password label missing'
end}
  end

  # Admin visitor
  admin_disallowed_paths.each do |path_name, path_setup|
    path_setup = "sign_in FactoryGirl.create :admin\n" << path_setup
    ensure_path_inaccessable.call 'admin', path_name, path_setup
  end

  # Reporter visitor
  reporter_disallowed_paths.each do |path_name, path_setup|
    path_setup = "sign_in FactoryGirl.create :reporter\n" << path_setup
    ensure_path_inaccessable.call 'reporter', path_name, path_setup
  end

  # Uploader visitor
  uploader_disallowed_paths.each do |path_name, path_setup|
    path_setup = "sign_in FactoryGirl.create :uploader\n" << path_setup
    ensure_path_inaccessable.call 'uploader', path_name, path_setup
  end

  # Normal user visitor
  user_disallowed_paths.each do |path_name, path_setup|
    path_setup = "sign_in FactoryGirl.create :user\n" << path_setup
    ensure_path_inaccessable.call 'user', path_name, path_setup
  end

  # Guest visitor
  all_disallowed_paths.each do |path_name, path_setup|
    ensure_path_inaccessable.call 'guest', path_name, path_setup
  end

  def test_whatever
  end
end
