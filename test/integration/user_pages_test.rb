require "test_helper"

class UserPagesTest < Capybara::Rails::TestCase
  def test_users_index_has_h1
    visit 'users'
    assert page.has_selector? 'h1', text: 'Users'
  end

  def test_users_index_shows_users
    emails = []
    3.times do
      emails << FactoryGirl.create(:user).email
    end
    visit 'users'
    emails.each do |email|
      assert page.has_content? email
    end
  end

  def test_new_user_page_has_correct_content
    admin = FactoryGirl.create :admin
    sign_in admin
    visit new_user_path
    assert page.has_selector? 'h1', text: 'Create new user'
    %w(Email First Last Password Confirmation Reporter Uploader Admin
    ).each do |label|
      assert page.has_selector?('label', text: label),
        "New user page should have label '#{label}'"
    end
  end

  def create_new_user_via_page(user = nil)
    user ||= FactoryGirl.build :user
    visit 'users/new'
    fill_in 'First name', with: user.first_name
    fill_in 'Last name', with: user.last_name
    fill_in 'Email', with: user.email
    fill_in 'Password', with: user.password
    fill_in 'Confirmation', with: user.password_confirmation
    click_button 'Create new user'

    user
  end

  def test_failed_user_creation_should_repopulate_fields
    user = FactoryGirl.build :user, password: 'no'
    create_new_user_via_page user
    assert page.has_selector?('#user_first_name', visible: user.first_name),
      'First name field not repopulated upon incorrect submission'
    assert page.has_selector?('#user_last_name', visible: user.last_name),
      'Last name field not repopulated upon incorrect submission'
    assert page.has_selector?('#user_password', visible: user.password),
      'Password name field not repopulated upon incorrect submission'
    assert page.has_selector?('#user_password_confirmation',
                              visible: user.password_confirmation),
      'Password confirmation field not repopulated upon incorrect submission'
  end

  def test_new_user_page_should_save_to_db
    user    = create_new_user_via_page
    db_user = User.last
    assert_equal user.email, db_user.email
    assert_equal user.first_name, db_user.first_name
    assert_equal user.last_name, db_user.last_name
  end

  def test_after_new_user_page_should_show_that_users_page
    user = create_new_user_via_page
    assert page.has_content? user[:email]
  end

  def test_after_new_user_page_should_not_show_all_users_page
    create_new_user_via_page
    refute page.has_selector?('h1', text: 'Users'),
      'Page after new user create has the Users header'
  end

  def test_flash_message_should_show_after_new_user_creation
    create_new_user_via_page
    assert page.has_selector?(
      'div.alert.alert-success', text: 'New user successfully created'),
      'Success message missing'
  end

  def test_flash_message_should_show_error_after_failed_new_user_creation
    create_new_user_via_page FactoryGirl.build :user, password: 'no'
    refute page.has_selector?('div.alert.alert-error'),
      'Error message missing'
  end

  def test_user_edit_path_has_correct_content
    admin = FactoryGirl.create :admin
    sign_in admin
    user = FactoryGirl.create :user
    visit edit_user_path user
    %w(Email First Last Password Confirmation Uploader Reporter Admin
    ).each do |label|
      assert page.has_selector?('label', text: label),
        "Edit user page should have label '#{label}'"
    end
    assert page.has_title?("Edit #{user.email}"),
      "Title should include 'Edit #{user.email}'"
    assert page.has_selector?('h1', text: "Edit #{user.email}"),
      "A h1 should include 'Edit #{user.email}'"
  end

  def test_edit_button_on_user_page_for_admin
    admin = FactoryGirl.create :admin
    sign_in admin
    user = FactoryGirl.create :user
    visit user_path user
    assert page.has_selector?('a', text: 'Edit'),
      'Edit button missing from user page for admin viewer'
  end

  def test_edit_button_on_user_page_for_same_user
    user = FactoryGirl.create :user
    sign_in user
    visit user_path user
    assert page.has_selector?('a', text: 'Edit'),
      'Edit button missing from user page for same user viewer'
  end

  def test_no_edit_button_on_user_page_for_non_same_regular_user
    user = FactoryGirl.create :user
    sign_in user
    other_user = FactoryGirl.create :user
    visit user_path other_user
    refute page.has_selector?('a', text: 'Edit'),
      'Edit button should not be present on user page different user'
  end

  def test_delete_button_on_user_page_for_admin
    admin = FactoryGirl.create :admin
    sign_in admin
    user = FactoryGirl.create :user
    visit user_path user
    assert page.has_selector?('a', text: 'Delete'),
      'Delete buttons should be present on user list for admin'
  end

  def test_edit_buttons_on_user_index_for_admin
    admin = FactoryGirl.create :admin
    sign_in admin
    visit users_path
    assert page.has_selector?('a', text: 'Edit'),
      'Delete button should be present on user page for admin'
  end

  def test_no_delete_button_on_user_page_for_same_admin
    admin = FactoryGirl.create :admin
    sign_in admin
    visit user_path admin
    refute page.has_selector?('a', text: 'Delete'),
      "Delete button should not be present on admin's own page"
  end
end
