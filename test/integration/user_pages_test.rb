require "test_helper"

class UserPagesTest < ActionDispatch::IntegrationTest
  def test_users_index_has_h1
    visit '/users'
    assert page.has_selector? 'h1', text: 'Users'
  end

  def test_users_index_shows_users
    emails = []
    3.times do
      emails << FactoryGirl.create(:user).email
    end
    visit '/users'
    emails.each do |email|
      assert page.has_content? email
    end
  end

  def test_new_user_page_has_correct_content
    new_signed_in_admin
    visit new_user_path
    assert page.has_selector? 'h1', text: 'Create new user'
    %w(Email First Last Password Confirmation Reporter Uploader Admin
    ).each do |label|
      assert page.has_selector?('label', text: label),
        "New user page should have label '#{label}'"
    end
  end

  def create_new_user_via_page(user = nil)
    sign_in FactoryGirl.create :admin
    user ||= FactoryGirl.build :user
    visit '/users/new'
    fill_in 'First name', with: user.first_name
    fill_in 'Last name', with: user.last_name
    fill_in 'Email', with: user.email
    fill_in 'Password', with: user.password
    fill_in 'Confirmation', with: user.password_confirmation
    click_button 'Save user'

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
      '.alert-box.success', text: 'New user successfully created'),
      'Success message missing'
  end

  def test_flash_message_should_show_error_after_failed_new_user_creation
    create_new_user_via_page FactoryGirl.build :user, password: 'no'
    refute page.has_selector?('div.alert-box.error'),
      'Error message missing'
  end

  def test_user_edit_path_has_correct_content
    new_signed_in_admin
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
    new_signed_in_admin
    user = FactoryGirl.create :user
    visit user_path user
    assert page.has_selector?('a', text: 'Edit'),
      'Edit button missing from user page for admin viewer'
  end

  def test_edit_button_on_user_page_for_same_user
    user = new_signed_in_user
    visit user_path user
    assert page.has_selector?('a', text: 'Edit'),
      'Edit button missing from user page for same user viewer'
  end

  def test_no_edit_button_on_user_page_for_non_same_regular_user
    new_signed_in_user
    other_user = FactoryGirl.create :user
    visit user_path other_user
    refute page.has_selector?('a', text: 'Edit'),
      'Edit button should not be present on user page different user'
  end

  def test_delete_button_on_user_page_for_admin
    new_signed_in_admin
    user = FactoryGirl.create :user
    visit user_path user
    assert page.has_selector?('a', text: 'Delete'),
      'Delete buttons should be present on user list for admin'
  end

  def test_edit_buttons_on_user_index_for_admin
    FactoryGirl.create :user
    new_signed_in_admin
    visit users_path
    assert page.has_selector?('a', text: 'Edit'),
      'Edit buttons should be present on user index for admins'
  end

  def test_no_delete_button_on_user_page_for_same_admin
    admin = new_signed_in_admin
    visit user_path admin
    refute page.has_selector?('a', text: 'Delete'),
      "Delete button should not be present on admin's own page"
  end

  def test_admin_can_update_users
    new_signed_in_admin
    user = FactoryGirl.create :user
    visit edit_user_path user
    fill_in 'First name', with: 'Thomas'
    click_button 'Save user'
    assert page.has_selector?('.alert-box',
                              "#{user.email} successfully edited"),
    'Page after successful edit should have success alert message'
  end

  def test_failed_user_edit_shows_error_message
    new_signed_in_admin
    user = FactoryGirl.create :user
    visit edit_user_path user
    fill_in 'Email', with: ''
    fill_in 'Password', with: 'no'
    fill_in 'Confirmation', with: "something that doesn't match"
    click_button 'Save user'
    assert page.has_selector?('li', 't be blank'),
      "Email can't be blank alert is missing"
    assert page.has_selector?('li', 'Email is invalid'),
      'Email is invalid alert is missing'
    assert page.has_selector?('li', 'too short'),
      'Password is too short alert is missing'
    assert page.has_selector?('li', 'Password confirmation doesn'),
      "Confirmation doesn't match alert is missing"
  end
end
