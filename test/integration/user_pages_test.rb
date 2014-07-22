require "test_helper"

class UserPagesTest < Capybara::Rails::TestCase
  def test_users_index_has_title
    visit 'users'
    assert page.has_selector? 'h1', 'Users'
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

  def test_new_user_page_has_title
    visit 'users#new'
    assert page.has_selector? 'h1', 'Create new user'
  end

  def create_new_user_via_page(user = nil)
    user ||= FactoryGirl.build :user
    visit 'users/new'
    assert page.has_content?('Create new user'), 'foo'
    fill_in 'First name', with: user.first_name
    fill_in 'Last name', with: user.last_name
    fill_in 'Email', with: user.email
    fill_in 'Password', with: user.password
    fill_in 'Password confirmation', with: user.password_confirmation
    click_button 'Create new user'

    user
  end

  def test_new_user_page_should_save_to_db
    user    = create_new_user_via_page
    db_user = User.last
    assert_equal user.email, db_user.email
    assert_equal user.first_name, db_user.first_name
    assert_equal user.last_name, db_user.last_name
  end

  def test_new_user_page_should_show_users_page
    user = create_new_user_via_page
    assert page.has_content? user[:email]
  end
end
