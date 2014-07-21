require "test_helper"

class UserPagesTest < Capybara::Rails::TestCase
  def test_signup_page_has_signup_text
    visit signup_path
    assert page.has_selector? 'h1', 'Sign up'
  end
end
