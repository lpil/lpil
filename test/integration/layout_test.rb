require "test_helper"

class UserPagesTest < Capybara::Rails::TestCase
  def test_topbar_has_small_screen_menu_button
    visit user_path new_signed_in_user
    assert page.has_selector? '.toggle-topbar.menu-icon a span',
      'Topbar missing small device menu button'
  end
end
