require 'test_helper'

class UsersControllerTest < ActionController::TestCase
  def test_should_get_new
    get :new
    assert_response :success
  end
end
