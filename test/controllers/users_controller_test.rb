require 'test_helper'

class UsersControllerTest < ActionController::TestCase
  test "should get new" do
    get :new
    assert_response :success
  end
end

class MiniTest::Unit::TestCase
  include FactoryGirl::Syntax::Methods
end
