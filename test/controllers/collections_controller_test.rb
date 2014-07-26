require 'test_helper'

class CollectionsControllerTest < ActionController::TestCase
  test "should get index" do
    get :index
    assert_response :success
  end

  test "should get new" do
    get :new
    assert_response :success
  end

  test "should get show" do
    get :show
    assert_response :success
  end

  test "should get edit" do
    get :edit
    assert_response :success
  end

  test "should get create" do
    get :create
    assert_response :success
  end

  test "should get lock" do
    get :lock
    assert_response :success
  end

end
