require 'test_helper'

class UsersControllerAsAdminTest < ActionController::TestCase
  tests UsersController

  def setup
    @user  = FactoryGirl.create :admin
    @other = FactoryGirl.create :user
    @request.cookies[:remember_token] = @user.remember_token
  end

  def test_write_some_damn_admin_tests
    fail NotImplementedError
  end
end

  # test "should get index" do
  #   get :index
  #   assert_response :success
  #   assert_not_nil assigns(:things)
  # end

  # test "should get new" do
  #   get :new
  #   assert_response :success
  # end

  # test "should create thing" do
  #   assert_difference('Thing.count') do
  #     post :create, thing: {  }
  #   end

  #   assert_redirected_to thing_path(assigns(:thing))
  # end

  # test "should show thing" do
  #   get :show, id: @thing
  #   assert_response :success
  # end

  # test "should get edit" do
  #   get :edit, id: @thing
  #   assert_response :success
  # end

  # test "should update thing" do
  #   patch :update, id: @thing, thing: {  }
  #   assert_redirected_to thing_path(assigns(:thing))
  # end

  # test "should destroy thing" do
  #   assert_difference('Thing.count', -1) do
  #     delete :destroy, id: @thing
  #   end

  #   assert_redirected_to things_path
  # end

class UsersControllerAsUserTest < ActionController::TestCase
  tests UsersController
  
  def setup
    @user  = FactoryGirl.create :user
    @other = FactoryGirl.create :user
    @request.cookies[:remember_token] = @user.remember_token
  end

  { index:   'get :index',
    new:     'get :new',
    show:    'get :show, { id: @other.id }',
    edit:    'get :edit, { id: @other.id }',
    create:  'post :create, { user: FactoryGirl.attributes_for(:user) }',
    update:  "patch :update, { id: @other.id, user: { first_name: 'foo' } }",
    destroy: 'delete :destroy, { id: @user.id }',
  }.each do |action, visit_method|
    class_eval %{
      def test_user_cant_#{action}
        #{visit_method}
        refute response.success?,
          '#{action} should not be successful for user'
        assert_redirected_to '/signin',
          '#{action} should redirect to signin page for user'
        assert_equal flash[:alert], 'Not enough permissions to view this page',
          "#{action} should show 'Not enough perm' alert flash for user"
      end
    }
  end

  def test_user_can_show_self
    get :show, { id: @user.id }
    assert response.success?, 'Show self should be successful'
  end

  def test_user_can_edit_self
    get :edit, { id: @user.id }
    assert response.success?, 'Show self should be successful'
  end

  def test_user_can_update_self
    patch :update, { id: @user.id, user: { first_name: 'Foo' } }
    assert_equal 'Foo', User.find_by(id: @user.id).first_name,
      'User self update should update DB'
  end

  def test_user_can_current
    get :current
    assert_redirected_to "/users/#{@user.id}",
      'Current should redirect to signed in user'
  end
end

class UsersControllerAsGuestTest < ActionController::TestCase
  tests UsersController
  
  def setup
    @user = FactoryGirl.create :user
  end

  { index:   'get :index',
    new:     'get :new',
    show:    'get :new, { id: @user.id }',
    edit:    'get :edit, { id: @user.id }',
    create:  'post :create, { user: FactoryGirl.attributes_for(:user) }',
    update:  "patch :update, { id: @user.id, user: { first_name: 'foo' } }",
    destroy: 'delete :destroy, { id: @user.id }',
    current: 'get :current'
  }.each do |action, visit_method|
    class_eval %{
      def test_guest_cant_#{action}
        #{visit_method}
        refute response.success?,
          '#{action} should not be successful for guest'
        assert_redirected_to '/signin',
          '#{action} should redirect to signin page for guest'
        assert_equal flash[:notice], 'Please sign in',
          "#{action} should show 'Please sign in' notice flash for guest"
      end
    }
  end
end
