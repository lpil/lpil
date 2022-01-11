require 'test_helper'

class UsersControllerAsAdminTest < ActionController::TestCase
  tests UsersController

  def setup
    @user  = FactoryGirl.create :admin
    @other = FactoryGirl.create :user
    @new   = FactoryGirl.attributes_for :user
    @new[:collection_id] = @user.collection_id
    @request.cookies[:remember_token] = @user.remember_token
  end

  { index:   'get :index',
    new:     'get :new',
    show:    'get :new, id: @user.id',
    edit:    'get :edit, id: @user.id',
    create:  'post :create, user: @new',
    update:  "patch :update, id: @user.id, user: { first_name: 'foo' }",
    destroy: 'delete :destroy, id: @user.id',
    current: 'get :current'
  }.each do |action, visit_method|
    class_eval %{
      def test_admin_can_#{action}
        #{visit_method}
        assert ['200', '302'].include?(response.code),
          '#{action} should be successful for admin'
      end
    }
  end

  def test_index_sets_user_to_all_users
    get :index
    assert assigns[:users] == User.all,
      'get index should assign all users to the @users var'
  end

  def test_create_should_create_user
    assert_difference 'User.count', 1 do
      post :create, user: @new
    end
  end

  def test_destroy_should_archive_user
    assert_difference 'User.count', -1 do
      delete :destroy, id: @other.id
    end
  end
end

class UsersControllerAsUserTest < ActionController::TestCase
  tests UsersController
  
  def setup
    @user  = FactoryGirl.create :user
    @other = FactoryGirl.create :user
    @new   = FactoryGirl.attributes_for :user
    @new[:collection_id] = @user.collection_id
    @request.cookies[:remember_token] = @user.remember_token
  end

  { index:   'get :index',
    new:     'get :new',
    show:    'get :show, id: @other.id',
    edit:    'get :edit, id: @other.id',
    create:  'post :create, user: @new',
    update:  "patch :update, id: @other.id, user: { first_name: 'foo' }",
    destroy: 'delete :destroy, id: @user.id',
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
    assert_redirected_to user_path(@user), 'Current should be successful'
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
