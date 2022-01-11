require 'test_helper'

class CollectionsControllerTest < ActionController::TestCase
  def setup
    @collection = FactoryGirl.create :collection
    @new        = FactoryGirl.attributes_for :collection
  end

  { index:   'get :index',
    archive: 'get :archive',
    new:     'get :new',
    show:    'get :show, id: @collection.id',
    edit:    'get :edit, id: @collection.id',
    create:  'post :create, collection: @new',
    update:  "patch :update, id: @collection.id, collection: { name: 'foo' }",
    destroy: 'delete :destroy, id: @collection.id',
    restore: 'patch :restore, id: @collection.id',
    users:   'get :users, id: @collection.id'
  }.each do |action, visit_method|
    class_eval %{
      def test_user_cant_#{action}
        @request.cookies[:remember_token] =
          FactoryGirl.create(:user).remember_token
        #{visit_method}
        refute response.success?,
          '#{action} should not be successful for user'
        assert_redirected_to '/signin',
          '#{action} should redirect to signin page for user'
        assert_equal flash[:alert], 'Not enough permissions to view this page',
          "#{action} should show 'Not enough perm' alert flash for user"
      end

      def test_guest_cant_#{action}
        #{visit_method}
        refute response.success?,
          '#{action} should not be successful for guest'
        assert_redirected_to '/signin',
          '#{action} should redirect to signin page for guest'
        assert_equal flash[:alert], 'Not enough permissions to view this page',
          "#{action} should show 'Please sign in' notice flash for guest"
      end
    }
  end
end

class CollectionsControllerAsAdminTest < ActionController::TestCase
  tests CollectionsController

  def setup
    @collection = FactoryGirl.create :collection
    @new        = FactoryGirl.attributes_for :collection
    @request.cookies[:remember_token] =
      FactoryGirl.create(:admin).remember_token
  end

  def test_index_shows_unlocked_collections
    FactoryGirl.create :collection, locked: true
    get :index
    assert_equal Collection.where(locked: false), assigns[:collections],
      'Index should assign all unlocked collections to @collections'
  end

  def test_archive_shows_archived_collections
    FactoryGirl.create :collection, locked: true
    get :archive
    assert_equal Collection.where(locked: true), assigns[:collections],
      'Archive should assign all locked collections to @collections'
  end

  def test_new_assigns_new
    get :new
    assert_equal assigns[:collection].id, nil
      'New should assign new id-less Collection to @collection'
  end

  def test_show_assigns_collection_with_same_id
    get :show, id: @collection.id
    assert_equal assigns[:collection].id, @collection.id
      'Show should assign collection with the correct id'
  end

  def test_create_without_params_renders_new
    post :create, collection: { }
    refute response.success?
    assert_template :new, 'Create without params should render new again'
  end

  def test_create_without_params_shouldnt_save
    refute_difference 'Collection.count' do
      post :create, collection: { }
    end
  end

  def test_create_can_create
    assert_difference 'Collection.count', 1 do
      post :create, collection: @new
    end
  end

  def test_edit_assigns_collection_with_same_id
    get :edit, id: @collection.id
    assert_equal assigns[:collection].id, @collection.id
      'Edit should assign collectin with the correct id'
  end

  def test_update_renders_edit_when_invalid
    @collection.name = ''
    patch :update, id: @collection.id, collection: @collection.attributes
    refute response.success?
    assert_template :edit
  end

  def test_update_can_update
    new_name = 'test_update_can_update'
    @collection.name = new_name
    patch :update, id: @collection.id, collection: @collection.attributes
    assert_equal new_name, @collection.reload.name, 'Update should change name'
  end

  def test_destroy_locks_collections
    delete :destroy, id: @collection.id
    assert @collection.reload.locked, 'Destroyed collection should be locked'
  end

  def test_destroy_doesnt_delete_collection
    refute_difference 'Collection.count' do
      delete :destroy, id: @collection.id
    end
  end

  def test_restore_unlocks_collections
    @collection.update locked: true
    patch :restore, id: @collection.id
    refute @collection.reload.locked, 'Restored collection should be unlocked'
  end

  def test_users_assigns_users
    FactoryGirl.create :user, collection: @collection
    get :users, id: @collection.id
    assert_equal assigns[:users], @collection.users,
      "Users should assign the collection's users to @users"
  end
end
