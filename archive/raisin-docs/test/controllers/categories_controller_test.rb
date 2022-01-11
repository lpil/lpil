require 'test_helper'

class CategoriesControllerAsAdminTest < ActionController::TestCase
  tests CategoriesController
  
  def setup
    @cat  = FactoryGirl.create :category
    @new  = FactoryGirl.attributes_for :category
    @user = FactoryGirl.create :admin
    @request.cookies[:remember_token] = @user.remember_token
  end

  def test_new_assigns_new
    get :new
    assert_equal assigns[:category].id, nil
      'New should assign new id-less Collection to @collection'
  end

  def test_show_assigns_category_with_same_id
    get :show, id: @cat.id
    assert_equal assigns[:category].id, @cat.id
      'Show should assign collection with the correct id'
  end

  def test_create_without_params_renders_new
    post :create, category: { }
    refute response.success?
    assert_template :new, 'Create without params should render new again'
  end

  def test_create_without_params_shouldnt_save
    refute_difference 'Category.count' do
      post :create, category: { }
    end
  end

  def test_create_without_parent_id_shouldnt_save
    refute_difference 'Category.count' do
      post :create, category: @new
    end
  end

  def test_create_can_create
    assert_difference 'Category.count', 1 do
      @new[:parent_id] = @user.categories.first
      post :create, category: @new
    end
  end

  def test_update_can_update
    new_name = 'test_update_can_update'
    @cat.name = new_name
    patch :update, id: @cat.id, category: @cat.attributes
    assert_equal @cat.reload.name, new_name, 'Update should update name'
  end

  def test_update_doesnt_let_non_root_categories_become_root
    attrs = FactoryGirl.attributes_for :category
    attrs[:collection] = @cat.collection
    @child = @cat.children.create! attrs
    @child = @child.attributes
    @child[:parent_id] = ''
    patch :update, id: @child['id'], category: @child
    assert_equal flash[:alert], 'Non-root categories cannot become root!'
    refute_equal Category.find(@child['id']).ancestry, nil,
      'Child category ancestry should not be removable.'
  end
  
  def test_destroy_locks_categories
    delete :destroy, id: @cat.id
    assert @cat.reload.locked, 'Destroyed category should be locked'
  end

  def test_destroy_doesnt_delete_category
    refute_difference 'Category.count' do
      delete :destroy, id: @cat.id
    end
  end

  def test_restore_unlocks_categories
    @cat.update locked: true
    patch :restore, id: @cat.id
    refute @cat.reload.locked, 'Restored category should be unlocked'
  end

  def test_list_shows_correct_categories
    cat = FactoryGirl.attributes_for :category
    cat['collection_id'] = @user.collection.id
    cat['parent_id'] = @cat.id
    Category.create cat
    cat = FactoryGirl.attributes_for :category
    cat['collection_id'] = @user.collection.id
    cat['parent_id'] = @cat.id
    cat['locked'] = true
    Category.create cat
    get :list
    assert_equal assigns[:categories], @user.categories.where(locked: false),
      'List should show unlocked categories of same collection as user'
  end

  def test_archive_shows_correct_categories
    cat = FactoryGirl.attributes_for :category
    cat['collection_id'] = @user.collection.id
    cat['parent_id'] = @cat.id
    Category.create cat
    cat = FactoryGirl.attributes_for :category
    cat['collection_id'] = @user.collection.id
    cat['parent_id'] = @cat.id
    cat['locked'] = true
    Category.create cat
    get :archive
    assert_equal assigns[:categories], @user.categories.where(locked: true),
      'Archive should show locked categories of same collection as user'
  end
end

class CategoriesControllerAsUserTest < ActionController::TestCase
  tests CategoriesController
  
  def setup
    @cat  = FactoryGirl.create :category
    @new  = FactoryGirl.attributes_for :category
    @user = FactoryGirl.create :user
    @request.cookies[:remember_token] = @user.remember_token
  end

  { new:     'get :new',
    list:    'get :list',
    archive: 'get :archive',
    edit:    'get :edit, { id: @cat.id }',
    create:  'post :create, { cat: @new }',
    update:  "patch :update, { id: @cat.id, category: { first_name: 'foo' } }",
    destroy: 'delete :destroy, { id: @cat.id }',
    restore: 'patch :restore, { id: @cat.id }',
  }.each do |action, visit_method|
    class_eval %{
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

  def test_user_can_index
    get :index
    assert response.success?
  end

  def test_user_can_show
    get :index, id: @cat.id
    assert response.success?
  end
end

class CategoriesControllerAsGuestTest < ActionController::TestCase
  tests CategoriesController
  
  def setup
    @cat = FactoryGirl.create :category
    @new = FactoryGirl.attributes_for :category
  end

  { index:   'get :index',
    new:     'get :new',
    show:    'get :new, { id: @cat.id }',
    list:    'get :list',
    archive: 'get :archive',
    edit:    'get :edit, { id: @cat.id }',
    create:  'post :create, { cat: @new }',
    update:  "patch :update, { id: @cat.id, category: { first_name: 'foo' } }",
    destroy: 'delete :destroy, { id: @cat.id }',
    restore: 'patch :restore, { id: @cat.id }',
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
