require "test_helper"

class CategoryPagesTest < ActionDispatch::IntegrationTest
  def test_category_show_has_edit_button_for_admin
    new_signed_in_admin
    cat = FactoryGirl.create :category
    visit category_path cat
    assert page.has_selector? '.button', text: 'Edit category'
  end

  def test_category_show_does_not_have_edit_button_for_non_admin
    new_signed_in_user
    cat = FactoryGirl.create :category
    visit category_path cat
    refute page.has_selector? '.button', text: 'Edit category'
  end

  def test_can_set_parent_cat_for_new_cat
    user = new_signed_in_admin
    FactoryGirl.create :category, collection: user.collection
    mum = FactoryGirl.create :category, collection: user.collection
    FactoryGirl.create :category, collection: user.collection
    category = FactoryGirl.build :category,
      parent_id: mum.id, collection: user.collection
    visit new_category_path
    assert page.has_selector? '#category_parent_id', 'Parent select missing'
    fill_in 'Name', with: category.name
    select mum.name, from: 'Parent'
    click_on 'Save'
    child = Category.find_by name: category.name
    assert_equal child.parent, mum, 'Parent not set correctly'
  end

  def test_no_parent_select_for_root_cat
    user = new_signed_in_admin
    root = user.categories.first
    visit edit_category_path root
    refute page.has_selector? '#category_parent_id',
      'Should not be a parent select for root cat'
    assert page.has_selector?('.panel',
        text: 'This is a root category, a parent category cannot be set.'),
      'Should display explaination panel for root cat instead of select'
  end
end
