require 'test_helper'

class CollectionPagesTest < ActionDispatch::IntegrationTest
  def test_when_logged_out_site_title_is_static
    visit '/'
    assert page.has_title?('Raisin Docs'),
      'Site title is not Raisin Docs'
  end

  def test_when_logged_out_site_topbar_title_is_static
    visit '/'
    assert page.has_selector?('.top-bar .name', text: 'Raisin Docs'),
      'Site topbar title is not Raisin Docs'
  end

  def test_when_logged_in_site_title_is_collection_name
    user = new_signed_in_user
    visit '/'
    assert page.has_title?(user.collection.name),
      'Site title is not Raisin Docs'
  end

  def test_when_logged_out_site_topbar_title_is_static
    user = new_signed_in_user
    visit '/'
    assert page.has_selector?('.top-bar .name', text: user.collection.name),
      'Site topbar title is not Raisin Docs'
  end

  def test_admin_topbar_has_link_to_collections
    new_signed_in_admin
    visit '/'
    assert page.has_selector?('.top-bar .dropdown a', text: 'Collections'),
      "Admin's top-bar does not have a link to collections"
  end

  def test_admin_can_create_collection
    new_signed_in_admin
    col = FactoryGirl.build :collection
    visit new_collection_path
    fill_in 'Name', with: col.name
    assert_difference 'Collection.count', +1 do
      click_on 'Save'
    end
  end

  def test_admin_can_archive_collection
    new_signed_in_admin
    col = FactoryGirl.create :collection
    visit edit_collection_path col
    click_on 'Archive collection'
    assert col.reload.locked
  end

  def test_admin_can_restore_collection
    new_signed_in_admin
    col = FactoryGirl.create :collection, locked: true
    visit edit_collection_path col
    click_on 'Restore collection'
    refute col.reload.locked
  end

  def test_admin_can_rename_collection
    new_signed_in_admin
    col = FactoryGirl.create :collection
    visit edit_collection_path col
    new_name = col.name + 'test_admin_can_rename_collection'
    fill_in 'Name', with: new_name
    click_on 'Save'
    assert_equal col.reload.name, new_name, 'Collection not renamed'
  end

  def test_admin_cant_rename_collection_with_name_already_in_use
    new_signed_in_admin
    col_a = FactoryGirl.create :collection
    col_b = FactoryGirl.create :collection
    visit edit_collection_path col_b
    fill_in 'Name', with: col_a.name
    click_on 'Save'
    assert page.has_content? 'Name has already been taken'
    refute_equal col_b.reload.name, col_a.name,
      'Collection should not be able to be renamed with a name already in user'
  end

  def test_admin_cant_create_collection_with_name_in_use
    new_signed_in_admin
    col = FactoryGirl.create :collection
    visit new_collection_path
    fill_in 'Name', with: col.name
    refute_difference 'Collection.count' do
      click_on 'Save'
    end
    assert page.has_content? 'Name has already been taken'
  end

  def test_collection_index_contains_collections
    new_signed_in_admin
    col = FactoryGirl.create :collection
    visit collections_path
    assert page.has_content?(col.name),
      'Collection name missing from collection index'
  end

  def test_admin_can_view_achived_collections
    new_signed_in_admin
    col = FactoryGirl.create :collection
    col.locked = true
    visit collections_path
    click_on 'View archived collections'
    assert page.has_content?(col.name),
      'Archived collections page does not have collection name'
  end
end
