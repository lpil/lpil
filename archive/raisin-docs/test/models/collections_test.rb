require 'test_helper'

class CollectionTest < ActiveSupport::TestCase

  %i(name locked created_at updated_at categories users).each do |field|
    class_eval %{
      def test_collection_responds_to_#{field}
          assert Collection.new.respond_to?(:#{field}),
            'Collections should respond to #{field}'
      end
    }
  end

end

class PrecreatedCollectionTest < ActiveSupport::TestCase
  def setup
    @collection = FactoryGirl.create :collection
  end

  def test_these_tests_use_already_existing_records
    refute @collection.new_record?, 'These tests should be using saved records'
  end

  %w(name).each do |field|
    class_eval %{
      def test_existing_collection_should_be_invalid_without_#{field}
        @collection.#{field} = nil
        refute @collection.valid?,
          'New collection without #{field} should not be valid'
      end
    }
  end

  def test_dont_allow_duplicate_names
    collection = FactoryGirl.build(:collection, name: @collection.name)
    refute collection.valid?,
      'Collection using existing name should not be valid'
  end

  def test_validation_for_dupe_collection_emails_should_be_case_insensitive
    collection = FactoryGirl.build(:collection, name: @collection.name.upcase)
    refute collection.valid?,
      "Dupe Collection name shouldn't be valid, even if case is different"
  end

  def test_new_collection_has_root_category
    assert_equal @collection.categories.size, 1,
      'New collection should have a root category'
  end

  def test_new_collection_root_category_has_same_name_as_collection
    assert_equal @collection.categories.first.name, @collection.name
      'New collection should have a root category with same name'
  end
end

class PrebuiltCollectionTest < ActiveSupport::TestCase
  def setup
    @collection = FactoryGirl.build :collection
  end

  def test_these_tests_use_new_records
    assert @collection.new_record?,
      'These tests should be using unsaved records'
  end

  %w(name).each do |field|
    class_eval %{
      def test_new_collection_should_be_invalid_without_#{field}
        @collection.#{field} = nil
        refute @collection.valid?,
          'New collection without #{field} should not be valid'
      end
    }
  end
end
