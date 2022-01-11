require 'test_helper'

class CategoryTest < ActiveSupport::TestCase

  %i(name locked created_at updated_at ancestry collection users
    ).each do |field|
    class_eval %{
      def test_category_responds_to_#{field}
          assert Category.new.respond_to?(:#{field}),
            'Categories should respond to #{field}'
      end
    }
  end
end

class PrecreatedCategoryTest < ActiveSupport::TestCase
  def setup
    @category = FactoryGirl.create :category
  end

  def test_these_tests_use_already_existing_records
    refute @category.new_record?, 'These tests should be using saved records'
  end

  %w(name collection).each do |field|
    class_eval %{
      def test_exiting_category_should_be_invalid_without_#{field}
        @category.#{field} = nil
        refute @category.valid?,
          'New category without #{field} should not be valid'
      end
    }
  end
end

class PrebuiltCategoryTest < ActiveSupport::TestCase
  def setup
    @category = FactoryGirl.build :category
  end

  def test_these_tests_use_new_records
    assert @category.new_record?, 'These tests should be using unsaved records'
  end

  %w(name collection).each do |field|
    class_eval %{
      def test_new_category_should_be_invalid_without_#{field}
        @category.#{field} = nil
        refute @category.valid?,
          'New category without #{field} should not be valid'
      end
    }
  end
end
