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
