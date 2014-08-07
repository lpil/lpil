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
