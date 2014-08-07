class FactoryTest < ActiveSupport::TestCase
  %i(collection category user reporter uploader uploader_reporter admin
     ).each do |factory_type|
    class_eval %{
      def test_#{factory_type}_factory_default_valid
        assert FactoryGirl.build(:#{factory_type}).valid?,
          'Factory for #{factory_type} should build with valid attrs'
      end
    }
  end
end
