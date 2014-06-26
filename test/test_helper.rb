require 'minitest/autorun'
require 'minitest/pride'
require 'rack/test'
require 'factory_girl'

require_relative '../app.rb'

Sinatra::Application.environment = 'test'

# Include the FactoryGirl methods in the Minitest test object
class Minitest::Test
  include FactoryGirl::Syntax::Methods
end

FactoryGirl.find_definitions
