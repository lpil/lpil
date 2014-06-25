require_relative 'test_helper.rb'

# Test general application
class TestMailing < Minitest::Test
  include Rack::Test::Methods

  def app
    Sinatra::Application
  end

  def test_can_query_db_for_mailing
    Mailing.first
  end
end
