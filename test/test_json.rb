require_relative 'test_helper.rb'

# Test JSON output features
class TestJSON < Minitest::Test
  include Rack::Test::Methods

  def app
    Sinatra::Application
  end

  def test_jsonp_callback_string_format
    get '/delivery.json?order_ref=foo&callback=bar'
    assert_match(/\Abar(.*);\z/, last_response.body)
  end

  def test_jsonp_callback_content_type
    get '/delivery.json?order_ref=foo&callback=bar'
    assert_equal last_response.original_headers['Content-Type'],
                 'application/javascript;charset=utf-8'
  end
end
