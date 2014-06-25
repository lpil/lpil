require_relative 'test_helper.rb'

# Test general application
class TestApp < Minitest::Test
  include Rack::Test::Methods

  def app
    Sinatra::Application
  end

  def test_get_root
    get '/'
    assert last_response.ok?
    assert_match(/The time at the server is .+/, last_response.body)
  end

  def test_get_status
    get '/status'
    assert last_response.ok?
    assert_match(/Threads/, last_response.body)
  end

  def test_mailing_thread_alive?
    assert $threads[:mailings].alive?
  end
end
