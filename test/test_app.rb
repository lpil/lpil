require_relative 'test_helper.rb'

# Test general application
class TestApp < Minitest::Test
  include Rack::Test::Methods

  def app
    Sinatra::Application
  end

  def test_settings_env_is_test
    assert_equal 'test', app.settings.environment
  end

  def test_rack_env_is_test
    assert_equal 'test', ENV['RACK_ENV']
  end

  def test_get_root_ok?
    get '/'
    assert last_response.ok?
  end

  def test_get_root_shows_time
    get '/'
    assert_match(/The time at the server is .+/, last_response.body)
  end

  def test_get_status_ok?
    get '/status'
    assert last_response.ok?
  end

  def test_status_mentions_all_threads
    get '/status'
    $threads.keys.each do |name|
      assert_match(/#{name}/, last_response.body)
    end
  end

  def test_status_displays_sinatra_enviroment
    get '/status'
    assert_match(/Sinatra::Application Enviroment => test/,
                 last_response.body)
  end

  def test_fetch_dpd_reports_thread_alive?
    assert $threads[:fetch_dpd_reports].alive?
  end

  def test_fetch_dpd_reports_thread_alive?
    assert $threads[:delete_old_mailings].alive?
  end

  def test_d_dirs_end_with_slash
    $d.values.each do |dir|
      assert_match(/\/\z/, dir)
    end
  end
end
