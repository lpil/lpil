require 'sinatra/activerecord/rake'

# Run all tests
task :test do
  ENV['RACK_ENV'] = 'test'
  require './app.rb'
  Dir.glob('test/**/test_*.rb').each { |file| require file }
end
