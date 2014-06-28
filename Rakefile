require 'sinatra/activerecord/rake'
require 'sinatra/activerecord'

# Run all tests
task :test do
  ENV['RACK_ENV'] = 'test'

  # These tasks are invoked rather than specified as dependancies as we need
  # them to inherit the test enviroment. Is there a better way to do this?
  Rake::Task['db:drop'].invoke
  Rake::Task['db:create'].invoke
  Rake::Task['db:schema:load'].invoke

  require './app.rb'
  Dir.glob('test/**/test_*.rb').each { |file| require file }
end
