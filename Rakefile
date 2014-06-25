require 'sinatra/activerecord/rake'
require 'sinatra/activerecord'

# Run all tests
task :test do
  ENV['RACK_ENV'] = 'test'

  Rake::Task['db:drop'].invoke
  Rake::Task['db:create'].invoke
  Rake::Task['db:schema:load'].invoke

  require './app.rb'
  Dir.glob('test/**/test_*.rb').each { |file| require file }
end
