require './app.rb'
require 'sinatra/activerecord/rake'
require 'rake/testtask'

task default: :test

Rake::TestTask.new do |t|
  Rake::Task['db:drop'].invoke
  Rake::Task['db:create'].invoke
  Rake::Task['db:schema:load'].invoke
  t.pattern = 'test/test_*.rb'
end
