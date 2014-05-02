#!/usr/bin/env ruby
# encoding: utf-8

require 'sinatra'
require 'sinatra/activerecord'
require 'sinatra/reloader' if development?

# Add out application's lib dir to the require LOAD_PATH
lib_path = File.expand_path('lib', File.dirname(__FILE__))
$LOAD_PATH.unshift lib_path unless $LOAD_PATH.include? lib_path

db_path = {
  development: 'sqlite3:../data/db.sqlite3',
  production:  'sqlite3:../data/db.sqlite3',
  test:        'sqlite3:../data/test.sqlite3'
}
set :database, db_path[settings.environment]

# Mailings model
class Mailing < ActiveRecord::Base
  validates_uniqueness_of :order_ref
  validates_presence_of :order_ref, :date_sent
  validates_inclusion_of :is_post, in: [true, false]
end

get '/' do
  'Hello World!'
end

# Deliveries.
# Through which clients track their deliveries.

require 'dpd_reports.rb'

Thread.new do
  loop do
    sleep 10_800 # 3 hours
    DpdReports.new.save_to_db
  end
end

get '/deliveries' do
  send_file 'views/deliveries.html'
end

post '/deliveries' do
  'Deliveries!'
end
