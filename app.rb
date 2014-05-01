#!/usr/bin/env ruby
# encoding: utf-8

require 'sinatra'
require 'sinatra/activerecord'
require 'sinatra/reloader' if development?

db_path = {
  development: 'sqlite3:../data/db.sqlite3',
  production:  'sqlite3:../data/db.sqlite3',
  test:        'sqlite3:../data/test.sqlite3'
}
set :database, db_path[settings.environment]

# Deliverys model
class Delivery < ActiveRecord::Base
  validates_uniqueness_of :order_ref
  validates_presence_of :order_ref, :is_post?, :date_sent
end

get '/' do
  'Hello World!'
end

get '/deliveries' do
  send_file 'views/deliveries.html'
end

post '/deliveries' do
  'Deliveries!'
end
