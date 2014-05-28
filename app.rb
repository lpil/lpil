#!/usr/bin/env ruby
# encoding: utf-8

require 'sinatra'
require 'sinatra/activerecord'
require 'sinatra/reloader' if development?
require 'slim'
require 'logger'

# Add out application dir to the require LOAD_PATH
lib_path = File.expand_path(File.dirname(__FILE__))
$LOAD_PATH.unshift lib_path unless $LOAD_PATH.include? lib_path

db_path = {
  production:  'sqlite3:db/db.sqlite3',
  development: 'sqlite3:db/dev.sqlite3',
  test:        'sqlite3:db/test.sqlite3'
}
set :database, db_path[settings.environment]

# Logger
$log = Logger.new('tmp/app.log', 2, 1_024_000)

# Routes
require 'routes.rb'

# Models
require 'models/mailing.rb'

# DPD + post delivery tracking
#
# Every 3 hours:
#   Check the DPD report FTP for new reports, and add them to the database
#   Delete mailings more than 90 days old from the database

require 'lib/dpd_reports.rb'
Thread.new do
  loop do
    DpdReports.new.fetch_reports.save_to_db
    Mailing.delete_all(['date_sent < ?', Time.now - 7_776_000]) # 90 days
    sleep 10_800 # 3 hours
  end
end
