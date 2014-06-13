#!/usr/bin/env ruby
# encoding: utf-8

require 'sinatra'
require 'sinatra/activerecord'
require 'sinatra/reloader' if development?
require 'slim'
require 'logger'
require 'json'

# Add out application dir to the require LOAD_PATH
path = File.expand_path(File.dirname(__FILE__))
$LOAD_PATH.unshift path unless $LOAD_PATH.include? path

db_path = {
  production:  'sqlite3:db/db.sqlite3',
  development: 'sqlite3:db/dev.sqlite3',
  test:        'sqlite3:db/test.sqlite3'
}
set :database, db_path[settings.environment]

# Allow embedding in iframes
set :protection, except: [:frame_options, :json_csrf]

# Logger
$log = Logger.new('tmp/app.log', 2, 1_024_000)

# Threads
$threads = {}

# Routes
require 'routes.rb'

# Models
require 'models/mailing.rb'
require 'models/axa_upload.rb'

# Helpers
helpers do
  def html_escape(text)
    Rack::Utils.escape_html(text)
  end
end

# DPD + post delivery tracking
#
# Every hour:
#   Check the DPD report FTP for new reports, and add them to the database
#   Delete mailings more than 90 days old from the database

$threads[:mailings] = Thread.new do
  loop do
    begin
      Mailing.fetch_new_reports!
      Mailing.delete_all(['date_sent < ?', Time.now - 7_776_000]) # 90 days
      sleep 3_600 # 1 hour
    rescue StandardError, ScriptError => e
      $logger.error { "Mailing Thread => #{e}\n#{e.backtrace.join "\t\n"}" }
      sleep 60
    end
  end
end
