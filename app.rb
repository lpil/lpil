#!/usr/bin/env ruby
# encoding: utf-8

require 'sinatra'
require 'sinatra/activerecord'
require 'sinatra/reloader' if development?
require 'rack-livereload' if development?
require 'slim'
require 'logger'
require 'json'

use Rack::LiveReload if development?

# Add out application dir to the require LOAD_PATH
path = File.expand_path(File.dirname(__FILE__))
$LOAD_PATH.unshift path unless $LOAD_PATH.include? path

# Directories
$d = {}
$d[:log] = ENV['OPENSHIFT_LOG_DIR'] || File.expand_path('tmp/') << '/'
$d[:tmp] = ENV['OPENSHIFT_TMP_DIR'] || File.expand_path('tmp/') << '/'
$d[:db]  = ENV['OPENSHIFT_DATA_DIR'] || File.expand_path('db/') << '/'

db_path = {
  production:  "sqlite3:#{$d[:db]}db.sqlite3",
  development: 'sqlite3:db/dev.sqlite3',
  test:        'sqlite3:db/test.sqlite3'
}
set :database, db_path[settings.environment]

# Allow embedding in iframes
set :protection, except: [:frame_options, :json_csrf]

# Logger
$logger = Logger.new("#{$d[:log]}app.log", 2, 1_024_000)

# Threads
$threads = {}

# Routes
require 'routes.rb'

# Models
require 'models/mailing.rb'

# Periodic tasks
require 'lib/every.rb'

# Helpers
helpers do
  def html_escape(text)
    Rack::Utils.escape_html(text)
  end
end

#
# Periodic Tasks
#

# Check the DPD report FTP for new reports, and add them to the database
$threads[:fetch_dpd_reports] = every 1.hour do
  Mailing.fetch_new_reports!
end

#  Delete mailings more than 90 days old from the database
$threads[:delete_old_mailings] = every 1.hour do
  Mailing.delete_all(['date_sent < ?', 90.days.ago])
end
