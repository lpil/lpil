require 'minitest/autorun'
require 'minitest/pride'
require 'rack/test'

require_relative '../app.rb'

Sinatra::Application.environment = 'test'
