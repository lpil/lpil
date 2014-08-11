require 'simplecov'
SimpleCov.start
ENV['RAILS_ENV'] ||= 'test'
require File.expand_path('../../config/environment', __FILE__)
require 'rails/test_help'
require 'minitest/pride'
require 'minitest/rails/capybara'

FactoryGirl.find_definitions

class MiniTest::Unit::TestCase
  include FactoryGirl::Syntax::Methods
end

class ActiveSupport::TestCase
  # Setup all fixtures in test/fixtures/*.yml for all tests in alphabetical
  # order.
  # fixtures :all
end

class ActionDispatch::IntegrationTest
  include Capybara::DSL

  def sign_in(user)
    visit signin_path
    fill_in 'Email', with: user.email
    fill_in 'Password', with: user.password
    click_button 'Sign in'
    user
  end

  # Metaprogram a handy method for various user types
  #   i.e. new_signed_in_user
  # Creates a new user (or other), signs them in, and returns the someone
  %i(user admin uploader reporter).each do |type|
    define_method("new_signed_in_#{type}") do
      someone = FactoryGirl.create type
      sign_in someone
    end
  end
end
