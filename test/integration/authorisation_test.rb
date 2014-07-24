require 'test_helper'

class AuthorisationTest < Capybara::Rails::TestCase
  # Here be metaprogramming...

  reporter_disallowed_paths = [
    "'users'", "'users/new'", 'edit_user_path FactoryGirl.create :user',
    'user_path FactoryGirl.create :user'
  ]

  uploader_disallowed_paths = [
    "'users'", "'users/new'", 'edit_user_path FactoryGirl.create :user',
    'user_path FactoryGirl.create :user'
  ]

  user_disallowed_paths = ([
  ] + reporter_disallowed_paths + uploader_disallowed_paths).uniq

  guest_disallowed_paths = ([
  ] + user_disallowed_paths + uploader_disallowed_paths +
  reporter_disallowed_paths).uniq

  # Setup string goes before this string
  test_body0 = %{
    ;visit }
    # path goes here
  test_body1 = %{
      ;assert page.has_selector?('h1', text: 'Sign in'),
        'Did not redirect to sign in page. Sign in h1 missing'
      assert page.has_selector?('input', visible: 'Sign in'),
        'Did not redirect to sign in page. Sign in button missing'
      assert page.has_selector?('label', text: 'Email'),
        'Did not redirect to sign in page. Email label missing'
      assert page.has_selector?('label', text: 'Password'),
        'Did not redirect to sign in page. Password label missing'
      refute true, 'foo'
    end
  }

  # Test for reporters
  reporter_disallowed_paths.each do |path|
    name = path.scan(/[A-z]/).join('')
    class_eval 'def test_reporter_' + name +
      '; visitor = FactoryGirl.create :reporter' + test_body0 + path +
      test_body1
  end
  # Test for uploaders
  uploader_disallowed_paths.each do |path|
    name = path.scan(/[A-z]/).join('')
    class_eval 'def test_uploader_' + name +
      '; visitor = FactoryGirl.create :uploader' + test_body0 + path +
      test_body1
  end
  # Test for regular users
  user_disallowed_paths.each do |path|
    name = path.scan(/[A-z]/).join('')
    class_eval 'def test_user_' + name +
      '; visitor = FactoryGirl.create :user' + test_body0 + path +
      test_body1
  end
  # Test for guests
  guest_disallowed_paths.each do |path|
    name = path.scan(/[A-z]/).join('')
    class_eval 'def test_guest_' + name + '; ' + test_body0 + path +
      test_body1
  end
end
