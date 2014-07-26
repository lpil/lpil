FactoryGirl.define do
  factory :user do
    first_name 'Louis'
    last_name  'Pilfold'
    sequence(:email) { |n| "lpilfold#{n}@email.co.uk" }
    reporter false
    uploader false
    password 'foobarbaz'
    password_confirmation 'foobarbaz'

    factory :reporter do
      reporter true
    end

    factory :uploader do
      uploader true
    end

    factory :uploaderReporter do
      uploader true
      reporter true

      factory :admin do
        admin true
      end
    end
  end
end
