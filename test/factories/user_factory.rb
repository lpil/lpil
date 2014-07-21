# This will guess the User class
FactoryGirl.define do
  factory :user do
    first_name 'Louis'
    last_name  'Pilfold'
    sequence(:email) { |n| "lpilfold#{n}@email.co.uk" }
    reporter false
    uploader false
  end
end
