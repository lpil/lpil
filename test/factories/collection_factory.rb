FactoryGirl.define do
  factory :collection do
    sequence(:name) { |n| "Collection Number #{n}" }
  end
end
