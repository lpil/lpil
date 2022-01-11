FactoryGirl.define do
  factory :category do
    sequence(:name) { |n| "Category #{(n*n*n).to_s 33}" }
    association :collection, factory: :collection
  end
end
