FactoryGirl.define do
  factory :collection do
    sequence(:name) { |n| "Collection #{(n*n*n).to_s 36}" }
  end
end
