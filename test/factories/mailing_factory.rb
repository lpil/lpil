FactoryGirl.define do
  factory :mailing do
    sequence :id
    sequence(:order_ref) { |n| "ref_#{n}" }
    is_post false
    sequence(:dpd_ref) { |n| "dpd_#{n}" }
    date_sent { rand(60).days.ago }
    created_at { Time.now }
    updated_at { Time.now }
  end
end
