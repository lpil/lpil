class Collection < ActiveRecord::Base
  has_many :categories
  has_many :users

  validates :name, presence: true,
    uniqueness: { case_sensitive: false }

  # Create the root category when we create a new collection
  # Things break if we have a collection without any categories
  after_create do |collection|
    Category.create name: collection.name, collection: collection
  end
end
