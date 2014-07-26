class Collection < ActiveRecord::Base
  has_many :users

  validates :name, presence: true,
    uniqueness: { case_sensitive: false }
end
