class Category < ActiveRecord::Base
  has_ancestry orphan_strategy: :adopt

  belongs_to :collection
  has_many :users, through: :collection

  validates :name, presence: true
  validates :collection, presence: true
end
