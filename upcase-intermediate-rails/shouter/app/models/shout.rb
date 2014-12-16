class Shout < ActiveRecord::Base
  belongs_to :user

  # We always want the newest shouts first
  default_scope { order 'created_at DESC' }

  validates :body, presence: true
end
