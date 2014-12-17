class Shout < ActiveRecord::Base
  belongs_to :user
  belongs_to :content, polymorphic: true

  # We always want the newest shouts first
  default_scope { order 'created_at DESC' }
end
