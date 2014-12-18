class User < ActiveRecord::Base
  include ActiveModel::ForbiddenAttributesProtection

  has_many :shouts

  has_many :following_relationships, foreign_key: :follower_id
  has_many :followed_users, through: :following_relationships
end
