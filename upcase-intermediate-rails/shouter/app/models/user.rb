class User < ActiveRecord::Base
  include ActiveModel::ForbiddenAttributesProtection

  has_many :shouts

  has_many :followed_user_relationships,
           foreign_key: :follower_id,
           class_name: 'FollowingRelationship'
  has_many :followed_users, through: :followed_user_relationships

  has_many :follower_relationships,
           foreign_key: :followed_user_id,
           class_name: 'FollowingRelationship'
  has_many :followers, through: :follower_relationships
end
