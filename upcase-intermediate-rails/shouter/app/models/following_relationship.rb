class FollowingRelationship < ActiveRecord::Base
  belongs_to :follower, class_name: 'User'
  belongs_to :followed_user, class_name: 'User'
end
