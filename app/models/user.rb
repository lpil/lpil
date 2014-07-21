class User < ActiveRecord::Base
  # Emails should be donwcase
  before_save { |user| user.email.downcase! }

  validates :email, presence: true,
    format: { with: /\A[\w+\-.]+@[a-z\d\-.]+\.[a-z]+\z/i },
    uniqueness: { case_sensitive: false }
end
