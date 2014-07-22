class User < ActiveRecord::Base
  has_secure_password

  # Emails should be lowercase
  before_save { |user| user.email.downcase! }

  validates :email, presence: true,
    format: { with: /\A[\w+\-.]+@[a-z\d\-.]+\.[a-z]+\z/i },
    uniqueness: { case_sensitive: false }

  validates :password, presence: true,
    length: { minimum: 8 }

  validates :password_confirmation, presence: true
end
