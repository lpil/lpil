class User < ActiveRecord::Base
  has_secure_password

  # Emails should be lowercase
  before_save { |user| user.email.downcase! }
  before_save :create_remember_token

  validates :email, presence: true,
    format: { with: /\A[\w+\-.]+@[a-z\d\-.]+\.[a-z]+\z/i },
    uniqueness: { case_sensitive: false }

  validates :password, presence: true,
    length: { minimum: 8 }

  validates :password_confirmation, presence: true

  private

  # Forget me not! Used to recognise logged in users returning to site
  def create_remember_token
    self.remember_token = SecureRandom.urlsafe_base64
  end
end
