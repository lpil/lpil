class User < ActiveRecord::Base
  has_secure_password
  belongs_to :collection

  # Emails should be lowercase
  before_save { |user| user.email.downcase! }
  before_save :create_remember_token

  validates :email, presence: true,
    format: { with: /\A[\w+\-.]+@[a-z\d\-.]+\.[a-z]+\z/i },
    uniqueness: { case_sensitive: false }

  # Password validation for creation
  validates :password, presence: true,
    length: { minimum: 8 }, on: :create

  # Password validation for future updates
  # We allow nil values, because we don't always want to have to enter the
  # password every time we change a user attribute. Especially if it is an
  # admin making the changes- they won't know the password
  validates :password, presence: true,
    length: { minimum: 8 }, allow_nil: true, on: :update

  private

  # Forget me not! Used to recognise logged in users returning to site
  def create_remember_token
    self.remember_token = SecureRandom.urlsafe_base64
  end
end
