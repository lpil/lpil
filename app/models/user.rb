class User < ActiveRecord::Base
  attr_accessor :first_name, :last_name, :email, :reporter, :uploader

  validates :email, presence: true,
    format: { with: /\A[\w+\-.]+@[a-z\d\-.]+\.[a-z]+\z/i }
end
