class User < ActiveRecord::Base
  include ActiveModel::ForbiddenAttributesProtection
end
