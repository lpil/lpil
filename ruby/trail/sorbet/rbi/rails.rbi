# typed: strong
class Rails::Application
  def self.config; end
  def config; end
end

class ActionMailer::Base
  def self.layout(layout); end
end

class ActiveSupport::Logger
  def formatter=(formatter); end
end

class ActiveSupport::TestCase
  def self.fixtures(formatter); end
end

class ActionDispatch::Routing::Mapper
  def devise_for(x); end
end
