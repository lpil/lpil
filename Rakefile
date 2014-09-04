require 'active_record'
require 'sqlite3'
require 'yaml'

# task :default => :migrate

namespace :db do
  desc 'Migrate the database, Target specific version with VERSION=x'
  task migrate: :environment do
    ActiveRecord::Migrator.migrate('db/migrate',
                                   ENV['VERSION'] ? ENV['VERSION'].to_i : nil)
  end
end

task :environment do
  ActiveRecord::Base.establish_connection(
    adapter:  'sqlite3',
    database: 'orders.sqlite3'
  )
end
