this_dir = File.dirname(
  File.symlink?(__FILE__) ? File.readlink(__FILE__) : __FILE__
)

$LOAD_PATH << this_dir

require 'active_record'
require 'csv'
require 'investor'
require 'sqlite3'

namespace :db do
  desc 'Migrate the database, Target specific version with VERSION=x'
  task migrate: :environment do
    ActiveRecord::Migrator.migrate(
      'migrations', ENV['VERSION'] ? ENV['VERSION'].to_i : nil)
  end

  task clear: [:'db:exists?', :environment] do
    puts 'This will remove all existing investor records from this db.'
    puts 'Are you sure? (Y/n)'
    abort 'Aborted' unless STDIN.gets.chomp.downcase == 'y'
    Investor.delete_all
  end

  task :exists? do
    fail 'No db! Run migrations to create db' unless File.exist?(
      "#{this_dir}/investors.sqlite3"
    )
  end
end

namespace :investor do
  desc 'Upload a new investor csv to the database. spreadsheet=/path/to/csv'
  task upload: :'db:clear' do
    fail NotImplementedError
  end
end

task :environment do
  ActiveRecord::Base.establish_connection(
    adapter:  'sqlite3',
    database: 'investors.sqlite3'
  )
end
