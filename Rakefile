this_dir = File.dirname(
  File.symlink?(__FILE__) ? File.readlink(__FILE__) : __FILE__
)

$LOAD_PATH << this_dir

require 'active_record'
require 'investor'
require 'csv'
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
  desc "Upload a new investor csv spreadsheet to the database.\n"\
  'csv=/path/to/csv'
  # task upload: :environment, :'db:clear' do
  task upload: :environment do
    fail 'Pass arg: csv=/path/to/csv' unless ENV['csv']

    header_map = {
      'Partner Code' => :partner_code,
      'Name' => :name,
      'Add1' => :address0,
      'Add2' => :address1,
      'Add3' => :address2,
      'Add4' => :address3,
      'Postcode' => :post_code,
      'PIM Quantity' => :pim_quantity,
      'QIR Quantity' => :qir_quantity,
      'GIM Quantity' => :gim_quantity,
      'Investment CD - generic CD & Wallet' => :cd_wallet,
      'Investment CD generic CD personalised wallet' => :cd_custom_wallet,
      'Investment CD personalised Wallet and recording' =>
                                                        :cd_custom_wallet_rec,
      'Generic Trustee CD' => :trust_cd_generic,
      'Personalised Trustee CD' => :trust_cd_personal
    }

    CSV.read(ENV['csv'], headers: true).each do |row|

      Investor.create(
        row.select do |e|
          header_map.keys.include? e.first
        end.each_with_object({}) do |attribute, acc|
          acc[header_map[attribute.first]] = attribute.last if attribute.last
        end
      )
    end
  end
end

task :environment do
  ActiveRecord::Base.establish_connection(
    adapter:  'sqlite3',
    database: 'investors.sqlite3'
  )
end
