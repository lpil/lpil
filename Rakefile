require 'active_record'
require 'csv'
require 'net/ftp'
require 'sqlite3'

# task :default => :migrate
site  = ENV['perivan_ordertracking_site']
login = ENV['perivan_ordertracking_login']
pass  = ENV['perivan_ordertracking_pass']

this_dir = File.dirname(
  File.symlink?(__FILE__) ? File.readlink(__FILE__) : __FILE__
)

namespace :db do
  desc 'Migrate the database, Target specific version with VERSION=x'
  task migrate: :environment do
    ActiveRecord::Migrator.migrate('migrations',
                                   ENV['VERSION'] ? ENV['VERSION'].to_i : nil)
  end
end

namespace :orders do
  desc 'Fetch new DPD reports from FTP and place in ./reports/'
  task :fetch do
    parsed_files = Dir.glob("#{this_dir}/reports/parsed/").map do |f|
      File.basename f
    end

    Net::FTP.open(site, login, pass) do |ftp|
      ftp.passive = true

      ftp.nlst.select do |f|
        f.match(/\.OUT\z/)
      end.reject do |f|
        parsed_files.include? f
      end.each do |f|
        write_file = "#{this_dir}/reports/#{f}"
        ftp.gettextfile f, write_file

        report_with_mod_time = File.read(write_file)
          .force_encoding('BINARY')
          .sub(/\n/, ",#{ftp.mtime f}")

        File.open write_file, 'w' do |e|
          e.puts report_with_mod_time
        end
      end
    end
  end

  desc 'Parse reports in ./reports/ and add to db'
  task parse: [:check_db_exists, :environment] do
    Dir.glob("#{this_dir}/reports/*.OUT").each do |f|
      Mailing.add_dpd_report(File.read f)
      File.rename f, "#{File.dirname f}/parsed/#{File.basename f}"
    end
  end
end

task :environment do
  ActiveRecord::Base.establish_connection(
    adapter:  'sqlite3',
    database: 'orders.sqlite3'
  )
end

# The Mailing model for the DPD + post deliveries.
class Mailing < ActiveRecord::Base
  validates_uniqueness_of :order_ref
  validates_presence_of :order_ref, :date_sent
  validates_inclusion_of :is_post, in: [true, false]

  def self.add_dpd_report(report)
    Mailing.create(
      Mailing.parse_dpd_report_string report.force_encoding('BINARY')
    )
  end

  # Used by fetch_new_reports
  def self.parse_dpd_report_string(report)
    report = CSV.parse_line(report)

    # TODO
    # FIXME
    fail "The report format isn't standardised! Talk to Dave"

    {
      order_ref: report[1].force_encoding('UTF-8'),
      is_post:   false,
      dpd_ref:   report[11].force_encoding('UTF-8'),
      date_sent: Time.parse(report[12])
    }
  end
end
