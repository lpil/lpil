require 'csv'
require 'net/ftp'

# The Mailing model for the DPD + post deliveries.
class Mailing < ActiveRecord::Base
  validates_uniqueness_of :order_ref
  validates_presence_of :order_ref, :date_sent
  validates_inclusion_of :is_post, in: [true, false]

  # Look up an order ref in the database.
  # Returns nil if not found.
  #
  # @param order_ref [String] The order reference to find in the DB
  # @return [hash] The information accociated with that reference in the DB
  def self.lookup(order_ref)
    m = Mailing.find_by_order_ref(order_ref)
    return nil unless m
    m = m.attributes.symbolize_keys
    return unless m[:dpd_ref]
    m.merge url: "http://www.dpd.co.uk/apps/tracking/?reference=#{m[:dpd_ref]}"
  end

  # Fetches the DPD report files from the FTP specified by the ftp_dpd_site,
  # ftp_dpd_login, and ftp_dpd_pass enviroment variables.
  #
  # The reports files are moved to the 'parsed_reports' dir within the ftp
  # after being read.
  #
  # Parsed reports are saved to the database
  def self.fetch_new_reports!
    Mailing.each_new_dpd_ftp_report do |ftp, file|
      ftp.gettextfile(file, "#{File.expand_path('tmp/')}/#{file}")

      Mailing.create(
        Mailing.parse_dpd_report(
          File.read("tmp/#{file}").force_encoding('BINARY')
        ).merge(date_sent: ftp.mtime(file))
      )
      ftp.rename(file, "parsed_reports/#{file}")
      File.delete "tmp/#{file}"
    end
  end

  # Used by fetch_new_reports
  def self.parse_dpd_report(report)
    report = CSV.parse_line(report)
    {
      order_ref: report[1].force_encoding('UTF-8'),
      is_post:   false,
      dpd_ref:   report[11].force_encoding('UTF-8')
    }
  end

  # Used by fetch_new_reports
  def self.each_new_dpd_ftp_report
    site, login, pass =
      ENV['ftp_dpd_site'], ENV['ftp_dpd_login'], ENV['ftp_dpd_pass']
    unless site && login && pass
      $logger.error { 'dpd_ftp env variables not set' }
      return
    end

    Net::FTP.open(site, login, pass) do |ftp|
      files = ftp.nlst
      ftp.mkdir 'parsed_reports' unless files.include? 'parsed_reports'

      files.select { |e| e.match(/\.OUT$/) }.each do |file|
        yield ftp, file
      end
    end
  end
end
