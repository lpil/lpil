#!/usr/bin/env ruby
# encoding: utf-8

require 'csv'
require 'net/ftp'

# Pulls the CSV for each delivery from the DPD FTP, parses, and adds to the db
# Reports that have been read are moved to a subdir
class DpdReports
  attr_reader :reports

  def initialize
    @site, @login, @pass, @reports =
      `echo $ftp_dpd_site`.chomp, `echo $ftp_dpd_login`.chomp,
      `echo $ftp_dpd_pass`.chomp, []
  end

  # Fetches the DPD report files from the FTP specified by the ftp_dpd_site,
  # ftp_dpd_login, and ftp_dpd_pass enviroment variables.
  #
  # If production? the reports are moved to the 'parsed_reports' dir within the
  # ftp after being read.
  def fetch_reports
    Net::FTP.open(@site, @login, @pass) do |ftp|
      files = ftp.nlst
      ftp.mkdir 'parsed_reports' unless files.include? 'parsed_reports'
      files.select { |e| e.match(/\.OUT$/) }.each do |file|
        ftp.gettextfile(file, "#{File.expand_path('../tmp/')}/#{file}")
        @reports << parse_report(File.read("../tmp/#{file}")
          ).merge(date_sent: ftp.mtime(file))
        ftp.rename(file, "parsed_files/#{file}")
        File.delete "../tmp/#{file}" if production?
      end
    end
    self
  end

  # Saves the reports to the database. Nothing will happen unless the
  # {#fetch_reports} method is called beforehand.
  #
  # @return [Array] The return values from each Mailing.create
  def save_to_db
    @reports.map do |report|
      Mailing.create report
    end
  end

  private

  def parse_report(report)
    report = CSV.parse_line(report)
    {
      order_ref: report[1],
      is_post:   false,
      dpd_ref:   report[11]
    }
  end
end
