#!/usr/bin/env ruby
# encoding: utf-8

require 'csv'
require 'net/ftp'

# Pulls the CSV for each delivery from the DPD FTP, parses, and adds to the db
# Reports that have been read are moved to a subdir
class DpdReports
  attr_reader :reports

  def initialize
    site, login, pass, @reports =
      `echo $ftp_dpd_site`.chomp, `echo $ftp_dpd_login`.chomp,
      `echo $ftp_dpd_pass`.chomp, []
    Net::FTP.open(site, login, pass) do |ftp|
      files = ftp.nlst
      ftp.mkdir 'parsed_files' unless files.include? 'parsed_files'
      files.select { |e| e.match(/\.OUT$/) }.each do |file|
        ftp.gettextfile(file, "#{File.expand_path('../tmp/')}/#{file}")
        @reports << parse_report(File.read("../tmp/#{file}")
          ).merge(date_sent: ftp.mtime(file))
        ftp.rename(file, "parsed_files/#{file}")
        File.delete "../tmp/#{file}"
      end
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
