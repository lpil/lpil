#!/usr/bin/env ruby
# encoding: utf-8

require 'net/ftp'

def ftp_has_file?(ftp_addr, ftp_user, ftp_pass, filename)
  Net::FTP.open(ftp_addr, ftp_user, ftp_pass) { |ftp|
    return ftp.nlst.include? filename
  }
end
