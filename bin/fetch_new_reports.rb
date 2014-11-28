#!/usr/bin/env ruby

require_relative '../lib/order_tracking.rb'

ReportManager
  .new(FTP_CONFIG)
  .fetch_new_reports
