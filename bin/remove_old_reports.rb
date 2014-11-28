#!/usr/bin/env ruby

require_relative '../lib/order_tracking.rb'

manager = ReportManager.new(FTP_CONFIG)
manager.remove_old_reports_from_ftp
manager.remove_old_reports_from_local_dir
manager.remove_old_reports_from_db
