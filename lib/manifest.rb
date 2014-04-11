#!/usr/bin/env ruby
# encoding: utf-8

require 'roo'

# Parses the manifest to get a list of orders with either
# post number or a dpd tracking number
class Manifest
  def initialize(xls_path)
    xls = Roo::Excel.new xls_path
    @dpd_orders  = parse_export_sheet xls.sheet('EXPORT')
    @post_orders = parse_post_sheet   xls.sheet('Post')
  end

  private

  def parse_export_sheet(sheet)
    dpd = {}
    sheet.each do |row|
      a = {}
      a[:tracking_num_0] = row[1].to_i
      a[:tracking_num_1] = row[2].to_i
      a[:name]           = row[3]
      a[:address] = [row[6], row[7], row[8]].compact.join ', '
      dpd[row[0].to_i] = (dpd.fetch(row[0].to_i, []) << a)
    end
    dpd
  end

  def parse_post_sheet(sheet)
    post = {}
    sheet.drop(1).each do |row|
      a = {}
      a[:date] = row[0]
      a[:name] = row[2]
      a[:address] = row[3]
      post[row[1].to_i] = (post.fetch(row[1].to_i, []) << a)
    end
    post
  end
end
