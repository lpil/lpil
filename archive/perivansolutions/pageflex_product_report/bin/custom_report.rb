#!/usr/bin/env ruby
# encoding: utf-8

# Add out application's lib dir to the require LOAD_PATH
lib_path = File.expand_path('../lib', File.dirname(__FILE__))
$LOAD_PATH.unshift lib_path unless $LOAD_PATH.include? lib_path

require 'pageflex_data.rb'
require 'categories.rb'
require 'current_products.rb'
require 'pageflex_report/custom_pageflex_report.rb'

# Currently CustomPageflexReport.write expects Expiry Date to be 3rd
# If it is not you will need to modify that class.
columns = [
  ['Code', :Code__STR],
  ['DisplayName', :HTML_DisplayName__STR],
  ['Expiry Date', :"metadata_Expiry Date"],
  ['Document Type', :"metadata_Document Type"],
  ['Team', :metadata_Team],
  ['Contact', :metadata_Contact],
  ['Key Words', :Keywords__STR],
  # ['Retired?', :b_IsRetired],
  # ['Archived?', :b_IsArchived],
  # ['Deleted?', :b_IsDeleted],
  ['Category 0', :Category0],
  ['Category 1', :Category1],
  ['Category 2', :Category2],
  ['Category 3', :Category3]
]

a = PageflexData.new
b = Categories.new a.categories
c = CurrentProducts.new a.products, a.metadata, a.cat_entries, b, a.assets
CustomPageflexReport.new(c, columns).write
puts "\t...done!"
