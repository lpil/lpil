#!/usr/bin/env ruby
# encoding: utf-8

# Add out application's lib dir to the require LOAD_PATH
lib_path = File.expand_path('../lib', File.dirname(__FILE__))
$LOAD_PATH.unshift lib_path unless $LOAD_PATH.include? lib_path

require 'pageflex_data.rb'
require 'categories.rb'
require 'current_products.rb'
require 'pageflex_report.rb'

columns = [
  ['Code', :Code__STR],
  ['DisplayName', :HTML_DisplayName__STR],
  ['Key Words', :Keywords__STR],
  ['Retired?', :b_IsRetired],
  ['Archived?', :b_IsArchived],
  ['Deleted?', :b_IsDeleted],
  ['Category 0', :Category0],
  ['Category 1', :Category1],
  ['Category 2', :Category2],
  ['Category 3', :Category3]
]

a = PageflexData.new
b = Categories.new a.categories
c = CurrentProducts.new a.products, a.metadata, a.cat_entries, b
PageflexReport.new(c, columns).write_csv
puts "\t...done!"
