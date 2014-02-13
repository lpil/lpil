#!/usr/bin/env ruby
# encoding: utf-8
# rubocop: disable MethodLength

columns = [
  ['Code', :Code__STR],
  ['DisplayName', :HTML_DisplayName__STR],
  ['Expiry Date', :"Expiry Date"],
  ['Document Type', :"Document Type"],
  ['Team', :Team],
  ['Contact', :Contact],
  ['Key Words', :Keywords__STR],
  ['Retired?', :b_IsRetired],
  ['Archived?', :b_IsArchived],
  ['Deleted?', :b_IsDeleted]
]

# FIXME
# rubocop: disable Debugger

require 'pry' # FIXME
require 'nokogiri'
require 'csv'

# Extracts the information from the XML
class PageflexData
  attr_reader :names, :metadata, :products

  def initialize
    puts 'Parsing XML document'
    @doc      = parse_xml_doc
    puts 'Extracting field names table'
    @names    = build_names_hash
    puts 'Extracting metadata table'
    @metadata = build_metadata_hash
    puts 'Extracting products table'
    @products = build_products_hash
  end

  private

  def parse_xml_doc
    # Get the xml location from the CLI args
    if ARGV.any?
      a_xml = File.read(ARGV.join(' '))
      doc   = Nokogiri::XML(a_xml)
    else
      abort 'Pass the location of the MMStore XML to run script'
    end
    doc
  end

  def build_names_hash
    names = {}
    @doc.xpath(
      '/PFWeb:Database/PFWeb:Names__Table/PFWeb:Names__Row'
    ).each do |i|
      n_key   = i.attributes['NameID__ID'].value.to_sym
      n_value = i.attributes['StringValue__STR'].value
      names[n_key] = n_value
    end
    names
  end

  def build_metadata_hash
    metadata = {}
    @doc.xpath([
      '/PFWeb:Database', '/PFWeb:ProductMetadataFieldValues__Table',
      '/PFWeb:ProductMetadataFieldValues__Row'
    ].join('')).each do |i|
      m_key   = i.attributes['ProductID__IDREF'].value.to_sym
      m_name  = @names[i.attributes['FieldNameID__IDREF'].value.to_sym].to_sym
      m_value = i.attributes['FieldValue__STR']
      m_value = m_value ? m_value.value : '' # Handle nil values
      a = metadata.fetch m_key, {}
      a[m_name] = m_value
      metadata[m_key] = a
    end
    metadata
  end

  def build_products_hash
    products = []
    @doc.xpath(
      '/PFWeb:Database/PFWeb:Products__Table/PFWeb:Products__Row'
    ).each do |i|
      attrs = {}
      i.attributes.each do |a|
        key   = a[0].to_sym
        value = a[1].value
        attrs[key] = value
      end
      products << attrs
    end
    products
  end
end

# Builds our report CSV
class AXAReport < Array
  def initialize(pageflex_data, columns)
    @products = attach_metadata pageflex_data.products, pageflex_data.metadata
    @products = filter_old_versions @products
    build_report @products, columns
  end

  def write_csv
    file_name = "axa_mmstore_report_#{Time.new.strftime '%Y_%m_%d'}.csv"
    puts "Writing to '#{file_name}'"
    CSV.open(file_name, 'wb') do |csv|
      each { |row| csv << row }
    end
  end

  private

  def filter_old_versions(products)
    puts 'Identifying current versions'
    buckets = {}
    products.each do |p|
      if p.key? :MasterProductID__IDREF
        b = buckets.fetch p[:MasterProductID__IDREF].to_sym, []
        buckets[p[:MasterProductID__IDREF].to_sym] = b << p
      else
        b = buckets.fetch p[:ProductID__ID].to_sym, []
        buckets[p[:ProductID__ID].to_sym] = b << p
      end
    end
    date_key = :DateTimeModified__ISO8601
    buckets.map do |b|
      b[1].sort { |x, y| y[date_key] <=> x[date_key] }.first
    end
  end

  def attach_metadata(products, metadata)
    puts 'Attaching metadata to products'
    products.each do |p|
      p.merge! metadata.fetch p[:ProductID__ID].to_sym, {}
    end
    products
  end

  def build_report(products, columns)
    puts 'Building report arrays'
    products.each do |p|
      row = []
      columns.each { |c| row << (p.fetch c[1], '') }
      self << row
    end
    strip_html
    # Sort by the third column
    self.sort! { |y, x| x[2] <=> y[2] }
    # Add header
    unshift columns.reduce([]) { |a, e| a << e[0] }
  end

  def strip_html
    puts 'Stripping in-line HTML'
    map! { |row| row.map! { |cell| cell.sub(/ *<.*>.*<\/.*>/, '') } }
  end
end

AXAReport.new(PageflexData.new, columns).write_csv
puts "\t...done!"
