#!/usr/bin/env ruby
# encoding: utf-8
# rubocop: disable MethodLength

require 'pry' # FIXME

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

require 'nokogiri'
require 'csv'

# Extracts the information from the XML
class PageflexData
  attr_reader :names, :metadata, :products

  def initialize
    puts 'Parsing XML document'
    @doc      = parse_xml_doc
    puts 'Extracting field names table'
    @names    = build_names
    puts 'Extracting metadata table'
    @metadata = build_metadata
    puts 'Extracting products table'
    @products = build_products
    puts 'Extracting categories table'
    @categories = build_categories
    puts 'Extracting catalog entries table'
    @catalog_entries = build_catalog_entries
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

  def build_names
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

  def build_metadata
    metadata = {}
    @doc.xpath([
      '/PFWeb:Database', '/PFWeb:ProductMetadataFieldValues__Table',
      '/PFWeb:ProductMetadataFieldValues__Row'
    ].join('')).each do |i|
      key   = i.attributes['ProductID__IDREF'].value.to_sym
      name  = @names[i.attributes['FieldNameID__IDREF'].value.to_sym].to_sym
      value = i.attributes['FieldValue__STR']
      value = value ? value.value : '' # Handle nil values
      a = metadata.fetch key, {}
      a[name] = value
      metadata[key] = a
    end
    metadata
  end

  def build_products
    products = {}
    @doc.xpath(
      '/PFWeb:Database/PFWeb:Products__Table/PFWeb:Products__Row'
    ).each do |i|
      id, attrs = nil, {}
      i.attributes.each do |a|
        id    = a[1].value.to_sym if a[1].name == 'ProductID__ID'
        key   = a[0].to_sym
        value = a[1].value
        attrs[key] = value
      end
      products[id] = attrs
    end
    products
  end

  def build_categories
    categories = {}
    @doc.xpath([
      '/PFWeb:Database/PFWeb:ProductCatalogCategories__Table',
      '/PFWeb:ProductCatalogCategories__Row'
    ].join('')).each do |i|
      id, attrs = nil, {}
      i.attributes.each do |a|
        id    = a[1].value.to_sym if a[1].name == 'ProductCategoryID__ID'
        key   = a[0].to_sym
        value = a[1].value
        attrs[key] = value
      end
      categories[id] = attrs
    end
    categories
  end

  def build_catalog_entries
    entries = {}
    @doc.xpath([
      '/PFWeb:Database/PFWeb:ProductCatalogEntries__Table',
      '/PFWeb:ProductCatalogEntries__Row'
    ].join('')).each do |i|
      key   = i.attributes['ProductID__IDREF'].value.to_sym
      value = i.attributes['ParentCategoryID__IDREF'].value
      entries[key] = (entries.fetch key, []) << value
    end
    entries
  end
end

# Builds our report CSV
class PageflexReport < Array
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
    self
  end

  private

  def filter_old_versions(products)
    puts 'Identifying current versions'
    buckets = {}
    products.values.each do |p|
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
    products.values.each do |p|
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
    sort_by_uk_date
    # Add header
    unshift columns.reduce([]) { |a, e| a << e[0] }
  end

  def sort_by_uk_date
    # Sort by the third column, after formatting the date to be sortable
    puts 'Sorting by date'
    sort! do |y, x|
      x[2].split('/').reverse.join('/') <=> y[2].split('/').reverse.join('/')
    end
  end

  def strip_html
    puts 'Stripping in-line HTML'
    map! { |row| row.map! { |cell| cell.sub(/ *<.*>.*<\/.*>/, '') } }
  end
end

PageflexReport.new(PageflexData.new, columns).write_csv
puts "\t...done!"
