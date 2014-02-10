#!/usr/bin/ruby
# encoding: utf-8
# rubocop: disable MethodLength

require 'pry'
require 'nokogiri'

# The meat
class AxaReportFromXml
  def initialize
    puts 'Parsing XML document'
    @doc      = parse_xml_doc
    puts 'Parsing field names table'
    @names    = build_names_hash
    puts 'Parsing metadata table'
    @metadata = build_metadata_hash
  end

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
    metadata.default = {}
    @doc.xpath([
    '/PFWeb:Database', '/PFWeb:ProductMetadataFieldValues__Table',
    '/PFWeb:ProductMetadataFieldValues__Row'
    ].join('')).each do |i|
      m_key   = i.attributes['ProductID__IDREF'].value.to_sym
      m_name  = @names[i.attributes['FieldNameID__IDREF'].value.to_sym]
      m_value = i.attributes['FieldValue__STR']
      m_value = m_value ? m_value.value : '' # Handle nil values
      a = metadata[m_key]
      a[m_name] = m_value
      metadata[m_key] = a
    end
    metadata
  end
end

# rubocop: disable all
a = AxaReportFromXml.new
puts a.class

binding.pry

# names = doc.xpath('/PFWeb:Database/PFWeb:Names__Table/PFWeb:Names__Row')
# names.each { |e| puts e.attributes['NameID__ID'].value }
#
# /PFWeb:Database/PFWeb:Products__Table/PFWeb:Products__Row
  # "ProductID__ID",
  # "HTML_DisplayName__STR",
  # "Keywords__STR",
  # "b_IsStaged",
  # "b_IsArchived",
  # "b_IsRetired",
  # "b_IsDeleted",
  # "Code__STR",
  # "SMDisplayName__STR",
  # "SMDescription__STR",
  # "SMPermaLink__STR",
  # "ExternalProductID__STR",

# /PFWeb:Database/PFWeb:ProductMetadataFieldValues__Table/PFWeb:ProductMetadataFieldValues__Row
  # "ProductID__IDREF",       The ref of the attached product
  # "FieldNameID__IDREF",     The ref of the field name
  # "FieldValue__STR",

# /PFWeb:Database/PFWeb:Names__Table/PFWeb:Names__Row
  # "NameID__ID",
  # "StringValue__STR"
