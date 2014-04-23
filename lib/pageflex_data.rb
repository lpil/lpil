require 'nokogiri'

# Extracts the information from the XML
class PageflexData
  attr_reader :names, :metadata, :products, :categories, :cat_entries

  def initialize
    puts 'Parsing XML document'
    @doc      = parse_xml_doc
    puts 'Extracting field names table'
    @names    = build_names
    puts 'Extracting metadata table, resolving names'
    @metadata = build_metadata
    puts 'Extracting products table'
    @products = build_products
    puts 'Extracting categories table'
    @categories = build_categories
    puts 'Extracting catalog entries table'
    @cat_entries = build_catalog_entries
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
      name  = @names[i.attributes['FieldNameID__IDREF'].value.to_sym]
      name  = ('metadata_' + name).to_sym
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
