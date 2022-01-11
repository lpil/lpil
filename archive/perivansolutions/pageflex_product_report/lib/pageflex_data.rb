# Extracts the information from the XML
class PageflexData
  attr_reader :names, :metadata, :products, :categories, :cat_entries, :assets

  def initialize
    puts 'Parsing XML document'
    tables = parse_xml_doc ARGV.join ' '
    puts 'Extracting field names table'
    @names = build_names tables[:Names__Row]
    puts 'Extracting metadata table, resolving names'
    @metadata = build_metadata tables[:ProductMetadataFieldValues__Row]
    puts 'Extracting products table'
    @products = build_products tables[:Products__Row]
    puts 'Extracting categories table'
    @categories = build_cats tables[:ProductCatalogCategories__Row]
    puts 'Extracting catalog entries table'
    @cat_entries = build_cat_entries tables[:ProductCatalogEntries__Row]
    puts 'Extracting digital assets table'
    @assets = build_all_attrs tables[:DigitalAssets__Row]
  end

  private

  # extract desired tables from XML
  def parse_xml_doc(xml_file_name)
    table_names = %w(
      Names__Row ProductMetadataFieldValues__Row Products__Row
      ProductCatalogCategories__Row ProductCatalogEntries__Row
      DigitalAssets__Row)
    tables = {}
    table_names.each { |e| tables[e.to_sym] = [] }
    File.open(xml_file_name) do |f|
      f.lazy.each.with_index do |line, i|
        type = table_names.find { |e| line.include? e }
        tables[type.to_sym] << line if type
      end
    end
    tables
  end

  # Get all attributes from a line
  def build_all_attrs(table)
    hash = {}
    table.each do |line|
      id, attrs = nil, {}
      line.sub(" />\n", '').split(' PFWeb:').drop(1).each.with_index do |e, i|
        match = e.match(/(?<name>\S*)="?(?<val>.*)"/)
        id = match[:val].to_sym if i == 0
        attrs[match[:name].to_sym] = match[:val]
      end
      hash[id] = attrs
    end
    hash
  end

  # Build names
  def build_names(names_table)
    names = {}
    names_table.each do |line|
      x = line.match(/NameID__ID="(.*?)".*StringValue__STR="(.*?)"/)
      names[x[1].to_sym] = x[2] if x
    end
    names
  end

  # Build metadata
  def build_metadata(metadata_table)
    meta = {}
    metadata_table.each do |line|
      match = line.match(
        /ProductID__IDREF="(?<prod>.*?)"
        .*FieldNameID__IDREF="(?<field>.*?)"
        (.*FieldValue__STR="(?<val>.*?)")?/x)
      name = "metadata_#{names[match[:field].to_sym]}".to_sym
      x = meta.fetch match[:prod].to_sym, {}
      x[name] = match[:val] ? match[:val] : ''
      meta[match[:prod].to_sym] = x
    end
    meta
  end

  # Build products
  def build_products(prods_table)
    build_all_attrs prods_table
  end

  # Build categories
  def build_cats(cats_table)
    build_all_attrs cats_table
  end

  # Build catalog entries
  def build_cat_entries(entries_table)
    entries = {}
    entries_table.each do |line|
      match = line.match(/ProductID__IDREF="(?<prod>\S*?)".*
                         ParentCategoryID__IDREF="(?<parent>\S*?)"/x)
      key = match[:prod].to_sym
      entries[key] = (entries.fetch key, []) << match[:parent]
    end
    entries
  end
end
