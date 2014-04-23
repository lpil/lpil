# Selects current versions of products and attaches data
class CurrentProducts < Hash
  def initialize(pf_products, pf_metadata, pf_cat_entries, categories)
    puts 'Identifying current products, adding metadata and categories'
    load_current_products pf_products, pf_cat_entries
    attach_metadata pf_metadata
    attach_categories pf_cat_entries, categories
  end

  private

  def load_current_products(pf_products, pf_cat_entries)
    pf_products.each do |p|
      self[p[0]] = p[1] if pf_cat_entries.key? p[0]
    end
  end

  def attach_metadata(metadata)
    values.each do |p|
      p.merge! metadata.fetch p[:ProductID__ID].to_sym, {}
    end
  end

  def attach_categories(pf_cat_entries, categories)
    pf_cat_entries.each do |e|
      e[1].each_with_index do |c, i|
        self[e[0]]["Category#{i}".to_sym] = categories[c.to_sym][:Path]
      end
    end
  end
end
