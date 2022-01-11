require 'csv'

# Builds our report CSV
class PageflexReport < Array
  def initialize(current_products, columns)
    build_report current_products, columns
  end

  def write(file_name = "mmstore_report_#{Time.new.strftime '%Y_%m_%d'}.csv")
    puts "Writing to '#{file_name}'"
    CSV.open(file_name, 'wb') do |csv|
      each { |row| csv << row }
    end
    self
  end

  private

  def build_report(products, columns)
    products.each do |p|
      row = []
      columns.each { |c| row << (p[1].fetch c[1], '') }
      self << row
    end
    sub_html_entities
    # Add header
    unshift columns.reduce([]) { |a, e| a << e[0] }
  end

  def sub_html_entities
    ents = [/&amp;/, '&'], [/&apos;/, "'"], [/&lt;/, '<'], [/&gt;/, '>']
    map! do |row|
      row.map! do |cell|
        ents.each { |e| cell.gsub!(e[0], e[1]) }
        cell
      end
    end
  end
end
