require 'csv'

# Builds our report CSV
class PageflexReport < Array
  def initialize(current_products, columns)
    build_report current_products, columns
  end

  def write_csv
    file_name = "mmstore_report_#{Time.new.strftime '%Y_%m_%d'}.csv"
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
    # Add header
    unshift columns.reduce([]) { |a, e| a << e[0] }
  end
end
