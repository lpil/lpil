require 'pageflex_report.rb'

# Report with our custom changes
class CustomPageflexReport < PageflexReport
  def initialize(current_products, columns)
    super
    shift # Remove header
    strip_html
    sort_by_uk_date
    # Add header
    unshift columns.reduce([]) { |a, e| a << e[0] }
  end

  private

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
