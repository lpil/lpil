require 'spreadsheet'
require 'pageflex_report.rb'

# Report with our custom changes
class CustomPageflexReport < PageflexReport
  def initialize(current_products, columns)
    @columns = columns
    super
    shift # Remove header
    strip_html
    sort_by_uk_date
  end

  def write(file_name = "mmstore_report_#{Time.new.strftime '%Y_%m_%d'}.xls")
    puts "Writing to '#{file_name}'"
    book = Spreadsheet::Workbook.new
    write_sheets book
    header = @columns.reduce([]) { |a, e| a << e[0] }
    book.sheet_count.times { |n| book.worksheet(n).replace_row 0, *header }
    book.write file_name
  end

  private

  def write_sheets(book)
    sheet_1w = book.create_worksheet name: '1 Week'
    sheet_6w = book.create_worksheet name: '6 weeks'
    sheet_mo = book.create_worksheet name: 'More'
    sheet_ex = book.create_worksheet name: 'Expired'
    each do |row|
      if row[2].match(%r{\d+/\d+/\d+})
        date = row[2].split('/').reverse.join ''
        if date <= Time.now.strftime('%Y%m%d')
          sheet_ex.insert_row sheet_ex.count + 1, row
        elsif date <= (Time.now + (7 * 24 * 60 * 60)).strftime('%Y%m%d')
          sheet_1w.insert_row sheet_ex.count + 1, row
        elsif date <= (Time.now + (6 * 7 * 24 * 60 * 60)).strftime('%Y%m%d')
          sheet_6w.insert_row sheet_ex.count + 1, row
        else
          sheet_mo.insert_row sheet_ex.count + 1, row
        end
      else
        sheet_mo.insert_row sheet_ex.count + 1, row
      end
    end
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
