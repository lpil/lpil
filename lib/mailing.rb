class Mailing < ActiveRecord::Base
  validates_uniqueness_of :order_ref
  validates_presence_of :order_ref, :date_sent
  validates_inclusion_of :is_post, in: [true, false]
end


class Report
  def initialize(report_string)
    report_string = report_string.force_encoding('BINARY')

    report = CSV.parse_line report_string

    @report = {
      order_ref: report[1].force_encoding('UTF-8'),
      is_post:   false,
      dpd_ref:   report[12].force_encoding('UTF-8'),
      date_sent: Date.strptime(report[11], '%d/%m/%y')
    }
  end

  def save_to_db
    Mailing.create(@report)
  end
end
