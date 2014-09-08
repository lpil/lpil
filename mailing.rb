# The Mailing model for the DPD + post deliveries.
class Mailing < ActiveRecord::Base
  validates_uniqueness_of :order_ref
  validates_presence_of :order_ref, :date_sent
  validates_inclusion_of :is_post, in: [true, false]

  def self.add_dpd_report(report)
    Mailing.create(
      Mailing.parse_dpd_report_string report.force_encoding('BINARY')
    )
  end

  # Used by fetch_new_reports
  def self.parse_dpd_report_string(report)
    report = CSV.parse_line(report)

    # TODO
    # FIXME
    fail "The report format isn't standardised! Talk to Dave"

    {
      order_ref: report[1].force_encoding('UTF-8'),
      is_post:   false,
      dpd_ref:   report[11].force_encoding('UTF-8'),
      date_sent: Time.parse(report[12])
    }
  end
end
