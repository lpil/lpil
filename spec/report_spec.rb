require_relative 'spec_helper'

describe Report do
  before :all do
    @csv = lambda do |e|
      ",#{e[:order_ref]},,,,,,,,,,#{e[:date].strftime '%d/%m/%y'},"\
      "#{e[:dpd_ref]}"
    end

    @rep0 = {
      order_ref: 'ref312',
      dpd_ref:   'ref123',
      date:       Date.today
    }
  end

  describe '#new' do
    it 'correctly parse the report string' do
      parsed_rep = Report.new @csv.call(@rep0)
      expected = {
        order_ref: @rep0[:order_ref],
        is_post:   false,
        dpd_ref:   @rep0[:dpd_ref],
        date_sent: @rep0[:date]
      }
      expect(parsed_rep.instance_variable_get :@report).to match(expected)
    end

    it 'parses ambiguous dates correctly' do
      rep = @rep0
      rep[:date] = Date.strptime '10', '%d'
      csv = @csv.call(rep)

      parsed_rep = Report.new csv
      expected = {
        order_ref: rep[:order_ref],
        is_post:   false,
        dpd_ref:   rep[:dpd_ref],
        date_sent: rep[:date]
      }
      expect(parsed_rep.instance_variable_get :@report).to match(expected)
    end
  end

  describe '#save' do
    it 'saves @report to the db' do
      report = Report.new(@csv.call(@rep0))

      expect(Mailing)
        .to receive(:create)
        .with(report.instance_variable_get(:@report))

      report.save
    end
  end
end
