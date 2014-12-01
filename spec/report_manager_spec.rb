require_relative 'spec_helper'

describe ReportManager do
  describe '#fetch_new_reports' do
    it 'Does lot of stuff', pending: 'How do I test this?' do
      fail
    end

    describe '#add_fetched_reports' do
      context 'with some reports to add' do
        it 'saves the reports' do
          manager = ReportManager.new FTP_CONFIG

          expect(manager).to receive(:unparsed_reports)
            .and_return(%w(1.OUT 2.OUT 3.OUT))

          expect(File).to receive(:read)
            .exactly(3).times
            .and_return ',1,,,,,,,,,,11/11/2011,1'

          report = double 'Report'
          expect(Report).to receive(:new)
            .exactly(3).times
            .and_return report
          expect(report).to receive(:save)
            .exactly(3).times

          expect(File).to receive(:rename)
            .exactly(3).times

          manager.add_fetched_reports
        end
      end
    end
  end
end
