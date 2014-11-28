class ReportManager
  def initialize(ftp_config)
    @ftp_config = ftp_config
  end

  def fetch_new_reports
    Net::FTP.open(*@ftp_config) do |ftp|
      ftp.passive = true

      new_reports = ftp.nlst.select do |f|
        f.match(/\.OUT\z/) && !(parsed_reports.include? f)
      end

      new_reports.each do |remote_file_name|
        ftp.gettextfile remote_file_name,
                        "./reports/#{remote_file_name}"
      end
    end
  end

  def add_fetched_reports
    Database.establish_connection

    unparsed_reports.each do |report|
      Report.new(File.read report).save_to_db
      File.rename f, "#{File.dirname f}/parsed/#{File.basename f}"
    end
  end

  def remove_old_reports_from_db(time_ago = 3.months.ago)
    Database.establish_connection
    Mailing.delete_all(['date_sent < ?', time_ago])
  end

  def remove_old_reports_from_ftp(time_ago = 3.months.ago)
    Net::FTP.open(*@ftp_config) do |ftp|
      ftp.nlst.each do |report|
        ftp.delete report if ftp.mtime(report) < time_ago
      end
    end
  end

  def remove_old_reports_from_local_dir(time_ago = 3.months.ago)
    Dir.glob("#{this_dir}/reports/parsed/*").each do |report|
      File.delete report if File.mtime(report) < time_ago
    end
  end

  private

  def unparsed_reports
    Dir.glob('./reports/*.OUT').map { |f| File.basename f }
  end

  def parsed_reports
    Dir.glob('./reports/parsed/*.OUT').map { |f| File.basename f }
  end
end
