#
# Periodic Tasks
#

# Check the DPD report FTP for new reports, and add them to the database
$threads[:fetch_dpd_reports] = every 1.hour do
  Mailing.fetch_new_reports!
end

#  Delete mailings more than 90 days old from the database
$threads[:delete_old_mailings] = every 1.hour do
  Mailing.delete_all(['date_sent < ?', 90.days.ago])
end
