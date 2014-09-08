#
# Periodic Tasks
#

#  Delete mailings more than 90 days old from the database
$threads[:delete_old_mailings] = every 1.hour do
  Mailing.delete_all(['date_sent < ?', 90.days.ago])
end
