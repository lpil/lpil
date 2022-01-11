job_type :bin, 'cd :path && bundle exec bin/:task'

every :hour, at: 00 do
  bin 'fetch_new_reports.rb'
end

every :hour, at: 01 do
  bin 'parse_fetched_reports.rb'
end

every :day, at: '12:34am' do
  bin 'remove_old_reports.rb'
end
