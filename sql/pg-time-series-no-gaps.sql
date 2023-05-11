-- https://sqlfordevs.com/statistical-results-fill-gaps
select dates_without_gaps.day, coalesce(sum(statistics.count), 0)
from generate_series(
  current_date - interval '14 days',
  current_date,
  '1 day'
) as dates_without_gaps(day)
left join statistics on(statistics.day = dates_without_gaps.day)
group by dates_without_gaps.day;
