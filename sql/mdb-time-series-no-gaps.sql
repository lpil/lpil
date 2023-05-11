-- https://sqlfordevs.com/statistical-results-fill-gaps
with recursive dates_without_gaps(day) as (
  select date_sub(current_date, interval 14 day) as day
  union all
  select date_add(day, interval 1 day) as day
  from dates_without_gaps
  where day < current_date
)
select dates_without_gaps.day, coalesce(sum(statistics.count), 0)
from dates_without_gaps
left join statistics on(statistics.day = dates_without_gaps.day)
group by dates_without_gaps.day;
