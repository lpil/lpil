select
  count(*) filter (where releasedate = 2001) as released_2001,
  count(*) filter (where releasedate = 2002) as released_2002,
  count(*) filter (where director = 'steven spielberg') as director_stevenspielberg,
  count(*) filter (where director = 'james cameron') as director_jamescameron,
  jsonb_agg(distinct genre) filter (where director = 'quentin tarantino') as genres_quentintarantino
from movies
where streamingservice = 'netflix';
