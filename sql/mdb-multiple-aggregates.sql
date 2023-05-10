select
  sum(releasedate = 2001) as released_2001,
  sum(releasedate = 2002) as released_2002,
  sum(director = 'steven spielberg') as director_stevenspielberg,
  sum(director = 'james cameron') as director_jamescameron
from movies
where streamingservice = 'netflix';
