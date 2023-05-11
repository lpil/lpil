-- https://sqlfordevs.com/non-overlapping-time-ranges

create extension btree_gist;
create table rooms (
  room int,
  period tstzrange,
  exclude using gist (room with =, period with &&)
);

insert into rooms (room, period) values (
  5, '[2023-03-27 16:00:00+00,2023-03-28 10:00:00+00]'
); -- ok: 1 row affected
insert into rooms (room, period) values (
  5, '[2023-03-28 16:00:00+00,2023-03-29 10:00:00+00]'
); -- ok: 1 row affected
insert into rooms (room, period) values (
  5, '[2023-03-28 18:00:00+00,2023-03-31 10:00:00+00]'
); -- error: conflicting value
insert into rooms (room, period) values (
  6, '[2023-03-28 18:00:00+00,2023-03-31 10:00:00+00]'
); -- ok: 1 row affected
