create or replace function type_exists(type_name text) 
returns boolean as $$
begin
  return exists (select 1 from pg_type where typname = type_name);
end 
$$ language plpgsql;

do $$ begin
if not type_exists('gleam_sample_kind')
then
  create type gleam_sample_kind as enum (
    'sponsorship_per_month',
    'sponsor_count',
    'compiler_github_stars',
    'discord_size',
    'stdlib_downloads_all',
    'stdlib_downloads_recent',
    'website_visitors_30d',
    'website_views_30d',
    'exercism_students',
    'exercism_runs',
    'exercism_descussions'
  );
end if;
end $$;

-- Time series data relating to Gleam
create table if not exists gleam_samples (
  kind gleam_sample_kind not null,
  value double precision not null,
  time timestamp with time zone not null,
  primary key (time, kind)
);

do $$ begin
if not type_exists('starling_transaction_direction')
then
  create type starling_transaction_direction as enum ('IN', 'OUT');
end if;
end $$;

do $$ begin
if not type_exists('starling_transaction_status')
then
  create type starling_transaction_status as enum (
    'UPCOMING',
    'PENDING',
    'REVERSED',
    'SETTLED',
    'DECLINED',
    'REFUNDED',
    'RETRYING',
    'ACCOUNT_CHECK'
  );
end if;
end $$;

create or replace function timestampz_from_iso8601(input text)
returns timestamp with time zone as $$
begin
  return to_timestamp(input,'YYYY-MM-DDThh24:mi:ss.ms')::timestamp without time zone at time zone 'Etc/UTC';
end $$ language plpgsql;

create table if not exists starling_transactions (
  uid text primary key,
  amount_minor_units integer not null,
  source_amount_currency text not null,
  source_amount_minor_units integer not null,
  updated_at timestamp with time zone not null,
  direction starling_transaction_direction not null,
  transaction_time timestamp with time zone not null,
  source text not null,
  status starling_transaction_status not null,
  counter_party_type text,
  counter_party_uid text,
  counter_party_name text not null,
  reference text,
  country text not null,
  spending_category text not null,
  user_note text
);

drop view if exists simple_starling_transactions;
create view simple_starling_transactions as
select
  uid,
  case
    when direction = 'IN' then amount_minor_units
    else -amount_minor_units
  end as amount,
  transaction_time as time,
  source,
  counter_party_name as counter_party,
  reference,
  spending_category,
  user_note
from
  starling_transactions
where
  status = 'SETTLED'
