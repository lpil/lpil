create or replace function type_exists(type_name text) 
returns boolean as $$
begin
  return exists (select 1 from pg_type where typname = type_name);
end 
$$ language plpgsql;

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

create table if not exists starling_transactions (
  uid text primary key,
  amount_currency text not null,
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
  total_fees integer,
  total_fee_amount_currency text,
  total_fee_amount_minor_units integer,
  reference text,
  country text not null,
  spending_category text not null,
  user_note text
);
