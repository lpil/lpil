-- Insert or update a starling_transactions record
insert into starling_transactions
( uid, amount_minor_units, source_amount_currency
, source_amount_minor_units, updated_at, direction, transaction_time, source
, status, counter_party_type, counter_party_uid, counter_party_name, reference
, country, spending_category, user_note
)
values
( $1, $2, $3, $4, timestampz_from_iso8601($5), $6
, timestampz_from_iso8601($7), $8, $9, $10, $11, $12, $13, $14, $15, $16
)
on conflict (uid)
do update set
  amount_minor_units = excluded.amount_minor_units
, source_amount_currency = excluded.source_amount_currency
, source_amount_minor_units = excluded.source_amount_minor_units
, updated_at = excluded.updated_at
, direction = excluded.direction
, transaction_time = excluded.transaction_time
, source = excluded.source
, status = excluded.status
, counter_party_type = excluded.counter_party_type
, counter_party_uid = excluded.counter_party_uid
, counter_party_name = excluded.counter_party_name
, reference = excluded.reference
, country = excluded.country
, spending_category = excluded.spending_category
, user_note = excluded.user_note
