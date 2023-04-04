-- A trigger function function to update the updated_at column to the current
-- time. This is to be used in a trigger on the tables to be updated.
create or replace function update_updated_at()
  returns trigger as $$
begin
  new.updated_at = now() at time zone 'utc';
  return new;
end
$$ language plpgsql;

-- Automatically update the updated_at column when a row is updated.
create trigger users_updated_at
  before update on users
  for each row
  execute procedure update_updated_at();
