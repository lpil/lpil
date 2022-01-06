do $$
declare
  command varchar;
begin
  select 'truncate table ' || string_agg(oid::regclass::text, ', ') || ' cascade'
  into command
  from pg_class
  where relkind = 'r'  -- only tables
  and relnamespace = 'public'::regnamespace;

  raise notice 'Running `%`', command;
  execute command;
  raise notice 'Done';
end
$$;
