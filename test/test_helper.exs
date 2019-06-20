ExUnit.start

Mix.Task.run "ecto.create", ~w(-r Fawkes.Repo --quiet)
Mix.Task.run "ecto.migrate", ~w(-r Fawkes.Repo --quiet)
Ecto.Adapters.SQL.begin_test_transaction(Fawkes.Repo)

{:ok, _} = Application.ensure_all_started(:ex_machina)
