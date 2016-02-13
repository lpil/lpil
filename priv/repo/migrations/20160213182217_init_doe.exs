defmodule Fawkes.Repo.Migrations.InitDoe do
  use Ecto.Migration

  def change do
    create table(:users) do
      add :email,    :string
      add :username, :string
      add :password_hash, :string
      timestamps
    end
    create unique_index(:users, [:email])
    create unique_index(:users, [:username])
  end
end
