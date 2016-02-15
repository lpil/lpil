defmodule Fawkes.Repo.Migrations.InitDoe do
  use Ecto.Migration

  def change do
    create table(:users) do
      add :email,    :string, null: false
      add :username, :string, null: false
      add :password_hash, :string, null: false
      timestamps
    end
    create unique_index(:users, [:email])
    create unique_index(:users, [:username])

    create table(:articles) do
      add :title, :string, null: false
      add :slug,  :string, null: false
      add :body,  :text,   null: false
      add :published_at, :datetime, null: false
      timestamps
    end
    create unique_index(:articles, [:slug])
  end
end
