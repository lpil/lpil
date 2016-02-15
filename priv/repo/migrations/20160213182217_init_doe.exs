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

    create table(:articles) do
      add :title, :string
      add :slug,  :string
      add :body,  :text
      add :published_at, :datetime, null: false
      timestamps
    end
    create unique_index(:articles, [:slug])
  end
end
