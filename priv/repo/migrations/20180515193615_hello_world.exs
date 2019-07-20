defmodule Boilerplate.Repo.Migrations.HelloWorld do
  use Ecto.Migration

  def up do
    #
    # Users
    #

    create table(:users, primary_key: false) do
      add(:id, :uuid, primary_key: true)
      add(:email, :string, null: false)
      add(:name, :string, null: false)
      add(:password_hash, :string)
      add(:email_confirmed_at, :utc_datetime)
      timestamps()
    end

    create(unique_index(:users, [:email]))

    #
    # Email confirmation tokens
    #

    create table(:email_confirmation_tokens, primary_key: false) do
      add(:id, :uuid, primary_key: true)

      add(
        :user_id,
        references(:users, on_delete: :delete_all, type: :uuid),
        null: false
      )

      timestamps()
    end

    create(unique_index(:email_confirmation_tokens, [:user_id]))

    #
    # Password reset tokens
    #

    create table(:password_reset_tokens, primary_key: false) do
      add(:id, :uuid, primary_key: true)
      add(:expires_at, :utc_datetime)

      add(
        :user_id,
        references(:users, on_delete: :delete_all, type: :uuid),
        null: false
      )

      timestamps()
    end

    create(unique_index(:password_reset_tokens, [:user_id]))

    #
    # Organisations
    #

    create table(:organisations, primary_key: false) do
      add(:id, :uuid, primary_key: true)
      add(:name, :string, null: false)
      timestamps()
    end

    create(unique_index(:organisations, [:name]))

    #
    # Memberships
    #

    execute("CREATE TYPE membership_type AS ENUM ('admin', 'user');")

    create table(:memberships) do
      add(:type, :membership_type, null: false, default: "user")

      add(
        :user_id,
        references(:users, on_delete: :delete_all, type: :uuid),
        null: false
      )

      add(
        :organisation_id,
        references(:organisations, on_delete: :delete_all, type: :uuid),
        null: false
      )

      timestamps()
    end

    create(unique_index(:memberships, [:user_id, :organisation_id]))
    create(index(:memberships, [:user_id]))
    create(index(:memberships, [:organisation_id]))
  end

  def down do
    drop(table(:users))
    drop(table(:organisations))
    drop(table(:memberships))
    execute("DROP TYPE membership_type;")
  end
end
