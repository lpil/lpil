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
      timestamps()
    end

    create(unique_index(:users, [:email]))

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
      timestamps()

      user_ref = references(:users, on_delete: :delete_all, type: :uuid)
      add(:user_id, user_ref, null: false)

      org_ref = references(:organisations, on_delete: :delete_all, type: :uuid)
      add(:organisation_id, org_ref, null: false)
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
