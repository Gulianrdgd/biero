defmodule Biero.Repo.Migrations.CreateUsers do
  use Ecto.Migration

  def change do
    create table(:users) do
      add :hasAdmin, :boolean, default: false
      add :username, :string
      add :password_hash, :string
      add :team, :string
      add :token, :string

      timestamps()
    end

  end
end
