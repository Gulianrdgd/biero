defmodule Biero.Repo.Migrations.CreateTeams do
  use Ecto.Migration

  def change do
    create table(:teams) do
      add :name, :string
      add :users, {:array, :string}
      add :etappe, :integer

      timestamps()
    end

  end
end
