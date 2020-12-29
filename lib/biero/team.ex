defmodule Biero.Team do
  use Ecto.Schema
  import Ecto.Changeset
  import Ecto.Query
  alias Biero.{Team, Repo}
  require Logger

  schema "teams" do
    field :name, :string
    field :users, {:array, :string}
    field :etappe, :integer

    timestamps()
  end

  @doc false
  def changeset(team, attrs) do
    team
    |> cast(attrs, [:name, :users, :etappe])
    |> validate_required([:name, :users, :etappe])
  end

  def getTeams() do
    _result = Repo.all(from t in Team,
                       select: map(t, [:name, :users, :etappe]))
  end

  def setTeam(changes) do
    _side = Enum.map(changes, fn x ->
      case Team |> Ecto.Query.where(name: ^x["team"]) |> Repo.exists? do
        true ->
            case x["delete"] do
              true ->
                _result = Team |> Ecto.Query.where(name: ^x["team"]) |> Repo.delete_all
              _ ->
                user = Team |> Ecto.Query.where(name: ^x["team"]) |> Repo.one
                changeset = Team.changeset(user, %{name: x["team"], users: String.split(x["users"], ","), etappe: (x["etappe"] + user.etappe)})
                Repo.update(changeset)
            end
        false ->
            teamSet = %Team{}
            changeset = Team.changeset(teamSet, %{name: x["team"], users: String.split(x["users"], ","), etappe: (x["etappe"])})
            Repo.insert(changeset)
      end
    end)
    _result = getTeams()
  end
end
