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
    field :color, :string

    timestamps()
  end

  @doc false
  def changeset(team, attrs) do
    team
    |> cast(attrs, [:name, :users, :etappe, :color])
    |> validate_required([:name, :users, :etappe, :color])
  end

  def getTeams() do
    _result = Repo.all(from t in Team,
                       select: map(t, [:name, :users, :etappe, :color]))
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
                case x["color"] do
                  "" ->  changeset = Team.changeset(user, %{name: x["team"], users: String.split(x["users"], ","), etappe: (x["etappe"] + user.etappe)})
                           Repo.update(changeset)
                  "none" -> changeset = Team.changeset(user, %{name: x["team"], users: String.split(x["users"], ","), etappe: (x["etappe"] + user.etappe), color: getRandomHex()})
                            Repo.update(changeset)
                  col -> changeset = Team.changeset(user, %{name: x["team"], users: String.split(x["users"], ","), etappe: (x["etappe"] + user.etappe), color: col})
                           Repo.update(changeset)
                end
            end
        false ->
            teamSet = %Team{}
            changeset = Team.changeset(teamSet, %{name: x["team"], users: String.split(x["users"], ","), etappe: (x["etappe"]), color: getRandomHex()})
            Repo.insert(changeset)
      end
    end)
    _result = getTeams()
  end

  def getRandomHex() do
    "#" <> Integer.to_charlist(:rand.uniform(255), 16) <> Integer.to_charlist(:rand.uniform(255), 16) <> Integer.to_charlist(:rand.uniform(255), 16)
  end

end
