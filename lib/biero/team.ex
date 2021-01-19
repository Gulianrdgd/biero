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
    _result = Repo.all(
      from t in Team,
      select: map(t, [:name, :users, :etappe, :color])
    )
  end

  def setTeam(changes) do
    _side = Enum.map(
      changes,
      fn x ->
        case Team
             |> Ecto.Query.where(name: ^x["team"])
             |> Repo.exists? do
          true ->
            case x["delete"] do
              true ->
                _result = Team
                          |> Ecto.Query.where(name: ^x["team"])
                          |> Repo.delete_all
              _ ->
                team = Team
                       |> Ecto.Query.where(name: ^x["team"])
                       |> Repo.one
                case x["color"] do
                  "" ->
                    changeset = Team.changeset(
                      team,
                      %{name: x["team"], users: String.split(x["users"], ","), etappe: (x["etappe"] + team.etappe)}
                    )
                    Repo.update(changeset)
                  "none" ->
                    changeset = Team.changeset(
                      team,
                      %{
                        name: x["team"],
                        users: String.split(x["users"], ","),
                        etappe: (x["etappe"] + team.etappe),
                        color: getRandomHex()
                      }
                    )
                    Repo.update(changeset)
                  col ->
                    changeset = Team.changeset(
                      team,
                      %{
                        name: x["team"],
                        users: String.split(x["users"], ","),
                        etappe: (x["etappe"] + team.etappe),
                        color: col
                      }
                    )
                    Repo.update(changeset)
                end
            end
          false ->
            teamSet = %Team{}
            changeset = Team.changeset(
              teamSet,
              %{name: x["team"], users: String.split(x["users"], ","), etappe: (x["etappe"]), color: getRandomHex()}
            )
            Repo.insert(changeset)
        end
      end
    )
    _result = getTeams()
  end

  def getRandomHex() do
    "#" <> fixedSize(:rand.uniform(255)) <> fixedSize(:rand.uniform(255)) <> fixedSize(:rand.uniform(255))
  end

  def fixedSize(number) do
    if number < 10 do
      "0" <> Integer.to_string(number, 16)
    else
      Integer.to_string(number, 16)
    end
  end

end
