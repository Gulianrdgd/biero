defmodule BieroWeb.UserController do
  use BieroWeb, :controller

  alias Biero.{User, Repo, Team}

  def createSuperUser() do
    username = "julianAdmin"
    token = Phoenix.Token.sign(BieroWeb.Endpoint, "user auth", username)
    userSet = %User{}
    changeset = User.changeset(userSet, %{username: username, password_hash: "test", token: token, hasAdmin: true, team: "admins"})
    Repo.insert(changeset)
  end

  def createTeamTest() do
    username = "testTeam"
    users = ["Julian", "Nog een Keer Julian"]
    teamSet = %Team{}
    changeset = Team.changeset(teamSet, %{name: username, users: users, etappe: 0, color: "#E62272"})
    Repo.insert(changeset)
  end
end
