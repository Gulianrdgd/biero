defmodule BieroWeb.PageController do
  use BieroWeb, :controller
  import Ecto.Query, warn: false
  alias Biero.{User, Encryption, Repo}
  require Logger

  def index(conn, _params) do
    render(conn, "index.html")
  end

  def dash(conn, _params) do
    render(conn, "dash.html", wrongPass: false )
  end

  def checkPass(conn, _params) do
    con = parse(conn)
    username = con.params["username"]
    password = con.params["password"]
    case User |> Ecto.Query.where(username: ^username) |> Repo.exists? do
      true ->
          user = User |> Ecto.Query.where(username: ^username) |> Repo.one
          case Encryption.validate_password(user, password) and user.hasAdmin do
            true ->
              token = Phoenix.Token.sign(BieroWeb.Endpoint, "user auth", username)
              changeset = User.changeset(user, %{token: token})
              Repo.update(changeset)
              render(conn, "dashAdmin.html", token: token, username: username)
            _ ->
              render(conn, "dash.html", wrongPass: true)
          end
      _ ->
        render(conn, "dash.html", wrongPass: true)
    end
  end

  def parse(conn, opts \\ []) do
    opts = Keyword.put_new(opts, :parsers, [Plug.Parsers.URLENCODED, Plug.Parsers.MULTIPART])
    Plug.Parsers.call(conn, Plug.Parsers.init(opts))
  end
end
