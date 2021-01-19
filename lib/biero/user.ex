defmodule Biero.User do
  use Ecto.Schema
  import Ecto.Changeset
  import Ecto.Query
  alias Biero.{Encryption, User, Repo}
  require Logger

  schema "users" do
    field :hasAdmin, :boolean, default: false
    field :username, :string
    field :password_hash, :string
    field :token, :string

    timestamps()
  end

  @doc false
  def changeset(user, attrs) do
    user
    |> cast(attrs, [:username, :password_hash, :token, :hasAdmin])
    |> validate_required([:username, :password_hash, :token, :hasAdmin])
    |> validate_confirmation(:password_hash)
    |> validate_format(:username, ~r/^[a-z0-9][a-z0-9]+[a-z0-9]$/i)
    |> unique_constraint(:username)
    |> encrypt_password
  end

  defp encrypt_password(changeset) do
    password = get_change(changeset, :password_hash)
    if password do
      encrypted_password = Encryption.hash_password(password)
      put_change(changeset, :password_hash, encrypted_password)
    else
      changeset
    end
  end

  def getUsers() do
    _result = Repo.all(
      from u in User,
      select: map(u, [:username, :hasAdmin])
    )
  end

  def setUser(changes, token) do
    _side = Enum.map(
      changes,
      fn x ->
        Logger.info(token)

        isAdmin = boolStringToBool(x["isAdmin"])
        if User
           |> Ecto.Query.where(token: ^token, hasAdmin: true)
           |> Repo.exists? do
          if User
             |> Ecto.Query.where(username: ^x["username"])
             |> Repo.exists? do
            user = User
                   |> Ecto.Query.where(username: ^x["username"])
                   |> Repo.one
            if x["delete"] do
              _result = User
                        |> Ecto.Query.where(username: ^x["username"])
                        |> Repo.delete_all
            else
              if x["password"] == "" do
                changeset = Users.changeset(user, %{username: x["username"], hasAdmin: isAdmin})
                Repo.update(changeset)
              else
                changeset = Users.changeset(
                  user,
                  %{username: x["username"], hasAdmin: isAdmin, password_hash: x["password"]}
                )
                Repo.update(changeset)
              end
            end
          else
            token = Phoenix.Token.sign(BieroWeb.Endpoint, "user auth", x["isAdmin"])
            changeset = User.changeset(
              %User{},
              %{username: x["username"], password_hash: x["password"], token: token, hasAdmin: isAdmin, team: "admins"}
            )
            Repo.insert(changeset)
          end
        else
          us = User
               |> Ecto.Query.where(token: ^token, hasAdmin: false)
               |> Repo.one
          if x["username"] == us.username  && x["password"] != "" do
            changeset = User.changeset(
              us,
              %{password_hash: x["password"]}
            )
            Repo.update(changeset)
          end
        end
      end
    )
    _result = getUsers()
  end

  def boolStringToBool(bool) do
    case bool do
      "T" -> true
      "F" -> false
      _ -> false
    end
  end

end
