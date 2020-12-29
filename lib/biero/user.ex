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
    _result = Repo.all(from u in User,
                   select: map(u, [:username, :hasAdmin]))
  end

  def setUser(changes) do
    _side = Enum.map(changes, fn x ->
      user = Users |> Ecto.Query.where(username: ^x.username) |> Repo.one
      changeset = Users.changeset(user, %{username: x.username, hasAdmin: boolStringToInt(x.isAdmin)})
      Repo.update(changeset)
    end)
    _result = getUsers()
  end

  def boolStringToInt(bool) do
      case bool do
        "T" -> true
        "F" -> false
        _ -> false
      end
  end

end
