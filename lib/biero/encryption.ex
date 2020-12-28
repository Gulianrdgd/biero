defmodule Biero.Encryption do
  alias Bcrypt
  alias Biero.{User}
  require Logger

  def hash_password(password), do: Bcrypt.add_hash(password)[:password_hash]

  def validate_password(%User{} = user, pass) do
    case Bcrypt.check_pass(user, pass) do
      {:ok, xs} ->
        true
      _ ->
        false
    end
  end

  def checkToken(username, token) do
    case Phoenix.Token.verify(BieroWeb.Endpoint, "user auth", token, max_age: 86400) do
        {:ok, ^username} -> true
        _ -> false
    end
  end

end
