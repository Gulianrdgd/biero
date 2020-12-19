defmodule Biero.Repo do
  use Ecto.Repo,
    otp_app: :biero,
    adapter: Ecto.Adapters.Postgres
end
