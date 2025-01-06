# In this file, we load production configuration and secrets
# from environment variables. You can also hardcode secrets,
# although such is generally not recommended and you have to
# remember to add this file to your .gitignore.
use Mix.Config

config :biero, Biero.Repo,
  username: "Biero",
  password: "Yk42WV5JoWWYJvOSBphtG29p7JfZMTpR",
  database: "biero_prod",
  hostname: "localhost",
  show_sensitive_data_on_connection_error: true,
  pool_size: 10


secret_key_base ="jw2EKL/XqofGmTI1NwmqF+zhrP28Kf5tG9rLDVJfbOb6K+KC1go38HhogTPyrM/R"

config :biero, BieroWeb.Endpoint,
  http: [
    port: String.to_integer("6971"),
    transport_options: [socket_opts: [:inet6]]
  ],
  secret_key_base: secret_key_base

# ## Using releases (Elixir v1.9+)
#
# If you are doing OTP releases, you need to instruct Phoenix
# to start each relevant endpoint:
#
#     config :biero, BieroWeb.Endpoint, server: true
#
# Then you can assemble a release by calling `mix release`.
# See `mix help release` for more information.
