defmodule Biero.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application
  use Supervisor

  def start(_type, _args) do
    children = [
      # Start the Ecto repository
      Biero.Repo,
      # Start the Telemetry supervisor
      BieroWeb.Telemetry,
      # Start the PubSub system
      {Phoenix.PubSub, name: Biero.PubSub},
      # Start the Endpoint (http/https)
      BieroWeb.Endpoint,
      # Start a worker by calling: Biero.Worker.start_link(arg)
      # {Biero.Worker, arg}
      worker(Biero.ChannelWatcher, [:chat])
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Biero.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    BieroWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
