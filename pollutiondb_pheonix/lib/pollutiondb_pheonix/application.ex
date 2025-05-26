defmodule PollutiondbPheonix.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      PollutiondbPheonixWeb.Telemetry,
      {DNSCluster, query: Application.get_env(:pollutiondb_pheonix, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: PollutiondbPheonix.PubSub},
      # Start the Finch HTTP client for sending emails
      {Finch, name: PollutiondbPheonix.Finch},
      # Start a worker by calling: PollutiondbPheonix.Worker.start_link(arg)
      # {PollutiondbPheonix.Worker, arg},
      # Start to serve requests, typically the last entry
      PollutiondbPheonixWeb.Endpoint
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: PollutiondbPheonix.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    PollutiondbPheonixWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
