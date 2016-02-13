defmodule Fawkes.Mixfile do
  use Mix.Project

  def project do
    [
      app: :fawkes,
      version: "0.0.1",
      elixir: "~> 1.2",
      elixirc_paths: elixirc_paths(Mix.env),
      compilers: [:phoenix, :gettext] ++ Mix.compilers,
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      aliases: aliases,
      deps: deps
    ]
  end

  def application do
    [
      mod: {Fawkes, []},
      applications: [
        :phoenix, :phoenix_html, :cowboy, :logger, :gettext, :phoenix_ecto,
        :postgrex
      ]
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "web", "test/support"]
  defp elixirc_paths(_),     do: ["lib", "web"]

  defp deps do
    [
      # Web framework
      {:phoenix, "~> 1.1.4"},
      # Postgres database connector
      {:postgrex, ">= 0.0.0"},
      # SQL query DSL
      {:phoenix_ecto, "~> 2.0"},
      # HTML template helpers
      {:phoenix_html, "~> 2.4"},
      # Inject frontend changes in dev
      {:phoenix_live_reload, "~> 1.0", only: :dev},
      # Text translation macros
      {:gettext, "~> 0.9"},
      # Web server
      {:cowboy, "~> 1.0"},

      # Code style linter
      {:dogma, "~> 0.0", only: :dev},
      # Automatic test runner
      {:mix_test_watch, "~> 0.1", only: :dev},
    ]
  end

  defp aliases do
    [
      "ecto.seed": ["run priv/repo/seeds.exs"],
      "ecto.setup": ~w[ecto.create ecto.migrate ecto.seed],
      "ecto.reset": ~w[ecto.drop ecto.setup],
    ]
  end
end
