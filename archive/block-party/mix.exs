defmodule BlockParty.Mixfile do
  use Mix.Project

  def project do
    [app: :block_party,
     version: "0.0.1",
     elixir: "~> 1.0",
     elixirc_paths: elixirc_paths(Mix.env),
     compilers: [:phoenix, :gettext] ++ Mix.compilers,
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  def application do
    [mod: {BlockParty, []},
     applications: [:phoenix, :phoenix_html, :cowboy, :logger, :gettext]]
  end

  defp elixirc_paths(:test), do: ["lib", "web", "test/support"]
  defp elixirc_paths(_),     do: ["lib", "web"]

  defp deps do
    [
      # Web framework
      {:phoenix, "~> 1.1.4"},
      # HTML view helpers
      {:phoenix_html, "~> 2.4"},
      # Page reloading in dev
      {:phoenix_live_reload, "~> 1.0", only: :dev},
      # Translation macros
      {:gettext, "~> 0.9"},
      # Web server
      {:cowboy, "~> 1.0"},

      # Code style linter
      {:dogma, "~> 0.0", only: [:dev, :test]},
      # Automatic test runner
      {:mix_test_watch, "~> 0.0", only: :dev},
    ]
  end
end
