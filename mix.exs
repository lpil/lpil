defmodule Fcat.Mixfile do
  use Mix.Project

  def project do
    [
      app: :fcat,
      version: "0.1.0",
      elixir: "~> 1.4",
      elixirc_paths: elixirc_paths(Mix.env()),
      compilers: [:phoenix, :gettext] ++ Mix.compilers(),
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Configuration for the OTP application.
  #
  # Type `mix help compile.app` for more information.
  def application do
    [
      mod: {Fcat.Application, []},
      extra_applications: [:logger, :runtime_tools]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [
      # Web framework
      {:phoenix, "~> 1.3.2"},
      {:phoenix_pubsub, "~> 1.0"},
      {:phoenix_html, "~> 2.10"},
      {:phoenix_live_reload, "~> 1.0", only: :dev},
      # Internationalisation
      {:gettext, "~> 0.11"},
      # Web server
      {:cowboy, "~> 1.0"},
      # Performance monitoring
      {:appsignal, "~> 1.0"},
      # Automatic test runner
      {:mix_test_watch, "~> 0.4", only: [:dev], runtime: false},
      # Release builder
      {:distillery, "~> 1.5", runtime: false}
    ]
  end
end
