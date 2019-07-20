defmodule Boilerplate.Mixfile do
  use Mix.Project

  def project do
    [
      app: :boilerplate,
      version: "0.1.0",
      elixir: "~> 1.4",
      elixirc_paths: elixirc_paths(Mix.env()),
      compilers: [:phoenix, :gettext] ++ Mix.compilers(),
      start_permanent: Mix.env() == :prod,
      dialyzer: [ignore_warnings: ".dialyzerignore"],
      aliases: aliases(),
      deps: deps()
    ]
  end

  # Configuration for the OTP application.
  #
  # Type `mix help compile.app` for more information.
  def application do
    [
      mod: {Boilerplate.Application, []},
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
      # Database and validation toolkit
      {:phoenix_ecto, "~> 3.2"},
      # Postgres DB client
      {:postgrex, "~> 0.13"},
      {:ecto_enum, "~> 1.0"},
      # Internationalisation
      {:gettext, "~> 0.11"},
      # Web server
      {:cowboy, "~> 1.0"},
      # Password hashing
      {:bcrypt_elixir, "~> 1.0"},
      # Email sending
      {:phoenix_swoosh, "~> 0.2"},
      # Time data structure helpers
      {:timex, "~> 3.1"},
      # Type checker
      {:dialyxir, "~> 0.5", only: [:dev], runtime: false},
      # Automatic test runner
      {:mix_test_watch, "~> 0.4", only: [:dev], runtime: false},
      # Release builder
      {:distillery, "~> 2.0", runtime: false}
    ]
  end

  defp aliases do
    [
      "ecto.seed": ["run priv/repo/seeds.exs"],
      "ecto.setup": ["ecto.create", "ecto.migrate", "ecto.seed"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      test: ["ecto.create --quiet", "ecto.migrate", "test"]
    ]
  end
end
