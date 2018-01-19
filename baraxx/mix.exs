defmodule Baraxx.MixProject do
  use Mix.Project

  def project do
    [
      app: :baraxx,
      version: "0.1.0",
      elixir: "~> 1.6",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {Baraxx.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # Web app API
      {:raxx, "~> 0.13"},
      # Web server
      {:ace, "~> 0.15"}
    ]
  end
end
