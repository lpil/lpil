defmodule Particle.Repo do
  use Ecto.Repo, otp_app: :particle

  def ping? do
    match?({:ok, _}, query("select 1"))
  end

  def fetch(query, opts \\ []) do
    case one(query, opts) do
      nil -> :not_found
      val -> {:ok, val}
    end
  end

  @doc """
  Dynamically loads the repository url from the
  DATABASE_URL environment variable.
  """
  def init(_, opts) do
    {:ok, Keyword.put(opts, :url, System.get_env("DATABASE_URL"))}
  end
end
