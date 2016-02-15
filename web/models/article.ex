defmodule Fawkes.Article do
  @moduledoc """
  A person what can log in.
  """
  use Fawkes.Web, :model

  schema "articles" do
    field :title, :string
    field :slug,  :string
    field :body,  :string
    field :published_at, Ecto.DateTime
    timestamps
  end

  @required_params ~w(title slug body published_at)
  @optional_params ~w()

  @slug_regex ~r/\A[a-z0-9-]+\z/


  @doc """
  Create a changeset for the given params, and perform data validations.
  """
  def changeset(model, params \\ :empty) do
    model
    |> cast(params, @required_params, @optional_params)
    |> validate_length(:title, min: 6)
    |> unique_constraint(:slug)
    |> validate_format(:slug, @slug_regex)
    |> validate_length(:body, min: 50)
  end
end

defimpl Phoenix.Param, for: Fawkes.Article do
  def to_param(%Fawkes.Article{ slug: slug }) when is_binary(slug) do
    slug
  end
end
