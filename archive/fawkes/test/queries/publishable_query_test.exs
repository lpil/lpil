defmodule Fawkes.PublishableQueryTest do
  use Fawkes.QueryCase

  alias Fawkes.Article
  alias Fawkes.PublishableQuery

  test "published/1 gets published entities" do
    # TODO: factory for published and unpublished
    past     = Ecto.DateTime.from_erl {{2010, 1, 1}, {1, 1, 1}}
    future   = Ecto.DateTime.from_erl {{2030, 1, 1}, {1, 1, 1}}
    art1     = Factory.create(:article, published_at: past)
    _art2    = Factory.create(:article, published_at: future)
    entities = Article |> PublishableQuery.published |> Repo.all
    assert entities == [art1]
  end
end
