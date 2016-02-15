defmodule Fawkes.ArticleTest do
  use Fawkes.ModelCase

  alias Fawkes.Article

  @attrs %{
    title: "Amazing Blog Post!",
    slug: "amazing-blog-post-0123456789",
    body: """
    <p>This is our super amazing blog post.</p>
    <h1>Wow.</h1>
    """
  }

  # changeset/2

  @tag :async
  test "changeset can be valid" do
    changeset = Article.changeset(%Article{}, @attrs)
    assert changeset.valid?
  end

  @tag :async
  test "slug must be present" do
    attrs = Dict.delete @attrs, :slug
    changeset = Article.changeset(%Article{}, attrs)
    refute changeset.valid?
    assert [slug: _] = changeset.errors
  end

  @tag :async
  test "slug must be url friendly" do
    attrs = %{ @attrs | slug: "What the??" }
    changeset = Article.changeset(%Article{}, attrs)
    refute changeset.valid?
    assert [slug: _] = changeset.errors
  end

  test "slugs must be unique" do
    assert {:ok, _} =
      %Article{}
      |> Article.changeset(@attrs)
      |> Repo.insert
    assert {:error, changeset} =
      %Article{}
      |> Article.changeset(@attrs)
      |> Repo.insert
    assert changeset.errors == [
      slug: "has already been taken",
    ]
  end

  @tag :async
  test "titles must be present" do
    attrs = Dict.delete @attrs, :title
    changeset = Article.changeset(%Article{}, attrs)
    refute changeset.valid?
    assert [title: _] = changeset.errors
  end

  @tag :async
  test "titles must be longish" do
    attrs = %{ @attrs | title: "Short" }
    changeset = Article.changeset(%Article{}, attrs)
    refute changeset.valid?
    assert [title: _] = changeset.errors
  end

  @tag :async
  test "body must be present" do
    attrs = Dict.delete @attrs, :body
    changeset = Article.changeset(%Article{}, attrs)
    refute changeset.valid?
    assert [body: _] = changeset.errors
  end

  @tag :async
  test "body must be longish" do
    body = """
    A very short blog post.
    """
    attrs = %{ @attrs | body: body }
    changeset = Article.changeset(%Article{}, attrs)
    refute changeset.valid?
    assert [body: _] = changeset.errors
  end

  @tag :skip
  test "body must be stripped of dangerous tags"
end
