defmodule Fawkes.Factory do
  @moduledoc """
  Responsible for creation of data for our tests.
  """
  use ExMachina.Ecto, repo: Fawkes.Repo

  def factory(:user) do
    password = "super-secret"
    %Fawkes.User{
      username: sequence(:username, &"username-#{&1}"),
      email:    sequence(:email, &"email-#{&1}@example.uk"),
      password: password,
      password_confirmation: password,
      password_hash: Comeonin.Bcrypt.hashpwsalt(password),
    }
  end

  def factory(:article) do
    %Fawkes.Article{
      title: sequence(:title, &"Amazing Blog Post #{&1}!"),
      slug:  sequence(:slug, &"slug-#{&1}"),
      published_at: Ecto.DateTime.utc,
      body: sequence(:body, &"""
      <p>This is our super amazing blog post number #{&1}.</p>
      <h1>Wow.</h1>
      """),
    }
  end
end
