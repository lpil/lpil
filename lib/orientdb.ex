defmodule Orientdb do
  @moduledoc """
  Wrapper for the OrientDB HTTP REST API.
  """

  @doc """
  Ping the database.

      iex> OrientDb.ping()
      :ok
  """
  def ping do
    match?({:ok, %{"result" => [%{"1" => 1}]}}, command("SELECT 1"))
  end

  @doc """
  Run an SQL command against the database.
  """
  def command(sql, params \\ %{})

  def command(sql, params) when is_list(params) do
    command(sql, Enum.into(params, %{}))
  end

  def command(sql, params) do
    body = Poison.encode!(%{command: sql, parameters: params})
    creds = Application.get_env(:particle, :orientdb) |> Enum.into(%{})

    headers = [
      {"Authorization", "Basic " <> Base.encode64(creds.username <> ":" <> creds.password)}
    ]

    resp =
      HTTPoison.post!(
        "http://#{creds.host}:#{creds.port}/command/#{creds.db_name}/sql",
        body,
        headers
      )

    case {resp.status_code, resp.body} do
      {200, body} ->
        {:ok, Poison.decode!(body)}

      {204, _} ->
        {:ok, nil}

      {500, body} ->
        body
        |> Poison.decode!()
        |> Map.fetch!("errors")
        |> Enum.map(&parse_error/1)
        |> Enum.filter(& &1)
        |> Term.tag(:invalid)
    end
  catch
    {:orient_error, error} ->
      error
  end

  defp parse_error(error) do
    e = error["content"]

    cond do
      caps = captures(~r/The field '.+\.(?<field>.+)' cannot be null/, e) ->
        {:error, field(caps), :presence, "must be present"}

      caps = captures(~r/The field '.+\.(?<field>.+)' does not match the regular exp/, e) ->
        {:error, field(caps), :format, "must have the correct format"}

      caps = captures(~r/found duplicated key '.+' in index '.+_(?<field>.+)'/, e) ->
        {:error, field(caps), :uniqueness, "has already been taken"}

      Regex.match?(~r/Index with name (?<name>\w+) already exists/, e) ->
        e |> Term.tag(:duplicate_index) |> Term.tag(:orient_error) |> throw()

      true ->
        e |> Term.tag(:unknown_orient_error) |> throw()
    end
  end

  defp field(%{"field" => name}) do
    String.to_existing_atom(name)
  end

  defp captures(r, str) do
    Regex.named_captures(r, str)
  end
end
