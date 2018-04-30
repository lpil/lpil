defmodule Orientdb do
  @moduledoc """
  Wrapper for the OrientDB HTTP REST API.
  """

  import ParticleWeb.Gettext, only: [gettext: 1]

  @doc """
  Ping the database.

      iex> Orientdb.ping()
      true
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
    creds = get_creds()

    "http://#{creds.host}:#{creds.port}/command/#{creds.db_name}/sql"
    |> HTTPoison.post!(body, headers(creds))
    |> handle_response()
  end

  @doc """
  Run a series of commands in one request.
  If a single SQL string is given it is split on `;`.

  ## Options

  - `:transaction` Whether to wrap the batch in a transaction. Defaults to false.
  """
  def batch(sql_commands, params \\ %{}, args \\ [])

  def batch(sql_commands, params, args) when is_list(params) do
    batch(sql_commands, Enum.into(params, %{}), args)
  end

  def batch(sql_commands, params, args) when is_binary(sql_commands) do
    sql_commands
    |> String.split(";")
    |> batch(Enum.into(params, %{}), args)
  end

  def batch(sql_commands, params, args) do
    transaction = args[:transaction] || false

    body =
      Poison.encode!(%{
        transaction: transaction,
        operations: [
          %{
            type: "script",
            language: "sql",
            script: sql_commands,
            # TODO: Does this work? There's no mention of this in the docs.
            params: params
          }
        ]
      })

    creds = get_creds()

    "http://#{creds.host}:#{creds.port}/batch/#{creds.db_name}"
    |> HTTPoison.post!(body, headers(creds))
    |> handle_response()
  end

  defp handle_response(resp) do
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

  defp get_creds do
    Application.get_env(:particle, :orientdb) |> Enum.into(%{})
  end

  defp headers(creds) do
    [
      {"Authorization", "Basic " <> Base.encode64(creds.username <> ":" <> creds.password)}
    ]
  end

  defp parse_error(error) do
    e = error["content"]

    cond do
      caps = captures(~r/The field '.+\.(?<field>.+)' cannot be null/, e) ->
        {:error, field(caps), :presence, gettext("must be present")}

      caps = captures(~r/The field '.+\.(?<field>.+)' does not match the regular exp/, e) ->
        {:error, field(caps), :format, gettext("must have the correct format")}

      caps = captures(~r/found duplicated key '.+' in index '.+_(?<field>.+)'/, e) ->
        {:error, field(caps), :uniqueness, gettext("has already been taken")}

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
