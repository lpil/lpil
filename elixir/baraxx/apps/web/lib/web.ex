defmodule Web do
  use Raxx.Server
  require Logger

  @impl Raxx.Server
  def handle_request(request, config) do
    start = System.monotonic_time()
    Logger.info([to_string(request.method), ?\s, Enum.intersperse(request.path, ?\/)])

    response =
      try do
        Web.Router.handle_request(request, config)
      rescue
        e ->
          error_response(e)
      catch
        e ->
          error_response(e)
      end

    stop = System.monotonic_time()
    diff = System.convert_time_unit(stop - start, :native, :micro_seconds)
    Logger.info(["Sent", ?\s, Integer.to_string(response.status), " in ", formatted_diff(diff)])

    response
  end

  defp formatted_diff(diff) when diff > 1000 do
    [diff |> div(1000) |> Integer.to_string(), "ms"]
  end

  defp formatted_diff(diff) do
    [Integer.to_string(diff), "µs"]
  end

  defp error_response(error) do
    error
    |> inspect()
    |> Logger.error()

    response(:internal_server_error)
    |> set_header("content-type", "text/plain")
    |> set_body("Internal server error")
  end
end
