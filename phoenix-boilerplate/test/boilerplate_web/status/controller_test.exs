defmodule BoilerplateWeb.Status.ControllerTest do
  use BoilerplateWeb.ConnCase

  test "GET /", %{conn: conn} do
    conn = get(conn, "/v1/status")
    assert json_response(conn, 200) == %{"postgres_connected" => true}
  end
end
