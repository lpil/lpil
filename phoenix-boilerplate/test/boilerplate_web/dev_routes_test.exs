defmodule DevRoutesTest do
  use BoilerplateWeb.ConnCase

  # We've got some magic dev routes that we don't want to exist
  # outside of dev.

  test "/dev/mailbox", ctx do
    assert_raise Phoenix.Router.NoRouteError, fn ->
      get(ctx.conn, "/dev/mailbox")
    end
  end
end
