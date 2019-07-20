use Mix.Config

# Configuration of Swoosh, which is the library used to send email.

case Mix.env() do
  :test ->
    # Store emails in memory in test so we can make assertions about them.
    config :boilerplate, BoilerplateWeb.Mailer, adapter: Swoosh.Adapters.Test

  :dev ->
    # Store emails in memory in test so we can preview them in the browser.
    config :boilerplate, BoilerplateWeb.Mailer, adapter: Swoosh.Adapters.Local

  :prod ->
    nil
end
