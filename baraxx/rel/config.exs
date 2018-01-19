# Import all plugins from `rel/plugins`
# They can then be used by adding `plugin MyPlugin` to
# either an environment, or release definition, where
# `MyPlugin` is the name of the plugin module.
Path.join(["rel", "plugins", "*.exs"])
|> Path.wildcard()
|> Enum.map(&Code.eval_file(&1))

use Mix.Releases.Config,
  # This sets the default release built by `mix release`
  default_release: :default,
  # This sets the default environment used by `mix release`
  default_environment: Mix.env()

environment :prod do
  set(include_erts: true)
  set(include_src: false)
  set(cookie: :"4._JVuXd(1{(=av(E{73D^T_%U=H8BsLg@Nk_}^>k)YsCt;4&Sz^tsx)ko6x|^VP")
end

# You may define one or more releases in this file.
# If you have not set a default release, or selected one
# when running `mix release`, the first release in the file
# will be used by default

release :baraxx do
  set(version: current_version(:core))

  baraxx_apps =
    Path.wildcard("apps/*")
    |> Enum.map(&Path.basename/1)
    |> Enum.map(&String.to_atom/1)

  set(applications: [:runtime_tools | baraxx_apps])
end
