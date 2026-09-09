import gleam/dict.{type Dict}

/// Get the name of the operating system.
///
/// Possible values include:
///
/// - `darwin`
/// - `freebsd`
/// - `linux`
/// - `openbsd`
/// - `unknown`
/// - `win32`
///
@external(erlang, "gloss_ffi", "system_name")
@external(javascript, "../gloss_ffi.mjs", "system_name")
pub fn system_name() -> String

/// Get the location of the current system user's home directory.
///
/// On Windows this will use the `USERPROFILE` and `HOME` environment variables
/// Windows, and the `HOME` environment variable on other operating systems.
///
pub fn home_directory() -> Result(String, Nil) {
  case system_name() {
    "win32" ->
      case get("USERPROFILE") {
        Error(_) -> get("HOME")
        path -> path
      }
    _ -> get("HOME")
  }
}

/// Get an environment variable by name.
///
/// ```gleam
/// environment.get("HOME")
/// // -> Ok("/home/lucy")
/// ```
///
/// ```gleam
/// environment.get("WORLD_PEACE")
/// // -> Error(Nil)
/// ```
///
@external(erlang, "gloss_ffi", "environment_get")
@external(javascript, "../gloss_ffi.mjs", "environment_get")
pub fn get(name: String) -> Result(String, Nil)

//
/// Set an environment variable.
///
/// ```gleam
/// environment.get("FAVOURITE_COLOUR")
/// // -> Error(Nil)
/// environment.set("FAVOURITE_COLOUR", "Pink")
///
/// environment.get("FAVOURITE_COLOUR")
/// // -> Ok("Pink")
/// ```
///
@external(erlang, "gloss_ffi", "environment_set")
@external(javascript, "../gloss_ffi.mjs", "environment_set")
pub fn set(name: String, value: String) -> Nil

/// Unset an environment variable.
///
/// ```gleam
/// environment.set("FAVOURITE_COLOUR", "Pink")
///
/// environment.get("FAVOURITE_COLOUR")
/// // -> Ok("Pink")
///
/// environment.unset("FAVOURITE_COLOUR")
///
/// environment.get("FAVOURITE_COLOUR")
/// // -> Error(Nil)
/// ```
///
@external(erlang, "gloss_ffi", "environment_unset")
@external(javascript, "../gloss_ffi.mjs", "environment_unset")
pub fn unset(name: String) -> Nil

/// Get all the environment variables.
///
@external(erlang, "gloss_ffi", "environment_all")
@external(javascript, "../gloss_ffi.mjs", "environment_all")
pub fn all() -> Dict(String, String)
