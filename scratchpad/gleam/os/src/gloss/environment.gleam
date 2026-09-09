import gleam/dict.{type Dict}
import gleam/result
import gloss/path

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

/// Get the location of the system temporary file directory.
///
/// Note: This is a directory where temporary files are expected to be written.
/// This function does not create a new directory, or later delete any files
/// created within it. The operating system may delete files from this
/// directory at some point, but it is not guarenteed.
///
/// On Windows this will use the `TEMP` and `TMP`, and `SystemRoot` (+ `/Temp`)
/// environment variables Windows, and the `TMPDIR` environment variable on
/// other operating systems.
///
pub fn temporary_directory() -> String {
  case system_name() {
    "win32" ->
      get("TEMP")
      |> result.lazy_or(fn() { get("TMP") })
      |> result.lazy_unwrap(fn() {
        get("TMP") |> result.unwrap("C:\\Windows") |> path.join("Temp")
      })
    _ ->
      get("TMPDIR")
      |> result.unwrap("/tmp")
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
