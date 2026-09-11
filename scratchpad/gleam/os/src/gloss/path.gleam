import gleam/list
import gleam/option
import gleam/string
import splitter

@external(erlang, "gloss_ffi", "is_windows")
@external(javascript, "../gloss_ffi.mjs", "is_windows")
fn is_windows() -> Bool

/// There are four different kinds of path.
///
pub type Kind {
  /// Absolute paths refer to a specific file.
  ///
  /// - On Unix: `/usr/local/bin/gleam`
  /// - On Windows: `C:\usr\local\bin\gleam`
  /// - On Windows: `C:/usr/local/bin/gleam`
  /// - On Windows: `\\server\share\file`
  ///
  Absolute
  /// Relative paths refer to a file in relation to the current working
  /// directory.
  ///
  /// - On any: `gleam.toml`
  /// - On any: `./src`
  /// - On any: `../../elsewhere/secrets`
  ///
  Relative
  /// Drive relative paths refer to a file relative to the current directory
  /// on a specific drive.
  ///
  /// These paths only exist on Windows.
  ///
  /// - On Windows: `D:some/file`
  ///
  DriveRelative
  /// Root relative paths refer to a file relative to the current drive.
  ///
  /// These paths only exist on Windows.
  ///
  /// - On Windows: `/usr/local/bin/gleam`
  /// - On Windows: `\usr\local\bin\gleam`
  ///
  RootRelative
}

/// Returns which kind a given path is.
///
/// ```gleam
/// assert path.kind("/usr/bin/gleam") == path.Absolute
/// ```
///
/// ```gleam
/// assert path.kind("gleam.toml") == path.Relative
/// ```
///
/// See the `Kind` type for information on the different kinds.
///
pub fn kind(path: String) -> Kind {
  case is_windows() {
    True -> kind_windows(path)
    _ -> kind_unix(path)
  }
}

/// Returns whether a path is relative or not.
///
/// On Windows drive-relative and root-relative paths are considered relative
/// by this function. Use the `kind` function if you want some other behaviour.
///
pub fn is_relative(path: String) -> Bool {
  kind(path) != Absolute
}

/// Returns whether a path is absolute or not.
///
/// On Windows drive-relative and root-relative paths are not considered
/// absolute by this function. Use the `kind` function if you want some other
/// behaviour.
///
pub fn is_absolute(path: String) -> Bool {
  kind(path) == Absolute
}

@internal
pub fn kind_unix(path: String) -> Kind {
  case path {
    "/" <> _ -> Absolute
    _ -> Relative
  }
}

@internal
pub fn kind_windows(path: String) -> Kind {
  let #(drive_prefix, path) = split_drive_prefix(path)
  let had_drive = drive_prefix != ""

  case had_drive, path {
    // UNC paths: \\server\share\file
    False, "\\\\" <> _ -> Absolute
    False, "\\/" <> _ -> Absolute
    False, "/\\" <> _ -> Absolute
    False, "//" <> _ -> Absolute

    // C:/one/two
    True, "/" <> _ -> Absolute

    // C:\one\two
    True, "\\" <> _ -> Absolute

    // /one/two
    False, "/" <> _ -> RootRelative

    // \one\two
    False, "\\" <> _ -> RootRelative

    True, _ -> DriveRelative

    False, _ -> Relative
  }
}

fn is_drive_prefix(part: String) -> Bool {
  case <<part:utf8>> {
    // Drive letters are expected to be A-Z or a-z
    <<drive, ":">>
      if { drive >= 65 && drive <= 90 } || { drive >= 97 && drive <= 122 }
    -> True

    _ -> False
  }
}

/// Join new components onto a path.
///
/// The seperator of the current operating system is used, so `\` on Windows
/// and `/` on others.
///
/// ```gleam
/// assert path.join("/usr/bin", "gleam") == "/usr/bin/gleam"
/// ```
///
/// The kind of the path returned by this function will always be the same as
/// the kind of the first function.
///
/// ```gleam
/// assert path.join("/tmp", "/etc/passwd") == "/tmp/etc/passwd"
/// ```
///
/// `..` components are preserved.
///
/// ```gleam
/// assert path.join("red/..", "green/../blue") == "red/../green/../blue"
/// ```
///
pub fn join(left: String, right: String) -> String {
  case is_windows() {
    True -> join_windows(left, right)
    _ -> join_unix(left, right)
  }
}

@internal
pub fn join_unix(left: String, right: String) -> String {
  case left, right {
    _, "/" <> right -> join_unix(left, right)
    "", _ -> right
    _, "" -> left
    _, _ -> {
      case string.ends_with(left, "/") {
        True -> left <> right
        False -> left <> "/" <> right
      }
    }
  }
}

@internal
pub fn join_windows(left: String, right: String) -> String {
  case left, right {
    _, "/" <> right -> join_windows(left, right)
    _, "\\" <> right -> join_windows(left, right)
    "", _ ->
      case kind_windows(right) {
        Relative -> right
        Absolute | DriveRelative | RootRelative -> ".\\" <> right
      }
    _, "" -> left
    _, _ ->
      case
        string.ends_with(left, "\\")
        || string.ends_with(left, "/")
        || is_drive_prefix(left)
      {
        True -> left <> right
        False -> left <> "\\" <> right
      }
  }
}

/// Get last component of the path.
///
/// ```gleam
/// assert path.file_name("/var/log/snapper.log") == Ok("snapper.log")
/// ```
///
/// An error is returned if the path has no components.
///
/// ```gleam
/// assert path.file_name("/") == Error(Nil)
/// ```
///
pub fn file_name(path: String) -> Result(String, Nil) {
  case is_windows() {
    True -> file_name_windows(path)
    _ -> file_name_unix(path)
  }
}

@internal
pub fn file_name_unix(path: String) -> Result(String, Nil) {
  path
  |> string.split("/")
  |> list.fold(Error(Nil), fn(found, segment) {
    case segment {
      "" -> found
      "." -> found
      ".." -> Error(Nil)
      _ -> Ok(segment)
    }
  })
}

@internal
pub fn file_name_windows(path: String) -> Result(String, Nil) {
  let slashes = windows_splitter()
  let #(_, path) = split_prefix(path, slashes)
  path
  |> splitter.split_all(slashes, _)
  |> list.fold(Error(Nil), fn(found, segment) {
    case segment {
      "" -> found
      "." -> found
      ".." -> Error(Nil)
      _ -> Ok(segment)
    }
  })
}

fn windows_splitter() -> splitter.Splitter {
  splitter.new(["/", "\\"])
}

fn split_drive_prefix(path: String) -> #(String, String) {
  let first_two = string.slice(path, 0, length: 2)
  let is_drive = is_drive_prefix(first_two)
  case is_drive {
    True -> #(first_two, string.remove_prefix(path, first_two))
    False -> #("", path)
  }
}

/// The parts that make up a path.
///
pub type Parts {
  Parts(
    /// A Windows drive prefix (`C:`) or a Windows UNC path prefix
    /// (`\\server\share`), if there was one.
    ///
    /// Paths on operating systems other than Windows never have a drive
    /// prefix.
    ///
    prefix: option.Option(String),
    /// Whether the path is absolute, including the root of the drive on
    /// Windows, or the root of the file system on other operating systems.
    ///
    rooted: Bool,
    /// Each of the directories and file names in the path.
    ///
    components: List(String),
  )
}

/// Parse a path into its parts.
///
/// On Windows the case of the prefix is preserved.
///
/// ```gleam
/// // On Unix
/// assert path.parse("/bin/sh") ==
///   path.Parts(
///     prefix: option.None,
///     rooted: True,
///     components: ["bin", "sh"],
///   )
/// ```
///
/// ```gleam
/// // On Windows
/// assert path.parse("C:src/app.gleam") ==
///   path.Parts(
///     prefix: option.Some("C:"),
///     rooted: False,
///     components: ["src", "app.gleam"],
///   )
/// ```
///
pub fn parts(path: String) -> Parts {
  case is_windows() {
    True -> parts_windows(path)
    _ -> parts_unix(path)
  }
}

@internal
pub fn parts_unix(path: String) -> Parts {
  let #(rooted, path) = case path {
    "/" <> path -> #(True, path)
    _ -> #(False, path)
  }
  let components =
    path
    |> string.split("/")
    |> list.filter(fn(component) { component != "" && component != "." })
  Parts(prefix: option.None, rooted:, components:)
}

@internal
pub fn parts_windows(path: String) -> Parts {
  // Drive prefix
  let #(drive_prefix, path) = split_drive_prefix(path)

  // UNC prefix
  let slashes = windows_splitter()
  let #(prefix, components) = case splitter.split_all(slashes, path) {
    components if drive_prefix != "" -> #(drive_prefix, components)
    ["", "", server, share, ..components] if server != "" && share != "" -> {
      let prefix = "\\\\" <> server <> "\\" <> share
      #(prefix, components)
    }
    ["", "", server] if server != "" -> {
      let prefix = "\\\\" <> server
      #(prefix, [])
    }
    components -> #(drive_prefix, components)
  }

  // Is rooted
  let rooted = case path {
    "/" <> _ -> True
    "\\" <> _ -> True
    _ -> False
  }

  let prefix = case prefix {
    "" -> option.None
    _ -> option.Some(prefix)
  }
  let components =
    list.filter(components, fn(component) {
      component != "" && component != "."
    })
  Parts(prefix:, rooted:, components:)
}

/// Get the parent of the file that a path is for.
///
/// ```gleam
/// assert path.parent("src/gleam/list.gleam") == Ok("src/gleam")
/// ```
///
/// An error is returned if there is no parent in the path.
///
/// ```gleam
/// assert path.parent("") == Error(Nil)
/// assert path.parent("/") == Error(Nil)
/// ```
///
pub fn parent(path: String) -> Result(String, Nil) {
  case is_windows() {
    True -> parent_windows(path)
    _ -> parent_unix(path)
  }
}

@internal
pub fn parent_unix(path: String) -> Result(String, Nil) {
  let path = remove_trailing_unix(path)
  case path |> string.split("/") |> list.last {
    Ok("") | Error(_) -> Error(Nil)
    Ok(segment) ->
      path |> string.remove_suffix(segment) |> remove_trailing_unix |> Ok
  }
}

fn remove_trailing_unix(path: String) -> String {
  case path {
    "/." -> "/"
    "/" -> "/"
    "." -> ""
    _ ->
      case string.remove_suffix(path, "/.") {
        new if new != path -> remove_trailing_unix(new)
        _ ->
          case string.remove_suffix(path, "/") {
            new if new != path -> remove_trailing_unix(new)
            _ -> path
          }
      }
  }
}

fn split_prefix(path: String, slashes: splitter.Splitter) -> #(String, String) {
  let #(prefix, path) = split_drive_prefix(path)
  case prefix {
    "" -> split_unc_prefix(path, slashes)
    _ -> #(prefix, path)
  }
}

@internal
pub fn parent_windows(path: String) -> Result(String, Nil) {
  let slashes = windows_splitter()
  let #(prefix, path) = split_prefix(path, slashes)
  let path = remove_trailing_windows(path)
  let components = splitter.split_all(slashes, path)
  case components |> list.last {
    Ok("") | Error(_) -> Error(Nil)
    Ok(segment) -> {
      let path =
        path |> string.remove_suffix(segment) |> remove_trailing_windows
      Ok(prefix <> path)
    }
  }
}

fn split_unc_prefix(
  path: String,
  slashes: splitter.Splitter,
) -> #(String, String) {
  case path {
    // The prefix in `parts` is normalised if a UNC path
    "//" as prefix <> path
    | "\\\\" as prefix <> path
    | "/\\" as prefix <> path
    | "\\/" as prefix <> path -> {
      case splitter.split(slashes, path) {
        // Server was empty
        #("", _, _) -> #("", path)
        #(a, b, path) -> {
          let prefix = prefix <> a <> b
          case splitter.split(slashes, path) {
            // Share was empty
            #("", a, b) -> #(prefix, a <> b)
            #(a, b, path) -> #(prefix <> a <> b, path)
          }
        }
      }
    }
    _ -> #("", path)
  }
}

fn remove_trailing_windows(path: String) -> String {
  case path {
    "/." -> "/"
    "/" -> "/"
    "\\." -> "\\"
    "\\" -> "\\"
    "." -> ""
    _ ->
      case string.remove_suffix(path, "/.") {
        new if new != path -> remove_trailing_windows(new)
        _ ->
          case string.remove_suffix(path, "/") {
            new if new != path -> remove_trailing_windows(new)
            _ ->
              case string.remove_suffix(path, "\\.") {
                new if new != path -> remove_trailing_windows(new)
                _ ->
                  case string.remove_suffix(path, "\\") {
                    new if new != path -> remove_trailing_windows(new)
                    _ -> path
                  }
              }
          }
      }
  }
}

/// Determine if one path starts with the other.
///
/// ```gleam
/// assert path.starts_with("/bin", "/bin/sh")
/// assert path.starts_with("test", "test/helper.gleam")
/// assert !path.starts_with("one", "two")
/// ```
///
/// Paths are only compared lexically and the file system is not accessed to
/// check if the files exist, or if any of them are symlinks. If you wish to
/// check if an actual file is within a directory you must canonicalise both
/// paths first.
///
/// Path components are always compared in a case sensitive manner, regardless
/// of whether the computer's file system is case sensitive or not.
///
/// ```gleam
/// assert !path.starts_with("/one", "/ONE/two")
/// ```
///
/// A path is considered to start with itself.
///
/// ```gleam
/// assert path.starts_with("/one", "/one")
/// ```
/// 
/// On Windows both `/` and `\` seperators are supported.
///
/// ```gleam
/// assert path.starts_with("one\two", "one/two/three")
/// ```
///
/// On Windows path prefixes are compared in a case insensitive manner.
///
/// ```gleam
/// assert path.starts_with("c:\", "C:\Windows")
/// ```
///
pub fn starts_with(parent: String, child: String) -> Bool {
  case is_windows() {
    True -> starts_with_windows(parent, child)
    _ -> starts_with_unix(parent, child)
  }
}

@internal
pub fn starts_with_unix(parent: String, child: String) -> Bool {
  let parent = parts_unix(parent)
  let child = parts_unix(child)
  starts_with_parts(parent, child)
}

@internal
pub fn starts_with_windows(parent: String, child: String) -> Bool {
  let parent = parts_windows(parent)
  let child = parts_windows(child)
  starts_with_parts(parent, child)
}

fn starts_with_parts(parent: Parts, child: Parts) -> Bool {
  let lower = fn(path: Parts) { option.map(path.prefix, string.lowercase) }
  let same_context =
    parent.rooted == child.rooted && lower(parent) == lower(child)
  case same_context {
    True -> starts_with_components(parent.components, child.components)
    False -> False
  }
}

fn starts_with_components(parent: List(String), child: List(String)) -> Bool {
  case parent, child {
    [], _ -> True
    _, [] -> False
    [p1, ..], [c1, ..] if p1 != c1 -> False
    [_, ..parent], [_, ..child] -> starts_with_components(parent, child)
  }
}
