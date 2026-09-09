import gleam/list
import gleam/option
import gleam/string
import splitter

@external(erlang, "gloss_ffi", "is_windows")
@external(javascript, "./gloss_ffi.mjs", "is_windows")
fn is_windows() -> Bool

pub type PathKind {
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

pub fn kind(path: String) -> PathKind {
  case is_windows() {
    True -> kind_windows(path)
    _ -> kind_unix(path)
  }
}

pub fn is_relative(path: String) -> Bool {
  kind(path) != Absolute
}

pub fn is_absolute(path: String) -> Bool {
  kind(path) == Absolute
}

@internal
pub fn kind_unix(path: String) -> PathKind {
  case path {
    "/" <> _ -> Absolute
    _ -> Relative
  }
}

@internal
pub fn kind_windows(path: String) -> PathKind {
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
  let path = case split_drive_prefix(path) {
    #("", path) -> remove_unc_prefix(path)
    #(_, path) -> path
  }

  path
  |> splitter.split_all(windows_splitter(), _)
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

fn remove_unc_prefix(path: String) -> String {
  case path {
    "//" <> path | "\\\\" <> path | "/\\" <> path | "\\/" <> path -> {
      let slashes = windows_splitter()
      case splitter.split(slashes, path) {
        // Server was empty
        #("", _, _) -> ""
        #(_, _, path) ->
          case splitter.split(slashes, path) {
            // Share was empty
            #("", _, _) -> ""
            #(_, _, path) -> path
          }
      }
    }
    path -> path
  }
}

fn split_drive_prefix(path: String) -> #(String, String) {
  let first_two = string.slice(path, 0, length: 2)
  let is_drive = is_drive_prefix(first_two)
  case is_drive {
    True -> #(first_two, string.remove_prefix(path, first_two))
    False -> #("", path)
  }
}

pub type Parts {
  Parts(prefix: option.Option(String), rooted: Bool, components: List(String))
}

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

@internal
pub fn parent_windows(path: String) -> Result(String, Nil) {
  todo
}
