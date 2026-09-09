import gleam/list
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
  // First we check for a drive letter, such as `C:` or `d:`
  let first_two = string.slice(path, 0, length: 2)
  let is_drive = is_drive_prefix(first_two)
  let path = case is_drive {
    True -> string.remove_prefix(path, first_two)
    False -> path
  }

  case is_drive, path {
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
  let path = case remove_drive_prefix(path) {
    had_drive if path != had_drive -> had_drive
    _ -> remove_unc_prefix(path)
  }

  path
  |> splitter.split_all(splitter.new(["/", "\\"]), _)
  |> list.fold(Error(Nil), fn(found, segment) {
    case segment {
      "" -> found
      "." -> found
      ".." -> Error(Nil)
      _ -> Ok(segment)
    }
  })
}

fn remove_unc_prefix(path: String) -> String {
  case path {
    "//" <> path | "\\\\" <> path | "/\\" <> path | "\\/" <> path -> {
      let slashes = splitter.new(["/", "\\"])
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

fn remove_drive_prefix(path: String) -> String {
  let first_two = string.slice(path, 0, length: 2)
  let is_drive = is_drive_prefix(first_two)
  case is_drive {
    True -> string.remove_prefix(path, first_two)
    False -> path
  }
}
