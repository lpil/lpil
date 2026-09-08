import gleam/string

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
        Absolute | DriveRelative | RootRelative -> ".\\" <> right
        Relative -> right
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
