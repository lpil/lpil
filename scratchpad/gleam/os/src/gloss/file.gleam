import gleam/list
import gleam/option.{None}
import gleam/result
import gloss/os
import gloss/path

pub opaque type Capability {
  Unrestricted
}

// TODO: document
pub const unrestricted: Capability = Unrestricted

// TODO: document
pub type FileError {
  OperationFailed(code: os.PosixError, operation: FileOperation, path: String)
  ContentsWereNotUtf8(path: String)
}

// TODO: document
pub type FileOperation {
  Copy(to: String)
  Delete
  Link(to: String)
  Move(to: String)
  Open
  Read
  Write
}

// TODO: document
// TODO: document symlink limit
// TODO: test
// TODO: implement
pub fn canonicalise(path: String) -> Result(String, FileError) {
  let symlink_limit = 32
  let parts = path.parts(path)
  // TODO: handle prefix
  // TODO: correct root handling for windows
  // TODO: what about drive relative? need to get cwd for windows
  use start <- result.try(case parts.rooted {
    True -> Ok("/")
    False -> current_working_directory()
  })
  canonicalise_components(parts.components, start, symlink_limit)
}

fn canonicalise_components(
  components: List(String),
  path: String,
  symlink_limit: Int,
) -> Result(String, FileError) {
  case components {
    [] -> Ok(path)

    // Follow ../ components up to the parent.
    // Error if already at the root.
    ["..", ..components] -> {
      case path.parent(path) {
        Ok(path) -> canonicalise_components(components, path, symlink_limit)
        Error(_) -> {
          let path = path.join(path, "..")
          Error(OperationFailed(code: os.Enoent, operation: Read, path: path))
        }
      }
    }

    [next, ..components] -> {
      let next = path.join(path, next)
      case unrestricted_read_link(next) {
        // It was not a symlink, continue.
        Error(os.Einval) ->
          canonicalise_components(components, next, symlink_limit)

        // It was a symlink. Follow it and continue.
        Ok(target) if symlink_limit > 0 -> {
          // Spend 1 permitted symlink following
          let symlink_limit = symlink_limit - 1
          let components = case path.parts(target) {
            // The target is relative to the current directory, add its
            // components to the remaining components
            path.Parts(prefix: None, rooted: False, components: new) ->
              list.append(new, components)

            // TODO: handle all 3 other kinds of path
            path.Parts(..) -> todo as "one of the 3 other kinds of path"
          }
          canonicalise_components(components, path, symlink_limit)
        }

        // The limit for number of symlinks was reached. Presumably this is a
        // symlink loop, error out.
        Ok(_) ->
          Error(OperationFailed(code: os.Eloop, operation: Read, path: next))

        Error(code) ->
          Error(OperationFailed(code:, operation: Read, path: next))
      }
    }
  }
}

// TODO: document
pub fn current_working_directory() -> Result(String, FileError) {
  case get_cwd() {
    Ok(path) -> Ok(charlist_to_string(path))
    Error(code) -> Error(OperationFailed(code:, operation: Read, path: "."))
  }
}

@external(erlang, "prim_file", "get_cwd")
fn get_cwd() -> Result(Charlist, os.PosixError)

// TODO: document
// TODO: test
// TODO: implement
pub fn change_working_directory(
  path: String,
  capability: Capability,
) -> Result(Nil, FileError) {
  use <- check_capability(capability, path, Read)
  unrestricted_cd(path)
}

// TODO: document
// TODO: test
pub fn read_bits(
  path: String,
  capability: Capability,
) -> Result(BitArray, FileError) {
  use <- check_capability(capability, path, Read)
  unrestricted_read_bits(path)
}

// TODO: write bit needs to ensure the bits are byte aligned

// TODO: document
// TODO: test
pub fn write(
  path: String,
  capability: Capability,
  contents: String,
) -> Result(Nil, FileError) {
  use <- check_capability(capability, path, Write)
  unrestricted_write(path, contents)
}

// TODO: document
// TODO: test
// TODO: implement in Gleam rather than Erlang to avoid using the file server
pub fn delete(path: String, capability: Capability) -> Result(Nil, FileError) {
  use <- check_capability(capability, path, Delete)
  unrestricted_delete(path)
}

fn check_capability(
  capability: Capability,
  _path: String,
  _file_operation: FileOperation,
  continue: fn() -> Result(t, FileError),
) -> Result(t, FileError) {
  case capability {
    Unrestricted -> continue()
  }
}

@external(erlang, "gloss_ffi", "cd")
fn unrestricted_cd(path: String) -> Result(Nil, FileError)

@external(erlang, "gloss_ffi", "read")
fn unrestricted_read_bits(path: String) -> Result(BitArray, FileError)

@external(erlang, "gloss_ffi", "write")
fn unrestricted_write(path: String, contents: String) -> Result(Nil, FileError)

@external(erlang, "gloss_ffi", "delete")
fn unrestricted_delete(path: String) -> Result(Nil, FileError)

type Charlist

@external(erlang, "unicode", "characters_to_binary")
fn charlist_to_string(list: Charlist) -> String

@external(erlang, "unicode", "characters_to_list")
fn string_to_charlist(a: String) -> Charlist

fn unrestricted_read_link(path: String) -> Result(String, os.PosixError) {
  path
  |> string_to_charlist
  |> charlist_unrestricted_read_link
  |> result.map(charlist_to_string)
}

// `prim_file` is used directly to avoid going via the file server, since
// `file:read_link/1` does not have a `raw` option.
@external(erlang, "prim_file", "read_link")
fn charlist_unrestricted_read_link(
  path: Charlist,
) -> Result(Charlist, os.PosixError)
