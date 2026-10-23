import gloss/os

pub opaque type Capability {
  Unrestricted
  Restricted(root: String)
}

pub type FileError {
  FileOperationFailed(
    code: os.PosixError,
    operation: FileOperation,
    path: String,
  )
  FileContentsWereNotUtf8(path: String)
  FilePathEscapedBoundary(path: String)
}

pub type FileOperation {
  Copy(to: String)
  Delete
  Link(to: String)
  Move(to: String)
  Open
  Read
  Write
}

pub fn current_working_directory() -> Result(String, FileError) {
  case get_cwd() {
    Ok(path) -> Ok(charlist_to_string(path))
    Error(code) -> Error(FileOperationFailed(code:, operation: Read, path: "."))
  }
}

type Charlist

@external(erlang, "unicode", "characters_to_binary")
fn charlist_to_string(list: Charlist) -> String

@external(erlang, "file", "get_cwd")
fn get_cwd() -> Result(Charlist, os.PosixError)
