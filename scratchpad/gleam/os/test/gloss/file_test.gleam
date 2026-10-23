import gleam/option
import gloss/file
import gloss/os
import gloss/path

pub fn current_working_directory_test() {
  let assert Ok(path) = file.current_working_directory()
  assert path.is_absolute(path)
}

pub fn canonicalise_test() {
  let assert Ok(path) = file.canonicalise("src")
  assert path.is_absolute(path)
}

pub fn canonicalise_does_not_exist_test() {
  let assert Ok(cwd) = file.current_working_directory()
  let assert Error(file.OperationFailed(code:, operation:, path:)) =
    file.canonicalise("does-not-exist/wibble")
  assert path == path.join(cwd, "does-not-exist")
  assert code == os.Enoent
  assert operation == file.Read
}

pub fn canonicalise_symlink_loop_test() {
  let assert Ok(cwd) = file.current_working_directory()
  assert file.canonicalise("fixtures/symlink-loop/a")
    == Error(file.OperationFailed(
      code: os.Eloop,
      operation: file.Read,
      path: path.join(cwd, "fixtures/symlink-loop/b"),
    ))
}

pub fn canonicalise_symlink_ascend_beyond_root_test() {
  let assert Ok(cwd) = file.current_working_directory()
  assert file.canonicalise("fixtures/symlink-ascend-beyond-root/link")
    == Error(file.OperationFailed(
      code: os.Enoent,
      operation: file.Read,
      path: option.unwrap(path.parts(cwd).prefix, "") <> "/..",
    ))
}

pub fn canonicalise_symlink_chain_test() {
  let assert Ok(cwd) = file.current_working_directory()
  assert file.canonicalise("fixtures/symlink-chain/00")
    == Error(file.OperationFailed(
      code: os.Eloop,
      operation: file.Read,
      path: path.join(cwd, "fixtures/symlink-chain/32"),
    ))
}

pub fn canonicalise_symlink_dot_dot_test() {
  let assert Ok(cwd) = file.current_working_directory()
  assert file.canonicalise("fixtures/symlink-dot-dot/link")
    == Ok(path.join(cwd, "fixtures/symlink-dot-dot/a/c/d"))
}

pub fn canonicalise_symlink_dot_test() {
  let assert Ok(cwd) = file.current_working_directory()
  assert file.canonicalise("fixtures/symlink-dot/a")
    == Ok(path.join(cwd, "fixtures/symlink-dot/b"))
}

pub fn read_ok_test() {
  assert file.read_bits("fixtures/the-movie/hello-joe.txt", file.unrestricted)
    == Ok(<<"Hello Mike!\n">>)
}
