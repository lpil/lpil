import gloss/file
import gloss/path

pub fn current_working_directory_test() {
  let assert Ok(path) = file.current_working_directory()
  assert path.is_absolute(path)
}
