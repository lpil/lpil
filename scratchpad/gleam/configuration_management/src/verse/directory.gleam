import verse.{type Verse}
import verse/internal/fs

pub type Builder {
  Builder(path: String)
}

pub type Directory {
  Directory(path: String)
}

pub fn at(path: String) {
  Builder(path:)
}

pub fn create(builder: Builder) -> Verse(Directory) {
  use <- verse.start("directory " <> builder.path)
  let directory = Directory(path: builder.path)
  use #(_info, file_created) <- verse.try(fs.ensure_directory(builder.path))
  let changed = file_created
  use <- verse.conditionally_mark_as_changed(changed)
  verse.finish(directory)
}
