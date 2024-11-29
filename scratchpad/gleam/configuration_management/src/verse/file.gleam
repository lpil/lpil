import verse.{type Verse}
import verse/internal/fs

pub type Builder {
  Builder(path: String)
}

pub type File {
  File(path: String)
}

pub fn at(path: String) {
  Builder(path:)
}

pub fn create(builder: Builder) -> Verse(File) {
  use <- verse.start("file " <> builder.path)
  let file = File(path: builder.path)
  use #(_info, file_created) <- verse.try(fs.ensure_file(builder.path))
  let changed = file_created
  use <- verse.conditionally_mark_as_changed(changed)
  verse.finish(file)
}

pub fn delete(builder: Builder) -> Verse(Nil) {
  use <- verse.start("file " <> builder.path)
  use deleted <- verse.try(fs.delete(builder.path))
  use <- verse.conditionally_mark_as_changed(deleted)
  verse.finish(Nil)
}
