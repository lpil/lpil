import gleam/result
import simplifile
import snag
import verse.{type Verse}

pub type Builder {
  Builder(path: String)
}

pub type Directory {
  Directory(path: String)
}

pub fn at(path: String) {
  Builder(path:)
}

pub fn add(builder: Builder) -> Verse(Directory) {
  use <- verse.start("directory " <> builder.path)
  let directory = Directory(path: builder.path)
  use is_directory <- verse.try(
    simplifile.is_directory(builder.path)
    |> result.map_error(fn(_) { snag.new("todo") }),
  )
  use is_file <- verse.try(
    simplifile.is_file(builder.path)
    |> result.map_error(fn(_) { snag.new("todo") }),
  )

  case is_directory {
    True -> verse.finish(directory)
    False if is_file -> panic as "already exists as file"
    False -> {
      use _ <- verse.try(
        simplifile.create_directory_all(builder.path)
        |> result.map_error(fn(_) { snag.new("todo") }),
      )
      use <- verse.changed
      verse.finish(directory)
    }
  }
}
