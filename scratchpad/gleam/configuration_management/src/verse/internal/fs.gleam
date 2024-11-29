import gleam/option.{type Option}
import gleam/result
import gleam/string
import simplifile
import snag.{type Snag}

fn optional_file_info(path: String) -> Result(Option(simplifile.FileInfo), Snag) {
  case simplifile.file_info(path) {
    Error(simplifile.Enoent) -> Ok(option.None)
    Error(e) -> panic as string.inspect(e)
    Ok(info) -> Ok(option.Some(info))
  }
}

fn file_info(path: String) -> Result(simplifile.FileInfo, Snag) {
  case simplifile.file_info(path) {
    Error(e) -> panic as string.inspect(e)
    Ok(info) -> Ok(info)
  }
}

fn create_directory(path: String) -> Result(Nil, Snag) {
  simplifile.create_directory_all(path)
  |> result.map_error(fn(_) { panic as "unable to make directory" })
}

fn create_file(path: String) -> Result(Nil, Snag) {
  simplifile.create_file(path)
  |> result.map_error(fn(_) { panic as "unable to make file" })
}

pub type FileTypeDesired {
  File
  Directory
}

pub fn ensure_directory(
  path: String,
) -> Result(#(simplifile.FileInfo, Bool), Snag) {
  use info <- result.try(optional_file_info(path))
  case info {
    option.None -> {
      use _ <- result.try(create_directory(path))
      use info <- result.try(file_info(path))
      Ok(#(info, True))
    }
    option.Some(info) ->
      case simplifile.file_info_type(info) {
        simplifile.Directory -> Ok(#(info, False))
        simplifile.File -> panic as "already exists as file"
        simplifile.Other -> panic as "already exists as other"
        simplifile.Symlink -> panic as "already exists as symlink"
      }
  }
}

pub fn ensure_file(path: String) -> Result(#(simplifile.FileInfo, Bool), Snag) {
  use info <- result.try(optional_file_info(path))
  case info {
    option.None -> {
      use _ <- result.try(create_file(path))
      use info <- result.try(file_info(path))
      Ok(#(info, True))
    }
    option.Some(info) ->
      case simplifile.file_info_type(info) {
        simplifile.File -> Ok(#(info, False))
        simplifile.Directory -> panic as "already exists as file"
        simplifile.Other -> panic as "already exists as other"
        simplifile.Symlink -> panic as "already exists as symlink"
      }
  }
}

pub fn delete(path: String) -> Result(Bool, Snag) {
  case simplifile.delete(path) {
    Ok(_) -> Ok(True)
    Error(simplifile.Enoent) -> Ok(False)
    Error(_) -> panic as "unable to delete"
  }
}
