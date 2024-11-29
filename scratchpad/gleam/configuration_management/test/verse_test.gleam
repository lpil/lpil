import verse
import verse/directory
import verse/file

pub fn main() {
  verse.run({
    use <- verse.start("My thing")
    use _ <- verse.include(directory.at("tmp/a") |> directory.create)
    use _ <- verse.include(directory.at("tmp/b") |> directory.create)
    use _ <- verse.include(directory.at("tmp/c") |> directory.create)
    use _ <- verse.include(file.at("tmp/c/one") |> file.create)
    use _ <- verse.include(file.at("tmp/c/two") |> file.create)
    use _ <- verse.include(file.at("tmp/c/two") |> file.delete)
    verse.finish(Nil)
  })
}
