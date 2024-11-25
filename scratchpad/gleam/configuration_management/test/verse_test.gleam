import verse
import verse/directory

pub fn main() {
  verse.run({
    use <- verse.start("My thing")
    use _ <- verse.include(directory.at("tmp/a") |> directory.add)
    use _ <- verse.include(directory.at("tmp/b") |> directory.add)
    use _ <- verse.include(directory.at("tmp/c") |> directory.add)
    verse.finish(Nil)
  })
}
