import gleam/result
import log/error.{type Error, StorageError}
import storail

pub fn write(key: storail.Key(t), object: t) -> Result(Nil, Error) {
  storail.write(key, object)
  |> result.map_error(StorageError)
}

pub fn read(key: storail.Key(t)) -> Result(t, Error) {
  storail.read(key)
  |> result.map_error(StorageError)
}

pub fn list(
  collection: storail.Collection(t),
  namespace: List(String),
) -> Result(List(String), Error) {
  storail.list(collection, namespace)
  |> result.map_error(StorageError)
}
