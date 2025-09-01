import storail

pub type Error {
  StorageError(storail.StorailError)
}
