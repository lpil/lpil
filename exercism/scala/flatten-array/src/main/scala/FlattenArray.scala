object FlattenArray {
  def flatten(list: List[Any]): List[Any] = list match {
    case Nil                  => Nil
    case null :: xs           => flatten(xs)
    case (x: List[Any]) :: xs => flatten(x) ::: flatten(xs)
    case x :: xs              => x :: flatten(xs)
  }
}
