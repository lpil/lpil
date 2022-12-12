object Hamming {
  def distance(xs: String, ys: String): Option[Int] =
    if (xs.length == ys.length)
      Some(xs.zip(ys).count { case (x, y) => x != y })
    else
      None
}
